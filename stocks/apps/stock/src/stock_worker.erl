%% @author Andrew Kondratovich <andrew.kondratovich@gmail.com>
%% @doc worker gen_server
%%
%%      We have one process for every stock.
%%      For large amount of stocks and uniform
%%      load all cores will be in use.
%%      This worker handles premerged ohlc data
%%      for minutes.In worst case for `month` scale
%%      for one month we need 60 * 24 * 30 (31) values.
%%      Also, selection is about 100 times faster than
%%      to_string self-encoding T_T

-module(stock_worker).
-author('Andrew Kondratovich <andrew.kondratovich@gmail.com>').

-behaviour(gen_server).
-export([start_link/1]).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%%% API
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%% we need to check if we have already data
%% for this time period
save(T, V) when is_tuple(V) ->
  %% Get (maybe) existing element
  Ex = ets:lookup(T, erlang:element(1, V)),
  %% merge it with new and put back
  ets:insert(T, stock_api:merge(Ex, V)).

%% ets:select/2 is very inefficient way to select
%% bounded range of values - it looks through
%% the whole table and is slow on big tables -
%% so I used next / lookup combo (see below).
select(T, From, To, F) ->
  %% we select first key in range, so
  %% we can be sure that lookup returns
  %% element and avoid additional check
  select0(T, ets:prev(T, To), From, [], F).

%% key iteration finished
select0(_, '$end_of_table', _, Acc0, _) -> Acc0;

%% we passed key limit
select0(_, Key, Limit, Acc0, _) when Key < Limit -> Acc0;

select0(T, Key, Limit, Acc0, F) ->
  %% we know that this key and value
  %% exists in table
  [V] = ets:lookup(T, Key),
  %% get next key and call recursively
  Key2 = ets:prev(T, Key),
  select0(T, Key2, Limit, F(V, Acc0), F).

%%% gen_server callbacks

init(Id) ->
  %% register new name in gproc
  true = gproc:reg({n, l, Id}, stock),
  %% create two ETS tables
  %% (ordered by time - 1st element in tuple)
  T0 = ets_manager:new(Id, [ordered_set, private]),
  {ok, T0}.

f_fold (V, Acc) -> [V | Acc].
f_merge(V, Acc) -> stock_api:merge(Acc, V).

handle_call({get, {From, To}}, _From, Table)
    when is_integer(From), is_integer(To) ->
  Reply = select(Table, From, To, fun f_fold/2),
  {reply, Reply, Table};

handle_call({merge, {From, To}}, _From, Table)
    when is_integer(From), is_integer(To) ->
  case select(Table, From, To, fun f_merge/2) of
    [] -> {reply, [], Table};
    Re -> {reply, erlang:setelement(1, Re, From), Table}
  end;

%% when we want to remove all data for this stock
%% we need to cancel `heir` option in table
handle_call(before_death, _From, Table) ->
  ets:setopts(Table, {heir, none}),
  {reply, ok, Table};

handle_call(_Request, _From, State) ->
  {noreply, State}.

%% Premerged - list of ohlc data for minute scale
handle_cast({put, Premerged}, Table) ->
  lists:map(fun(V) -> save(Table, V) end, Premerged),
  {noreply, Table};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
