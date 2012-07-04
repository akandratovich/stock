%% @author Andrew Kondratovich <andrew.kondratovich@gmail.com>
%% @doc ETS Manager. Used to save tables in case of process error.
%%
%% used from:
%% http://steve.vinoski.net/blog/2011/03/23/dont-lose-your-ets-tables/
%% http://hlabs.org/news/rescue_ets_data.html .

-module(ets_manager).

-behaviour(gen_server).
-export([start/0, new/2]).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(TIMEOUT, 1000).

%%% API
start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% gen_server callbacks

init([]) -> {ok, dict:new()}.

handle_call({get, Key}, {Process, Tag}, Tables) ->
  case dict:find(Key, Tables) of
    {ok, Table} ->
      true = ets:give_away(Table, Process, Tag), %% give away to calling process
      {reply, {ok, Tag}, dict:erase(Key, Tables)};
    error ->
      {reply, {error, not_found}, Tables}
  end;

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'ETS-TRANSFER', Table, _, Key}, Tables) -> % retrieve table from died process
  {noreply, dict:store(Key, Table, Tables)};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% like ets:new/2
new(Name, Options) when is_list(Name), is_list(Options) ->
  new(list_to_atom(Name), Options);

new(Name, Options) when is_atom(Name), is_list(Options) ->
  Key = {Name, Options},
  Server = whereis(?MODULE), %% find yourself
  case gen_server:call(Server, {get, Key}) of
    {ok, Tag} -> %% we have such table
      receive    %% and give it back
        {'ETS-TRANSFER', Table, Server, Tag} -> Table
      after
        ?TIMEOUT -> %% shit happens
          create({Name, Key}, Options, Server)
      end;
    {error, not_found} -> create({Name, Key}, Options, Server)
  end.

create({Name, Key}, Options, Server) ->
  O = case lists:keymember(heir, 1, Options) of
    true -> Options;  %% setup heir option if need
    false -> [{heir, Server, Key} | Options]
  end,
  ets:new(Name, O).
