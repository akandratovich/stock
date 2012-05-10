%% @author Andrew Kondratovich <andrew.kondratovich@gmail.com>
%% @doc General API for application

-module(stock_api).
-author('Andrew Kondratovich <andrew.kondratovich@gmail.com>').

-export([select/4, store/2, info/1, names/0, perf/0, erase/1, merge/2]).
-include("stock.hrl").

-spec perf() -> {Cpu, Mem, Time} | {'error', Reason} when
      Reason :: any(),
      Cpu    :: float(),
      Mem    :: number(),
      Time   :: number().
%% @doc request for current cpu/memory consumption
perf() ->
  case gproc:lookup_local_name(perf) of %% lookup worker
    undefined ->
      case stock_perf:start_link([]) of %% start new
        {ok, Worker} -> gen_server:call(Worker, perf);
        _ -> {error, "Cannot create worker"} %% woops
      end;
    Worker ->
      gen_server:call(Worker, perf) %% retrieve data
  end.

-spec erase(Name) -> Result when
      Result :: 'ok' | {'error', Error},
      Error  :: 'not_found' | 'simple_one_for_one',
      Name   :: string().
%% @doc when we want to clear stock data
%%      we need to say about it to process
%%      and send request to supervisor
erase(Name) ->
  case gproc:lookup_local_name(Name) of
    undefined -> ok;
    Pid ->
      %% we send message to worker because
      %% it should cancel `heir` option
      %% for ets tables
      gen_server:call(Pid, before_death),
      stock_sup:stop_worker(Pid)
  end.

-spec info(Name) -> Result when
      Name   :: string(),
      Result :: [] | [Info],
      Info   :: {Type, Value},
      Type   :: 'name' | 'wordsize' | 'heap_size' | 'total_heap_size',
      Value  :: string() | number() | float().
%% @doc get memory consumption of specified process
info(Name) when is_list(Name) ->
  info(Name, gproc:lookup_local_name(Name)).

info(_, undefined) -> [];

%% get heap size of process and wordsize
info(Name, Worker) ->
  [
    %% list of tuples - prepared for to_json()
    {name, iolist_to_binary(Name)},
    gproc:info(Worker, heap_size),
    gproc:info(Worker, total_heap_size),
    {wordsize, erlang:system_info(wordsize)}
  ].

-spec names() -> Result when
      Result :: list().
%% @doc list of available workers
names() ->
  lists:map(
    fun(N) -> iolist_to_binary(N) end,
    %% select local names with `stock` tag
    gproc:select([{{{n, l, '$1'}, '_', stock}, [], ['$1']}])
  ).

%% store new data to ets tables
to_ets(Worker, Data) ->
  Sorted = lists:keysort(1, Data), %% sort data according time
  {Merged, Last} = lists:foldl(fun premerge/2, {[], []}, Sorted), %% scale to minutes
  gen_server:cast(Worker, {put, [Last | Merged]}). %% send to worker

-spec store(Name, Data) -> 'ok' | {'error', Reason} when
      Name   :: string(),
      Data   :: [Obj],
      Obj    :: {Time, Price, Volume},
      Time   :: number(),
      Price  :: float(),
      Volume :: number(),
      Reason :: any().
%% @doc API point to store data
store(_, []) -> ok;

store(Name, Data) ->
  case get_or_create_worker(string:to_upper(Name)) of
    {error, Reason} -> {error, Reason};
    {ok, Worker} -> to_ets(Worker, Data)
  end.

-spec select(Name, From, To, Scale) -> Result when
      Name   :: string(),
      From   :: number(),
      To     :: number(),
      Scale  :: number(),
      Result :: [Obj],
      Obj    :: {Time, {Time0, Open}, {Time1, Close}, Max, Min, Volume},
      Time   :: number(), %% begin of time range unit
      Time0  :: number(), %% time of first deal
      Time1  :: number(), %% time of last deal
      Open   :: float(),  %% open price
      Close  :: float(),  %% close price
      Max    :: float(),  %% max price
      Min    :: float(),  %% min price
      Volume :: number(). %% total volume of time range
%% @doc API point to select data
select(Name, From, To, Scale) ->
  case gproc:lookup_local_name(string:to_upper(Name)) of
    undefined -> [];
    Pid -> from_ets(Pid, From, To, Scale)
  end.

%% for minute time scale we have premerged data in table
from_ets(Worker, From, To, ?MINUTE_SCALE) when From < To ->
  gen_server:call(Worker, {get, {From, To}});

%% get data from ets and merge it according scale unit
from_ets(Worker, F, T, S) when F < T ->
  From  = correct_from(F, S),
  To    = correct_to(T, S),
  All = collect(Worker, From, To, S), %% retrieve and merge data
  lists:filter(fun(E) -> E =/= [] end, All); %% remove empty

from_ets(_, _, _, _) -> [].

%% We walk from end to begin because of reverse order.
%% We retrieve data from worker for range of time
%% and collect it to list.
collect(Worker, From, To, Scale) ->
  Limit = correct_from(To, Scale), %% down limit of first step
  collect0([], Worker, From, Limit, To, Scale).

%% when we pass From time limit - return data
collect0(Data, _, From, _, To, _) when To =< From -> Data;

collect0(Data, Worker, From, Limit, To, Scale) ->
  NextLimit = correct_from(Limit - 1, Scale), %% down limit of next step
  V = gen_server:call(Worker, {merge, {Limit, To}}), %% get merged data from worker
  collect0([V | Data], Worker, From, NextLimit, Limit, Scale). %% continue for next step

before({Time0, _} = V, {Time1, _}) when Time1 >= Time0 -> V;
before(_, V) -> V.

later({Time0, _} = V, {Time1, _}) when Time0 >= Time1 -> V;
later(_, V) -> V.

%% Fold-like function.
%% It walks through the list and merge
%% elements to one ohlc temp object.
%% When function passes time step (one minute)
%% it puts temp ohlc value in list and begin
%% to merge data for the next time range.
premerge(V, {Data, []}) ->
  Time = correct_from(erlang:element(1, V), 0),
  {Data, merge([], V, Time)};

%% V0 - next element in list
%% V1 - current temp ohlc object
premerge(V0, {Data, V1}) ->
  Time0 = erlang:element(1, V0),
  Time1 = erlang:element(1, V1),
  if
    Time0 >= Time1 + ?MINUTE ->
      Time = correct_from(Time0, 0), %% get correct time stamp
      {[V1 | Data], merge([], V0, Time)};
    true -> {Data, merge(V1, V0)}
  end.

%% merge raw data with already premerged
merge([], {Time, Price, Volume}, Time0) ->
  {Time0, {Time, Price}, {Time, Price}, Price, Price, Volume};

merge([], V, Time0) when is_tuple(V) ->
  erlang:setelement(1, V, Time0);

merge([V0], V1, Time) -> merge(V0, V1, Time);

merge(V0, {Time, Price, Volume}, Time0) ->
  merge(V0, {Time, {Time, Price}, {Time, Price}, Price, Price, Volume}, Time0);

%% merge two values
merge({Time0, Open0, Close0, Min0, Max0, Volume0},
      {_,     Open1, Close1, Min1, Max1, Volume1}, Time0) ->
  {Time0, before(Open0, Open1), later(Close0, Close1), min(Min0, Min1), max(Max0, Max1), Volume0 + Volume1}.

merge([], V1) ->
  merge([], V1, erlang:element(1, V1));

merge([V0], V1) ->
  merge(V0, V1);

merge(V0, V1) ->
  merge(V0, V1, erlang:element(1, V0)).

%% check if worker with required name exists
%% and return it's pid back or create new
get_or_create_worker(Name) ->
  case gproc:lookup_local_name(Name) of
    undefined -> %% create new worker
      case stock_sup:start_worker(Name) of
        {ok, Worker} -> {ok, Worker};
        _ -> {error, "Cannot create worker: " ++ Name}
      end;
    Pid -> %% we already have worker
      {ok, Pid}
  end.

%% correct from timestamp to end of time unit
correct_to(From, ?MINUTE_SCALE) ->
  correct_from(From + ?MINUTE, ?MINUTE_SCALE);

correct_to(From, ?HOUR_SCALE) ->  %% hour
  correct_from(From + ?HOUR, ?HOUR_SCALE);

correct_to(From, ?DAY_SCALE) ->  %% day
  correct_from(From + ?DAY, ?DAY_SCALE);

%% correct timestamp for end of week
correct_to(From, ?WEEK_SCALE) ->
  correct_from(From + ?WEEK, ?WEEK_SCALE);

%% correct timestamp for end of month
correct_to(From, ?MONTH_SCALE) ->
  {{Ye, Mo, _}, _} = stock_util:to_date(From),
  case Mo of
    12 -> stock_util:from_date({{Ye + 1,  1, 1}, {0, 0, 0}});
    _  -> stock_util:from_date({{Ye, Mo + 1, 1}, {0, 0, 0}})
  end.

%% correct from timestamp to begin of time unit
correct_from(From, ?MINUTE_SCALE) ->
  erlang:trunc(From / ?MINUTE) * ?MINUTE;

correct_from(From, ?HOUR_SCALE) ->  %% hour
  erlang:trunc(From / ?HOUR) * ?HOUR;

correct_from(From, ?DAY_SCALE) ->  %% day
  erlang:trunc(From / ?DAY) * ?DAY;

%% correct timestamp for begin of week
correct_from(From, ?WEEK_SCALE) ->
  {Date, _} = stock_util:to_date(From),
  stock_util:from_date({stock_util:safe_begin_of_week(Date), {0, 0, 0}});

%% correct timestamp for begin of month
correct_from(From, ?MONTH_SCALE) ->
  {{Ye, Mo, _}, _} = stock_util:to_date(From),
  stock_util:from_date({{Ye, Mo, 1}, {0, 0, 0}}).