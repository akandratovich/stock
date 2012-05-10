%% @author Andrew Kondratovich <andrew.kondratovich@gmail.com>
%% @doc Worker that handles performance requests
%%      Updates info one time in 2 seconds
%%

-module(stock_perf).
-author('Andrew Kondratovich <andrew.kondratovich@gmail.com>').

-behaviour(gen_server).
-export([start_link/1]).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3,
  update_perf/1
]).

%%% API
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%%% gen_server callbacks

init(_Args) ->
  true = gproc:reg({n, l, perf}, perf),
  timer:apply_interval(2000, ?MODULE, update_perf, [self()]), %% every 2 second update info
  {ok, perf()}.

update_perf(Worker) ->
  gen_server:cast(Worker, update_perf).

nnow() ->
  {Ms, S, _} = erlang:now(),
  S * 1000 + Ms * 1000 * 1000 * 1000.

perf() ->
  Cpu = case os:type() of
    {unix, _} -> cpu_sup:util();
    _ -> 0.0  %% os specific
  end,
  Mem = erlang:memory(total),
  {Cpu, Mem, nnow()}.

%% return current state {Cpu, Mem, Time}
handle_call(perf, _From, State) ->
  {reply, State, State};

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(update_perf, _State) ->
  {noreply, perf()};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
