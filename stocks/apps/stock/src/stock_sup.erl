%% @author Andrew Kondratovich <andrew.kondratovich@gmail.com>
%% @doc Worker supervisor
%%

-module(stock_sup).
-author('Andrew Kondratovich <andrew.kondratovich@gmail.com>').

-behaviour(supervisor).

%% API
-export([
  start_link/0,
  start_worker/1,
  stop_worker/1
]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-define(WORKER_SUP, stock_worker_sup).
-define(WORKER_MOD, stock_worker).

start_worker(Args) ->
  supervisor:start_child(?WORKER_SUP, [Args]).

stop_worker(Worker) ->
  supervisor:terminate_child(?WORKER_SUP, Worker).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([stock_worker_start]) -> %% Worker supervisor
  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  Children = [{worker, {?WORKER_MOD, start_link, []}, Restart, Shutdown, Type, [?WORKER_MOD]}],

  Strategy = simple_one_for_one,
  MaxR = 10, MaxT = 10,
  {ok, {{Strategy, MaxR, MaxT}, Children}};

init([]) -> %% Main supervisor
  Restart = permanent,
  Shutdown = brutal_kill,
  Children = [{?WORKER_SUP, {supervisor, start_link, [{local, ?WORKER_SUP}, ?MODULE, [stock_worker_start]]}, Restart, Shutdown, supervisor, [?MODULE]}],
  Strategy = one_for_one,
  MaxR = 5, MaxT = 10,
  {ok, {{Strategy, MaxR, MaxT}, Children}}.

