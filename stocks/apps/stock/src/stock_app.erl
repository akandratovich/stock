%% @author Andrew Kondratovich <andrew.kondratovich@gmail.com>
%% @doc Stock application source file
%%

-module(stock_app).
-author('Andrew Kondratovich <andrew.kondratovich@gmail.com>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  ets_manager:start(), %% start ets manager
  stock_sup:start_link().

stop(_State) ->
  ok.
