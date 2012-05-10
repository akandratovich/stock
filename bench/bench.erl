%% @author Andrew Kondratovich <andrew.kondratovich@gmail.com>
%% @doc Some benchmarks for stock app
%%

-module(bench).
-author('Andrew Kondratovich <andrew.kondratovich@gmail.com>').

-compile([export_all]).
% -export([perform/0]).

-define(PRICE0, 0.0).
-define(PRICE1, 2.0).
-define(VOLUME, 1000).
-define(MINUTE, (60 * 1000)).


random_price(P0, P1) ->
  V = random:uniform(),
  P0 + V * (P1 - P0).

random_time(F, T) ->
  V = random:uniform(erlang:trunc(T - F)),
  erlang:trunc(F + V).

generate_value(P0, P1, F, T, Volume) ->
  {random_time(F, T), random_price(P0, P1), random:uniform(Volume)}.

generate(From, Minutes, PerMinute) ->
  lists:map(fun(E) ->
    generate_value(
      ?PRICE0,
      ?PRICE1,
       E      * ?MINUTE / PerMinute,
      (E + 1) * ?MINUTE / PerMinute,
      ?VOLUME)
  end, lists:seq(From, Minutes * PerMinute - 1)).

store(Name, Data) ->
  Self = self(),
  Node = node0(),
  erlang:spawn(Node, fun() -> Self ! stock_api:store(Name, Data) end).

select(Name, From, To, Scale) ->
  Self = self(),
  Node = node0(),
  erlang:spawn(Node, fun() -> Self ! stock_api:select(Name, From, To, Scale) end).

erase(Name) ->
  Node = node0(),
  erlang:spawn(Node, fun() -> stock_api:erase(Name) end).

node0() -> 'stocknode@127.0.0.1'.

t_store(TSize, Per) ->
  Data = generate(0, TSize, Per),
  {Time, _} = timer:tc(fun() ->
    store("TEST_STORE", Data),
    receive _ -> ok after 10000 -> timeout end
  end),
  Time.

t_select(Count, Scale) ->
  {Time, _} = timer:tc(fun() ->
    select("TEST_STORE", 0, Count * ?MINUTE, Scale),
    receive _ -> ok after 10000 -> timeout end
  end),
  Time.

perform() ->
  case net_adm:ping('stocknode@127.0.0.1') of
    pang -> exit(stocknode_not_found);
    _ -> c:nl(?MODULE)
  end.