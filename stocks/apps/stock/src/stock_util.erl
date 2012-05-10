%% @author Andrew Kondratovich <andrew.kondratovich@gmail.com>
%% @doc some useful functions
%%

-module(stock_util).
-author('Andrew Kondratovich <andrew.kondratovich@gmail.com>').

-export([
  prettify/1,
  to_json/1,
  to_date/1,
  from_date/1,
  safe_begin_of_week/1
]).

prettify(Data) when is_list(Data) ->
  lists:map(fun prettify/1, Data);

%% prepare data for to_json encode
prettify({From, {_, Begin}, {_, End}, Min, Max, Volume}) ->
  [
    {"from", From},
    {"begin", Begin},
    {"end", End},
    {"min", Min},
    {"max", Max},
    {"summary", Volume}
  ].

to_json(Data) -> %% this is very slow =(
  iolist_to_binary(mochijson2:encode(Data)).

-spec to_date(Milliseconds) -> Result when
      Result        :: {Date, Time},
      Date          :: {number(), number(), number()},
      Time          :: {number(), number(), number()},
      Milliseconds  :: number().
%% @doc convert timestamp to date/time tuple
to_date(Milliseconds) ->
  BaseDate = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  Seconds = BaseDate + (Milliseconds div 1000),
  calendar:gregorian_seconds_to_datetime(Seconds).

-spec from_date(DateTime) -> Result when
      Result   :: number(),
      DateTime :: {Date, Time},
      Date     :: {number(), number(), number()},
      Time     :: {number(), number(), number()}.
%% @doc convert date/time tuple to timestamp
from_date(DateTime) ->
  BaseDate = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  (calendar:datetime_to_gregorian_seconds(DateTime) - BaseDate) * 1000.

%% get date of this week monday
safe_begin_of_week({Ye, Mo, Da}) ->
  safe_begin_of_week({Ye, Mo, Da}, calendar:day_of_the_week(Ye, Mo, Da)).

%% when monday in current month
safe_begin_of_week({Ye, Mo, Da}, D) when (Da - D) >= 0 ->
  {Ye, Mo, Da - (D - 1)};

%% what if monday in previous month ?
%% (but in this year)
safe_begin_of_week({Ye, Mo, Da}, D) when Mo > 1 ->
  P = calendar:last_day_of_the_month(Ye, Mo - 1),
  {Ye, Mo - 1, (P + Da) - (D - 1)};

%% the last case - monday in previous year
safe_begin_of_week({Ye, _, Da}, D) ->
  P = calendar:last_day_of_the_month(Ye - 1, 12),
  {Ye - 1, 12, (P + Da) - (D - 1)}.