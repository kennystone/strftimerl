-module(strftime).
-export([f/2, f/3]).

-include_lib("eunit/include/eunit.hrl").

% strftime:f(now(), FormatString)
% strftime:f(now(), FormatString, universal | local)

% because formatting times should be easy, like ruby's strftime

% strftime:f(now(), "Printed on %m/%d/%Y").    => "Printed on 11/19/2007"
% strftime:f(now(), "at %I:%M%p").             => "at 08:37AM"
% strftime:f(now(), "at %I:%M%p", universal).  => "at 02:37PM"
% strftime:f(now(), "%D-%T.%N").               => "11/19/2007-08:38:02.445443"

% `strftime:f2` uses system local time

% `strftime:f3` allows you to specify 'local' or 'universal' as third argument.

% `strftime:f(now(), FormatString)`

% `%a` - The abbreviated weekday name ('Sun')
% `%A` - The  full  weekday  name ('Sunday')
% `%b` - The abbreviated month name ('Jan')
% `%B` - The  full  month  name ('January')
% `%C` - Century (20 in 2009)
% `%d` - Day of the month (01..31)
% `%D` - Date (%m/%d/%y)
% `%e` - Day of the month, blank-padded ( 1..31)
% `%F` - Equivalent to %Y-%m-%d (the ISO 8601 date format)
% `%h` - Equivalent to %b
% `%H` - Hour of the day, 24-hour clock (00..23)
% `%I` - Hour of the day, 12-hour clock (01..12)
% `%k` - hour, 24-hour clock, blank-padded ( 0..23)
% `%l` - hour, 12-hour clock, blank-padded ( 0..12)
% `%L` - Millisecond of the second (000..999)
% `%m` - Month of the year (01..12)
% `%M` - Minute of the hour (00..59)
% `%N` - Fractional seconds digits
% `%p` - Meridian indicator ('AM'  or  'PM')
% `%P` - Meridian indicator ('am'  or  'pm')
% `%r` - time, 12-hour (same as %I:%M:%S %p)
% `%R` - time, 24-hour (%H:%M)
% `%s` - Number of seconds since 1970-01-01 00:00:00 UTC.
% `%S` - Second of the minute (00..60)
% `%T` - time, 24-hour (%H:%M:%S)
% `%u` - Day of the week as a decimal, Monday being 1. (1..7)
% `%v` - VMS date (%e-%b-%Y)
% `%w` - Day of the week (Sunday is 0, 0..6)
% `%y` - Year without a century (00..99)
% `%Y` - Year with century
% `%%` - Literal '%' character



f(Now, FormatStr) ->
  f(Now, FormatStr, local).

f({_MegaSec,_Sec,_MicroSec}=Now, FormatStr, ZONAL) when is_list(FormatStr) ->
  ZONALF = case ZONAL of
    local     -> now_to_local_time;
    universal -> now_to_universal_time;
    Else      -> Else
  end,
  Res = [do_f(Now, FPart, ZONALF) || FPart <- re:split(FormatStr,"([%][^%])")],
  binary_to_list(list_to_binary(Res)).

do_f(Tm, <<"%d">>, ZONAL) ->
  {{_YY,_MM,DD},_} = calendar:ZONAL(Tm),
  f2(DD);

do_f(Tm, <<"%e">>, ZONAL) ->
  {{_YY,_MM,DD},_} = calendar:ZONAL(Tm),
  pad(integer_to_list(DD),2);

do_f(Tm, <<"%m">>, ZONAL) ->
  {{_YY,MM,_DD},_} = calendar:ZONAL(Tm),
  f2(MM);

do_f(Tm, <<"%y">>, ZONAL) ->
  {{YY,_MM,_DD},_} = calendar:ZONAL(Tm),
  f2(YY);

do_f(Tm, <<"%Y">>, ZONAL) ->
  {{YY,_MM,_DD},_} = calendar:ZONAL(Tm),
  f4(YY);

do_f(Tm, <<"%C">>, ZONAL) ->
  {{YY,_MM,_DD},_} = calendar:ZONAL(Tm),
  f2(round(YY/100));

do_f(Tm, <<"%H">>, ZONAL) ->
  {_,{H,_M,_S}} = calendar:ZONAL(Tm),
  f2(H);

do_f(Tm, <<"%l">>, ZONAL) ->
  {_,{H,_M,_S}} = calendar:ZONAL(Tm),
  pad(integer_to_list(H), 2);

do_f(Tm, <<"%k">>, ZONAL) ->
  {_,{H,_M,_S}} = calendar:ZONAL(Tm),
  pad(integer_to_list(H), 2);

do_f(Tm, <<"%I">>, ZONAL) ->
  {_,{H,_M,_S}} = calendar:ZONAL(Tm),
  case H < 13 of
    true -> f2(H);
    false -> f2(H-12)
  end;

do_f(Tm, <<"%M">>, ZONAL) ->
  {_,{_H,M,_S}} = calendar:ZONAL(Tm),
  f2(M);

do_f(Tm, <<"%S">>, ZONAL) ->
  {_,{_H,_M,S}} = calendar:ZONAL(Tm),
  f2(S);

do_f(Tm, <<"%u">>, ZONAL) ->
  {Date,_} = calendar:ZONAL(Tm),
  integer_to_list(calendar:day_of_the_week(Date));

do_f(Tm, <<"%w">>, ZONAL) ->
  {Date,_} = calendar:ZONAL(Tm),
  Day = calendar:day_of_the_week(Date),
  WDay = case Day of
    7 -> 0;
    _ -> Day - 1
  end,
  integer_to_list(WDay);

do_f({MegaSec,Sec,_}, <<"%s">>, _) -> 
  integer_to_list(1000000*MegaSec + Sec);

do_f(Tm, <<"%b">>, Z) -> abrv_mon(lists:flatten(do_f(Tm, <<"%m">>, Z)));
do_f(Tm, <<"%h">>, Z) -> do_f(Tm, <<"%b">>, Z);
do_f(Tm, <<"%B">>, Z) -> month(lists:flatten(do_f(Tm, <<"%m">>, Z)));
do_f(Tm, <<"%a">>, Z) -> abrv_day(lists:flatten(do_f(Tm, <<"%u">>, Z)));
do_f(Tm, <<"%A">>, Z) -> weekday(lists:flatten(do_f(Tm, <<"%u">>, Z)));

do_f(Tm, <<"%p">>, ZONAL) ->
  {_,{H,_M,_S}} = calendar:ZONAL(Tm),
  case H < 12 of
    true -> "AM";
    false -> "PM"
  end;

do_f(Tm, <<"%P">>, ZONAL) ->
  {_,{H,_M,_S}} = calendar:ZONAL(Tm),
  case H < 12 of
    true -> "am";
    false -> "pm"
  end;

do_f({_,_,MicroSec}, <<"%N">>,_) -> integer_to_list(MicroSec);
do_f({_,_,MicroSec}, <<"%L">>,_) -> f3(round(MicroSec/1000));

do_f(Tm, <<"%D">>, Z) -> f(Tm, "%m/%d/%y", Z);
do_f(Tm, <<"%F">>, Z) -> f(Tm, "%Y-%m-%d", Z);
do_f(Tm, <<"%T">>, Z) -> f(Tm, "%H:%M:%S", Z);
do_f(Tm, <<"%R">>, Z) -> f(Tm, "%H:%M", Z);
do_f(Tm, <<"%r">>, Z) -> f(Tm, "%I:%M:%S %p", Z);
do_f(Tm, <<"%v">>, Z) -> f(Tm, "%e-%b-%Y", Z);

do_f(_Tm,Str,_) -> Str.

f2(N) -> io_lib:format("~2.2.0w",[(N rem 100)]).
f3(N) -> io_lib:format("~3.3.0w",[(N rem 1000)]).
f4(N) -> io_lib:format("~4.4.0w",[(N rem 10000)]).

pad(Str, 2) when length(Str) < 2 -> [" ",Str];
pad(Str,_N) -> Str.

abrv_day("1") -> "Mon";
abrv_day("2") -> "Tue";
abrv_day("3") -> "Wed";
abrv_day("4") -> "Thu";
abrv_day("5") -> "Fri";
abrv_day("6") -> "Sat";
abrv_day("7") -> "Sun".

weekday("1") -> "Monday";
weekday("2") -> "Tuesday";
weekday("3") -> "Wednesday";
weekday("4") -> "Thursday";
weekday("5") -> "Friday";
weekday("6") -> "Saturday";
weekday("7") -> "Sunday".

abrv_mon("01") -> "Jan";
abrv_mon("02") -> "Feb";
abrv_mon("03") -> "Mar";
abrv_mon("04") -> "Apr";
abrv_mon("05") -> "May";
abrv_mon("06") -> "Jun";
abrv_mon("07") -> "Jul";
abrv_mon("08") -> "Aug";
abrv_mon("09") -> "Sep";
abrv_mon("10") -> "Oct";
abrv_mon("11") -> "Nov";
abrv_mon("12") -> "Dec".

month("01") -> "January";
month("02") -> "February";
month("03") -> "March";
month("04") -> "April";
month("05") -> "May";
month("06") -> "June";
month("07") -> "July";
month("08") -> "August";
month("09") -> "September";
month("10") -> "October";
month("11") -> "November";
month("12") -> "December".


% 2011-06-19 19:07:50.46425 -0500
test_tm() -> {1308,528470,46435}.
% 2011-06-19 09:07:50.46425 -0500
test_tm2() -> {1308,492470,46435}.

%D test
f_D_test() ->
  ?assertEqual("06/19/11", f(test_tm(), "%D")),
  ?assertEqual("fooey06/19/11fooey", f(test_tm(), "fooey%Dfooey")).

%F test
f_F_test() ->
  ?assertEqual("2011-06-19", f(test_tm(), "%F")),
  ?assertEqual("fooey2011-06-19fooey", f(test_tm(), "fooey%Ffooey")).

%y test
f_y_test() ->
  ?assertEqual("11", f(test_tm(), "%y")),
  ?assertEqual("fooey11", f(test_tm(), "fooey%y")).

%m test
f_m_test() ->
  ?assertEqual("06", f(test_tm(), "%m")),
  ?assertEqual("fooey06", f(test_tm(), "fooey%m")).

%d test
f_d_test() ->
  ?assertEqual("19", f(test_tm(), "%d")),
  ?assertEqual("fooey19", f(test_tm(), "fooey%d")).

f_Y_test() ->
  ?assertEqual("2011", f(test_tm(), "%Y")),
  ?assertEqual("fooey2011", f(test_tm(), "fooey%Y")).

f_H_test() -> 
  ?assertEqual("19", f(test_tm(), "%H")),
  ?assertEqual("09", f(test_tm2(), "%H")).

f_l_test() -> 
  ?assertEqual("19", f(test_tm(), "%l")),
  ?assertEqual(" 9", f(test_tm2(), "%l")).

f_M_test() -> ?assertEqual("07", f(test_tm(), "%M")).
f_S_test() -> ?assertEqual("50", f(test_tm(), "%S")).
f_T_test() -> ?assertEqual("19:07:50", f(test_tm(), "%T")).
f_R_test() -> ?assertEqual("19:07", f(test_tm(), "%R")).
f_p_test() -> ?assertEqual("PM", f(test_tm(), "%p")).
f_P_test() -> ?assertEqual("pm", f(test_tm(), "%P")).
f_N_test() -> ?assertEqual("46435", f(test_tm(), "%N")).
f_L_test() -> ?assertEqual("046", f(test_tm(), "%L")).
f_u_test() -> ?assertEqual("7", f(test_tm(), "%u")).
f_w_test() -> ?assertEqual("0", f(test_tm(), "%w")).
f_a_test() -> ?assertEqual("Sun", f(test_tm(), "%a")).
f_A_test() -> ?assertEqual("Sunday", f(test_tm(), "%A")).
f_C_test() -> ?assertEqual("20", f(test_tm(), "%C")).
f_b_test() -> ?assertEqual("Jun", f(test_tm(), "%b")).
f_B_test() -> ?assertEqual("June", f(test_tm(), "%B")).
f_h_test() -> ?assertEqual("Jun", f(test_tm(), "%h")).
f_e_test() -> ?assertEqual("19", f(test_tm(), "%e")).
f_r_test() -> ?assertEqual("07:07:50 PM", f(test_tm(), "%r")).
f_v_test() -> ?assertEqual("19-Jun-2011", f(test_tm(), "%v")).
f_s_test() -> ?assertEqual("1308528470", f(test_tm(), "%s")).
f_I_test() -> 
  ?assertEqual("09", f(test_tm2(), "%I")),
  ?assertEqual("07", f(test_tm(), "%I")).
f_k_test() -> 
  ?assertEqual("19", f(test_tm(), "%k")),
  ?assertEqual(" 9", f(test_tm2(), "%k")).

f_universal_test() -> 
  ?assertEqual("06/20/11", f(test_tm(), "%D", universal)),
  ?assertEqual("00:07:50", f(test_tm(), "%T", universal)).

literal_percent_test() -> 
  ?assertEqual("%%19:07:50%%", f(test_tm(), "%%%T%%")).


