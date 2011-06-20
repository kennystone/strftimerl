-module(strftime).
-export([f/2]).

-include_lib("eunit/include/eunit.hrl").


f({_MegaSec,_Sec,_MicroSec}=Tm, FormatStr) when is_list(FormatStr) ->
  Res = [do_f(Tm, FPart) || FPart <- re:split(FormatStr,"([%][^%])")],
  binary_to_list(list_to_binary(Res)).

do_f(Tm, <<"%d">>) ->
  {{_YY,_MM,DD},_} = calendar:now_to_local_time(Tm),
  f2(DD);

do_f(Tm, <<"%m">>) ->
  {{_YY,MM,_DD},_} = calendar:now_to_local_time(Tm),
  f2(MM);

do_f(Tm, <<"%y">>) ->
  {{YY,_MM,_DD},_} = calendar:now_to_local_time(Tm),
  f2(YY);

do_f(Tm, <<"%Y">>) ->
  {{YY,_MM,_DD},_} = calendar:now_to_local_time(Tm),
  f4(YY);

do_f(Tm, <<"%D">>) ->
  {{YY, MM, DD},_} = calendar:now_to_local_time(Tm),
  [f2(MM), "/", f2(DD), "/", f2(YY rem 100)];

do_f(_Tm,Str) -> Str.

f2(N) -> io_lib:format("~2.2.0w",[(N rem 100)]).
f4(N) -> io_lib:format("~4.4.0w",[(N rem 10000)]).

% 2011-06-20 00:07:50.46425 UTC
test_tm() -> {1308,528470,46435}.

%D test
f_D_test() ->
  ?assertEqual("06/19/11", f(test_tm(), "%D")),
  ?assertEqual("fooey06/19/11fooey", f(test_tm(), "fooey%Dfooey")).

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



