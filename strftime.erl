-module(strftime).
-export([f/2]).

-include_lib("eunit/include/eunit.hrl").


f({_MegaSec,_Sec,_MicroSec}=Tm, FormatStr) when is_list(FormatStr) ->
  Res = [do_f(Tm, FPart) || FPart <- re:split(FormatStr,"([%][^%])")],
  binary_to_list(list_to_binary(Res)).

do_f(Tm, <<"%y">>) ->
  {{YY,_MM,_DD},_} = calendar:now_to_local_time(Tm),
  io_lib:format("~2.2.0w",[(YY rem 100)]);


do_f(Tm, <<"%D">>) ->
  {{YY, MM, DD},_} = calendar:now_to_local_time(Tm),
  io_lib:format("~2.2.0w/~2.2.0w/~2.2.0w",[MM,DD,(YY rem 100)]);

do_f(_Tm,Str) -> Str.


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


