-module(strftime).
-export([f/2]).

-include_lib("eunit/include/eunit.hrl").


f({_MegaSec,_Sec,_MicroSec}=Tm, FormatStr) when is_list(FormatStr) ->
  Res = [do_f(Tm, FPart) || FPart <- re:split(FormatStr,"([%][^%])")],
  list_to_binary(Res).

do_f(Tm, <<"%D">>) ->
  {{YY, MM, DD},_} = calendar:now_to_local_time(Tm),
  io_lib:format("~2.2.0w/~2.2.0w/~4.4.0w",[MM,DD,YY]);

do_f(_Tm,Str) -> Str.


% 2011-06-20 00:07:50.46425 UTC
test_tm() -> {1308,528470,46435}.

%D test
f_D_test() ->
  ?assertEqual(<<"06/19/2011">>, f(test_tm(), "%D")).



