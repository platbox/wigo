%%%
%%% Application interface

-module(wigo).

%%

-export([start/0]).

%%

-spec start() -> ok.

start() ->
    application:ensure_all_started(?MODULE).
