%%%
%%% Application interface

-module(wigo).

%%

-export([start/0]).
-export([stop/0]).

-type cref() :: any().
-type alarm() :: {down | up, options()}.
-type options() :: #{atom() => any()}.

-export_type([
    cref/0,
    alarm/0,
    options/0
]).

-export([
    watch/2,
    unwatch/1
]).

% -export([
%     suspend/0,
%     suspend/1,
%     resume/0,
%     resume/1
% ]).

%%

-spec start() -> ok.

start() ->
    deathtoll_app:start_app(?MODULE).

-spec stop() -> ok | {error, term()}.

stop() ->
    deathtoll_app:stop_app(?MODULE).

%%

-spec watch(cref(), options()) -> {ok, pid()} | {error, any()}.

watch(Ref, Options) ->
    case deathtoll_app:start_child(Ref, Options) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        Error ->
            Error
    end.

-spec unwatch(cref()) -> ok | {error, not_found}.

unwatch(Ref) ->
    deathtoll_app:stop_child(Ref).
