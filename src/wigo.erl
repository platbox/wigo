%%%
%%% Application interface

-module(wigo).

%%

-export([main/1]).

%%

-spec main(Args :: [string()]) -> ok | no_return().

main(["ps" | Rest]) ->
    main_ps(Rest);
main(_) ->
    usage().

main_ps(["top", Criterion, Filename]) ->
    ok = wigo_ps:load(Filename),
    wigo_ps:top(list_to_atom(Criterion));

main_ps(["top", Criterion, N, Filename]) ->
    ok = wigo_ps:load(Filename),
    wigo_ps:top(list_to_atom(Criterion), list_to_integer(N));

main_ps(["i", PidTerm, Filename]) ->
    ok = wigo_ps:load(Filename),
    wigo_ps:i(PidTerm);

main_ps(_) ->
    usage().

usage() ->
    println("WHAT IS GOING ON?"),
    println(),
    println("  Usage: wigo ps ACTION [ARGS...] FILENAME"),
    println("         wigo ps top {memory|binary|message_queue|reductions} [N] FILENAME"),
    println("         wigo ps i PIDTERM FILENAME"),
    println(),
    println("         ... more to come"),
    println(),
    halt(1).

print(Text) ->
    io:format(Text).

println() ->
    println(<<>>).

println(Text) ->
    print([Text, io_lib:nl()]).
