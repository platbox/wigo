%%%
%%% Process info dumps

-module(wigo_ps).

%%

-export([dump/0]).
-export([dump/1]).
-export([load/1]).

-export([i/1]).
-export([top/1]).
-export([top/2]).

%%

-spec dump() -> {ok, file:name()} | {error, atom()}.
-spec dump(file:name()) -> {ok, file:name()} | {error, atom()}.

dump() ->
    dump(make_temp_filename("wigo.ps." ++ get_safe_nodename())).

dump(Filename) ->
    Dump = gather_ps(),
    case file:write_file(Filename, term_to_binary(Dump)) of
        ok ->
            {ok, Filename};
        Error ->
            Error
    end.

gather_ps() ->
    lists:foldl(fun (P, Acc) -> gather_ps_(P) ++ Acc end, [], processes()).

gather_ps_(P) ->
    Items = [
        registered_name,
        status,
        initial_call,
        memory,
        reductions,
        messages,
        message_queue_len,
        current_stacktrace,
        binary
    ],
    case erlang:process_info(P, Items) of
        Info when is_list(Info) ->
            {binary, Binaries} = lists:keyfind(binary, 1, Info),
            [[{pid, P}, {binary_memory, gather_binary_memory(Binaries)} | Info]];
        undefined ->
            []
    end.

gather_binary_memory(Binaries) ->
    lists:foldl(fun({_, Mem, _}, Tot) -> Mem + Tot end, 0, Binaries).

-spec load(file:name()) -> ok | {error, atom()}.

load(Filename) ->
    case file:read_file(Filename) of
        {ok, Dump} ->
            PInfo = lists:map(fun maps:from_list/1, binary_to_term(Dump)),
            _Was = put({?MODULE, dump}, PInfo),
            ok;
        Error ->
            Error
    end.

%%

-type pidterm() :: pid() | {pos_integer(), pos_integer(), pos_integer()} | binary() | string().
-type criterion() :: memory | binary | message_queue | reductions.

-spec i(pidterm() | atom()) -> ok | undefined.

i(Name) when is_atom(Name) ->
    format_first(lists:dropwhile(fun (#{registered_name := N}) -> N /= Name end, get_dump()));
i(PidTerm) ->
    Pid = to_pid(PidTerm),
    format_first(lists:dropwhile(fun (#{pid := P}) -> P /= Pid end, get_dump())).

format_first([H | _]) ->
    io:format(format_process_info(H));
format_first([]) ->
    undefined.

-spec top(criterion()) -> ok.
-spec top(criterion(), pos_integer()) -> ok.

top(Param) ->
    top(Param, 3).

top(Param, N) ->
    Sorted = lists:sort(get_sorter(Param), get_dump()),
    io:format(format_process_info(lists:sublist(Sorted, N))).

get_sorter(memory) ->
    get_sorter_(memory);
get_sorter(binary) ->
    get_sorter_(binary_memory);
get_sorter(message_queue) ->
    get_sorter_(message_queue_len);
get_sorter(reductions) ->
    get_sorter_(reductions);
get_sorter(Invalid) ->
    error(badarg, [Invalid]).

get_sorter_(Param) ->
    fun (P1, P2) -> genlib_map:get(Param, P1, 0) > genlib_map:get(Param, P2, 0) end.

get_dump() ->
    case get({?MODULE, dump}) of
        Dump when is_list(Dump) ->
            Dump;
        undefined ->
            error(nodump)
    end.

format_process_info(PInfo) when is_list(PInfo) ->
    [[format_process_info(P), nl()] || P <- PInfo];

format_process_info(#{
    pid                := Pid,
    registered_name    := RegName,
    status             := Status,
    initial_call       := InitialCall,
    memory             := Memory,
    reductions         := Reductions,
    messages           := Messages,
    binary_memory      := Binary,
    current_stacktrace := Stacktrace
}) ->
    Bl = "   - ",
    Pre = "        ",
    PidString = pid_to_list(Pid),
    Name = case RegName of [] -> PidString; _ -> [PidString, $\s, to_bin(RegName)] end,
    [" * ", Name, nl(),
        Bl, "status       : ", to_bin(Status), nl(),
        Bl, "initial call : ", format_call(InitialCall), nl(),
        Bl, "messagebox   : ", format_messages(Messages), nl(),
        Bl, "memory       : ", to_bin(Memory), " bytes", nl(),
        Bl, "binaries     : ", to_bin(Binary), " bytes", nl(),
        Bl, "reductions   : ", to_bin(Reductions), nl(),
        Bl, "stacktrace   : ", nl(), Pre, genlib_format:format_stacktrace(Stacktrace, [newlines]), nl()
    ].

format_call({M, F, A}) ->
    [to_bin(M), $:, to_bin(F), $/, to_bin(A)].

format_messages([]) ->
    "";
format_messages(List) ->
    Sub = lists:sublist(List, 5),
    Pre = "        ",
    [nl(),
        [[Pre, genlib:print(M, 80)] || M <- Sub] |
        case List of
            Sub -> [];
            _   -> [Pre, "... and ", to_bin(length(List) - 5), " more"]
        end
    ].

%%

get_safe_nodename() ->
    re:replace(to_bin(node()), <<"[^a-zA-Z0-9.@]+">>, <<".">>, [global, {return, list}]).

make_temp_filename(Prefix) ->
    Now = erlang:now(),
    filename:join(get_temp_dir(), Prefix ++ "." ++ [integer_to_list(E) || E <- tuple_to_list(Now)]).

get_temp_dir() ->
    case os:getenv("TMPDIR") of
        Dir when is_list(Dir) -> Dir;
        false -> "/tmp"
    end.

to_pid(P) when is_pid(P) ->
    P;
to_pid({A, B, C}) ->
    c:pid(A, B, C);
to_pid(B) when is_binary(B) ->
    to_pid(binary_to_list(B));
to_pid(L) when is_list(L) ->
    list_to_pid(L).

to_bin(E) ->
    genlib:to_binary(E).

kget(Key, List) ->
    kget(Key, List, undefined).

kget(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {_, Value} -> Value;
        false -> Default
    end.

nl() ->
    io_lib:nl().
