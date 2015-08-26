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
        binary,
        dictionary
    ],
    case erlang:process_info(P, Items) of
        Info when is_list(Info) ->
            Behaviour = gather_behaviour(Info),
            [[
                {pid, P},
                {binary_memory, gather_binary_memory(Info)},
                {ancestry, gather_ancestry(Info)},
                {behaviour, Behaviour},
                {state, gather_state(P, Behaviour)}
                    | Info
            ]];
        undefined ->
            []
    end.

gather_binary_memory(Info) ->
    lists:foldl(fun({_, Mem, _}, Tot) -> Mem + Tot end, 0, kget(binary, Info)).

gather_ancestry(Info) ->
    kget('$ancestors', kget(dictionary, Info), []).

gather_behaviour(Info) ->
    InitialCall = kget('$initial_call', kget(dictionary, Info)),
    [CurrentCall | _] = kget(current_stacktrace, Info),
    select_behaviour(InitialCall, CurrentCall).

gather_state(Pid, {supervisor, _Module}) ->
    guard(fun () -> {children, supervisor:which_children(Pid)} end);
gather_state(Pid, {gen_server, _Module}) ->
    guard(fun () -> {state, sys:get_state(Pid, 100)} end);
gather_state(Pid, {gen_fsm, _Module}) ->
    guard(fun () -> {State, Extra} = sys:get_state(Pid, 100), {state, State, Extra} end);
gather_state(Pid, gen_event) ->
    guard(fun () -> {handlers, gen_event:which_handlers(Pid)} end);
gather_state(_Pid, undefined) ->
    undefined.

guard(Fun) ->
    try Fun() catch _:_ -> undefined end.

select_behaviour({supervisor, Module, _}, _) ->
    {supervisor, Module};
select_behaviour({Module, init, 1}, Call) when element(1, Call) == gen_server ->
    {gen_server, Module};
select_behaviour({Module, init, 1}, Call) when element(1, Call) == gen_fsm ->
    {gen_fsm, Module};
select_behaviour({gen_event, _, _}, _) ->
    gen_event;
select_behaviour(_, _) ->
    undefined.

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
    ancestry           := Ancestry,
    initial_call       := InitialCall,
    memory             := Memory,
    reductions         := Reductions,
    messages           := Messages,
    binary_memory      := Binary,
    current_stacktrace := Stacktrace,
    behaviour          := Behaviour,
    state              := State
}) ->
    Bl = "   - ",
    Pre = "        ",
    [" * ", format_pidname(Pid, RegName), nl(),
        Bl, "status       : ", to_bin(Status), nl(),
        Bl, "initial call : ", format_call(InitialCall), nl(),
        format_line_opt([Bl, "ancestry     : "], fun format_ancestry/1, Ancestry),
        format_line_opt([Bl, "messagebox   : "], fun (M) -> format_messages(Pre, M) end, Messages),
        Bl, "memory       : ", to_bin(Memory), " bytes", nl(),
        Bl, "binaries     : ", to_bin(Binary), " bytes", nl(),
        Bl, "reductions   : ", to_bin(Reductions), nl(),
        Bl, "stacktrace   : ", nl(), Pre, genlib_format:format_stacktrace(Stacktrace, [newlines]), nl(),
        format_line_opt([Bl, "behaviour    : "], fun format_behaviour/1, Behaviour),
        format_line_opt([Bl], fun (S) -> format_state(Pre, S) end, State)
    ].

format_call({M, F, A}) ->
    [to_bin(M), $:, to_bin(F), $/, to_bin(A)].

format_line_opt(_, _, []) ->
    [];
format_line_opt(_, _, undefined) ->
    [];
format_line_opt(Pre, F, List) ->
    [Pre, F(List), nl()].

format_ancestry([E]) ->
    [format_name(E)];
format_ancestry([E | Rest]) ->
    [format_name(E), " <- ", format_ancestry(Rest)].

format_pidname(Pid, RegName) ->
    [pid_to_list(Pid), $\s, to_bin(RegName)].

format_name(Pid) when is_pid(Pid) ->
    pid_to_list(Pid);
format_name(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8).

format_messages(Pre, List) ->
    Sub = lists:sublist(List, 5),
    [nl(),
        [[Pre, genlib:print(M, 100)] || M <- Sub] |
        case List of
            Sub -> [];
            _   -> [Pre, "... and ", to_bin(length(List) - 5), " more"]
        end
    ].

format_behaviour({What, Module}) ->
    [to_bin(What), " (via module: ", to_bin(Module), ")"];
format_behaviour(What) ->
    to_bin(What).

format_state(Pre, What) ->
    [pad(to_bin(element(1, What)), 12), " : ", nl() | format_state_data(Pre, What)].

format_state_data(Pre, {handlers, Handlers}) ->
    [[Pre, genlib:print(H, 100), nl()] || H <- Handlers];
format_state_data(Pre, {children, Children}) ->
    [[Pre, format_child(C), nl()] || C <- Children];
format_state_data(Pre, {state, StateName, Extra}) ->
    [Pre, genlib:print(StateName, 100), nl() | format_state_data(Pre, {state, Extra})];
format_state_data(Pre, {state, Extra}) ->
    [Pre, genlib:print(Extra, 800), nl()].

format_child({Name, Pid, Type, _}) ->
    [pad(to_bin(Type), 10), " : ", padl(format_sup_pid(Pid), 12), " ", genlib:print(Name, 60)].

format_sup_pid(Special) when is_atom(Special) ->
    [$<, to_bin(Special), $>];
format_sup_pid(Pid) when is_pid(Pid) ->
    pid_to_list(Pid).

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

pad(S, N) ->
    genlib_string:pad_string(iolist_to_binary(S), N).

padl(S, N) ->
    genlib_string:pad_left(iolist_to_binary(S), $\s, N).

nl() ->
    io_lib:nl().
