-module(rebar3_sqlc_prv_compile).
-export([
    init/1,
    do/1,
    format_error/1
]).

-define(NAMESPACE, sqlc).
-define(PROVIDER, compile).
-define(DEPS, [{default, app_discovery}]).


-spec init(State) -> State when
    State :: rebar_state:t().
init(State) ->
    rebar_state:add_provider(State, providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, false},
        {deps, ?DEPS},
        {example, "rebar3 sqlc compile"},
        {opts, []},
        {short_desc, "A rebar plugin"},
        {desc, "A rebar plugin"}
    ])).


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Running sqlc...", []),
    lists:foreach(fun (App) ->
        Opts = rebar_app_info:opts(App),
        SourceDirs = rebar_dir:src_dirs(rebar_app_info:opts(App)),
        lists:foreach(fun (SourceDir) ->
            rebar_base_compiler:run(Opts, [], SourceDir, ".sql", SourceDir, ".erl", fun compile/3)
        end, SourceDirs)
    end, rebar_state:project_apps(State)).


compile(Source, Target, Options) ->
    case sqlc:file(Source) of
        {ok, Module} ->
            case rebar_file_utils:write_file_if_contents_differ(Target, sqlc_module:to_erl(Module), utf8) of
                ok ->
                    rebar_base_compiler:ok_tuple(Source, []);
                {error, Error} ->
                    Message = file:format_error(Error),
                    rebar_base_compiler:error_tuple(Target, [Message], [], Options)
            end;
        {error, #{message := Message}} ->
            rebar_base_compiler:error_tuple(Source, [Message], [], Options)
    end.


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
