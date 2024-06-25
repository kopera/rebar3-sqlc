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
    rebar_api:warn("Running sqlc...", []),
    Apps = case rebar_state:current_app(State) of
        undefined -> rebar_state:project_apps(State);
        App -> [App]
    end,
    {ok, lists:foldl(fun (App, StateAcc) ->
        compile_app(App, StateAcc)
    end, State, Apps)}.


compile_app(App, State) ->
    Opts = rebar_app_info:opts(App),
    AppDir = rebar_app_info:dir(App),
    SourceDirs = rebar_dir:src_dirs(rebar_app_info:opts(App)),
    lists:foreach(fun (SourceDir0) ->
        SourceDir = filename:join(AppDir, SourceDir0),
        rebar_api:debug("Compiling sqlc in dir: ~s", [SourceDir]),
        rebar_base_compiler:run(Opts, [], SourceDir, ".sql", SourceDir, ".erl", fun compile/3, [{check_last_mod, false}])
    end, SourceDirs),
    State.


compile(Source, _Target, Options) ->
    Target = filename:rootname(Source) ++ ".erl",
    rebar_api:debug("Compiling sqlc: ~s to ~s", [Source, Target]),
    case sqlc:file(Source) of
        {ok, Module} ->
            case rebar_file_utils:write_file_if_contents_differ(Target, sqlc_module:to_erl(Module), utf8) of
                ok ->
                    rebar_base_compiler:ok_tuple(Source, []);
                {error, Error} ->
                    rebar_base_compiler:error_tuple(Source, [{Target, [{file, Error}]}], [], Options)
            end;
        {error, #{line := Line} = Error} ->
            rebar_base_compiler:error_tuple(Source, [{Source, [{Line, ?MODULE, Error}]}], [], Options)
    end.


-spec format_error(any()) -> iolist().
format_error(#{message := Message}) ->
    Message;
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
