-module(mix_covertool).

%% mix plugin callbacks
-export([start/2]).

-include("covertool.hrl").

%% ===================================================================
%% Mix plugin callbacks
%% ===================================================================
start(CompilePath, Opts) ->
    _ = cover:start(),

    BaseCompilePath = 'Elixir.Path':expand(<<CompilePath/binary, <<"../../..">>/binary>>),
    Deps = 'Elixir.Mix.Dep':cached(),
    LocalDeps = lists:filter(
        fun(Dep) ->
            DepOpts = maps:get(opts, Dep),
            AppName = maps:get(app, Dep),
            (lists:keyfind(lock, 1, DepOpts) == {lock, nil}) and (AppName =/= nex_protocol)
        end,
        Deps
    ),
    SrcDirs = lists:map(
        fun(Dep) ->
            DepOpts = maps:get(opts, Dep),
            binary:bin_to_list(proplists:get_value(dest, DepOpts))
        end,
        LocalDeps
    ),
    LocalDepsDirectories = lists:map(
        fun(Dep) ->
            AppName = maps:get(app, Dep),
            {
                <<BaseCompilePath/binary, <<"/">>/binary, (atom_to_binary(AppName))/binary,
                    <<"/ebin">>/binary>>,
                AppName
            }
        end,
        LocalDeps
    ),
    AppName = proplists:get_value(app, mix_project(config)),
    LocalDepsDirectories2 = LocalDepsDirectories ++ [{CompilePath, AppName}],
    DepsBeamDirs = lists:map(fun({Dir, _}) -> Dir end, LocalDepsDirectories2),
    DepsMap = maps:from_list(LocalDepsDirectories2),

    BeamDirs = DepsBeamDirs,
    ModuleAppList = lists:flatmap(
        fun(BeamDir) ->
            case cover:compile_beam_directory(binary:bin_to_list(BeamDir)) of
                Results when is_list(Results) ->
                    AppName2 = maps:get(BeamDir, DepsMap),
                    NonFilteredList = lists:map(
                        fun(Result) ->
                            case Result of
                                {ok, Module} ->
                                    {Module, AppName2};
                                _ ->
                                    ok
                            end
                        end,
                        Results
                    ),
                    lists:filter(fun(Item) -> Item =/= ok end, NonFilteredList);
                {error, _} ->
                    mix(raise, <<"Failed to cover compile directory">>)
            end
        end,
        BeamDirs
    ),

    ModuleMapping = maps:from_list(ModuleAppList),

    {ok, SrcDir} = file:get_cwd(),
    Summary = proplists:get_bool(summary, Opts),
    Config = #config{
        appname = AppName,
        sources = [SrcDir] ++ SrcDirs,
        module_mapping = ModuleMapping,
        beams = BeamDirs,
        summary = Summary
    },

    Modules = cover:modules(),

    fun() ->
        covertool:generate_report(Config, Modules)
    end.

%% ===================================================================
%% Mix helpers
%% ===================================================================
mix(Fun, Arg) ->
    'Elixir.Mix':Fun(Arg).

mix_project(Fun) ->
    'Elixir.Mix.Project':Fun().
