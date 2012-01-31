-module(rebar_feature_runner).

-export(['run-features'/2]).

-define(DEBUG(Msg, Args), ?LOG(debug, Msg, Args)).
-define(WARN(Msg, Args), ?LOG(warn, Msg, Args)).
-define(LOG(Lvl, Msg, Args), rebar_log:log(Lvl, Msg, Args)).
-define(ABORT(Msg, Args), rebar_utils:abort(Msg, Args)).

'run-features'(_RebarConfig, _Unknown) ->
  Cwd = rebar_utils:get_cwd(),
  FeatureBasePath = rebar_config:get_global(path, "."),
      
  ?DEBUG("Searching for features in ~s ~n", [rebar_utils:get_cwd()]),
  FeaturesDir = filename:join([Cwd, FeatureBasePath, "features"]),

  case filelib:is_dir(FeaturesDir)
  of
    true ->
      code:add_path(filename:join([Cwd, FeatureBasePath, "ebin"])),
      run_dir(FeaturesDir);
    false ->
      ok
  end.

run_dir(FeaturesDir) ->
  {ok, Files} = file:list_dir(FeaturesDir),
  run_features(lists:map(fun (File) -> filename:join(FeaturesDir, File) end, Files)).

run_features([]) ->
  ok;
run_features([FeatureFile | Features]) ->
  ?DEBUG("Running feature ~s ~n", [FeatureFile]),
  run_feature(FeatureFile),
  run_features(Features).

run_feature(FeatureFile) ->
  cucumberl:run(FeatureFile).
