-module(rebar_feature_runner).

-export(['run-features'/2]).

-define(DEBUG(Msg, Args), ?LOG(debug, Msg, Args)).
-define(WARN(Msg, Args), ?LOG(warn, Msg, Args)).
-define(LOG(Lvl, Msg, Args), rebar_log:log(Lvl, Msg, Args)).
-define(ABORT(Msg, Args), rebar_utils:abort(Msg, Args)).

'run-features'(RebarConfig, _Unknown) ->
  ?DEBUG("Searching for features in ~s ~n", [rebar_utils:get_cwd()]),
  FeaturesDir = filename:join(rebar_utils:get_cwd(), "features"),

  case filelib:is_dir(FeaturesDir)
  of
    true ->
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
