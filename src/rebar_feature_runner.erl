%Copyright [2012] [Farruco Sanjurjo Arcay]

%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at

%       http://www.apache.org/licenses/LICENSE-2.0

%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
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
  CompileOptions = [
    binary,
    report,
    warnings_as_errors,
    {i, "ebin"},
    {i, "include"}],

  BaseName = filename:basename(FeatureFile, ".feature"),
  SrcDir   = filename:dirname(filename:dirname(FeatureFile)),
  SrcFile  = filename:join([SrcDir, "src", BaseName]),

  case compile:file(SrcFile, CompileOptions) of
    {ok, Module, Binary} ->
      code:load_binary(Module, SrcFile, Binary),
      cucumberl:run(FeatureFile);
    error -> error
  end.

