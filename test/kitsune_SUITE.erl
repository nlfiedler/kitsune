%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Nathan Fiedler
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
%%
%% The test suite.
%%
-module(kitsune_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

init_per_suite(Config) ->
    % ensure lager is configured for testing
    ok = application:set_env(lager, lager_common_test_backend, debug),
    % starting our app starts everything else that we need (e.g. hackney)
    {ok, _Started} = application:ensure_all_started(kitsune),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(kitsune).

all() ->
    [
        fetch_repos_test,
        process_repos_test
    ].

fetch_repos_test(_Config) ->
    % use a GitHub user that we know exists
    {ok, Repos} = kitsune:fetch_repos("nlfiedler"),
    % this user has at least one page of results, which is a good test
    ?assert(length(Repos) > 30),
    % the repo for this project had better be in the results
    ?assertNotEqual(undefined, proplists:get_value("kitsune", Repos)),
    ok.

% Test the processing of repositories functionality.
process_repos_test(_Config) ->
    % TODO: implement the tests for the backup procedure
    ok.
