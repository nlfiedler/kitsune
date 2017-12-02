%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016-2017 Nathan Fiedler
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
-export([init_per_suite/1, end_per_suite/1, all/0]).
-export([fetch_repos_test/1]).
-export([timer_value_test/1]).
-export([clone_exists_test/1]).
-export([git_clone_test/1]).
-export([parallel_test/1]).
-export([process_repos_test/1]).
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
        timer_value_test,
        clone_exists_test,
        git_clone_test,
        parallel_test,
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

timer_value_test(_Config) ->
    ?assertEqual(3600000, kitsune:timer_value(hourly, 1)),
    ?assertEqual(43200000, kitsune:timer_value(hourly, 12)),
    ?assertEqual(172800000, kitsune:timer_value(daily, 2)),
    ?assertEqual(2419200000, kitsune:timer_value(weekly, 4)),
    ?assertError(function_clause, kitsune:timer_value(foobar, 10)),
    ?assertError(function_clause, kitsune:timer_value(daily, "10")),
    ok.

clone_exists_test(_Config) ->
    % Hack to ensure our repository exists by using an empty repo name; while
    % this is not a bare clone, it works for the purpose of the test.
    ?assert(kitsune:clone_exists(os:getenv("PWD"), "")),
    ?assertNot(kitsune:clone_exists("/tmp", "foobar")),
    ok.

% Test the combination of git_clone/2 and git_fetch/2 so we avoid cloning the
% same repository twice.
git_clone_test(Config) ->
    %
    % Test cloning a repository...
    %
    PrivDir = ?config(priv_dir, Config),
    RepoUrl = "https://github.com/nlfiedler/wivrr.git",
    ok = kitsune:git_clone(PrivDir, RepoUrl),
    RepoName = "wivrr",
    ?assert(kitsune:clone_exists(PrivDir, RepoName)),
    %
    % Now update the cloned repository; hard to test that anything really
    % happened without first pushing new changes to the remote, which is ugly.
    % But at least we know it didn't explode.
    %
    ok = kitsune:git_fetch(PrivDir, RepoName),
    ?assert(kitsune:clone_exists(PrivDir, RepoName)),
    ok.

% Test the fork/join functionality.
parallel_test(_Config) ->
    poolboy:start_link([{name, {local, worker_test}},
                        {worker_module, kitsune_test_worker},
                        {size, 8}, {max_overflow, 0}]),
    % While we cannot know which inputs failed, we can at least regain
    % control and hope they magically work the next time around. Not in
    % this test, of course, but in the actual application.
    Inputs = [good, good, good, bad, good, bad, good, bad, good, bad, bad, good],
    ok = kitsune:parallel(worker_test, Inputs),
    ok.

% Test the processing of repositories functionality.
process_repos_test(Config) ->
    PrivDir = ?config(priv_dir, Config),
    ok = application:set_env(kitsune, destination, PrivDir),
    ok = application:set_env(kitsune, username, "nlfiedler"),
    ok = gen_server:call(kitsune_srv, begin_backup, infinity),
    % Do not test for wivrr, since earlier tests already created it, but test
    % for a few repos that are likely to always exist. In fact, having a clone
    % that already exists is a good test.
    ?assert(kitsune:clone_exists(PrivDir, "kitsune")),
    ?assert(kitsune:clone_exists(PrivDir, "jswat")),
    ?assert(kitsune:clone_exists(PrivDir, "replicaz")),
    ?assert(kitsune:clone_exists(PrivDir, "tanuki")),
    ?assert(kitsune:clone_exists(PrivDir, "akashita")),
    ?assert(kitsune:clone_exists(PrivDir, "provisioning")),
    ok.
