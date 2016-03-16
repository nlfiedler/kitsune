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
%% Primary driver of the backup procedure.
%%
-module(kitsune_srv).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {timer}).

%%
%% Client API
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% gen_server callbacks
%%
init([]) ->
    {ok, TRef} = fire_later(),
    State = #state{timer=TRef},
    {ok, State}.

handle_call(begin_backup, _From, State) ->
    % cancel the current timer, if any
    case State#state.timer of
        undefined -> ok;
        TRef -> {ok, cancel} = timer:cancel(TRef)
    end,
    NewState = process_repos(State),
    {reply, ok, NewState}.

handle_cast(process, State) ->
    NewState = process_repos(State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    lager:notice("unexpected message: ~w", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Private functions
%%

% Process the repositories, then set up the next timer.
process_repos(_State) ->
    {ok, Username} = application:get_env(kitsune, username),
    {ok, BaseDir} = application:get_env(kitsune, destination),
    {ok, AllRepos} = kitsune:fetch_repos(Username),
    IsLocal = fun({Name, _Url}) ->
        kitsune:clone_exists(BaseDir, Name)
    end,
    {Locals, Remotes} = lists:partition(IsLocal, AllRepos),
    % TODO: consider having a pool of processes to parallelize the work
    %       https://github.com/inaka/worker_pool
    Fetcher = fun({Name, _Url}) ->
        ok = kitsune:git_fetch(BaseDir, Name)
    end,
    ok = lists:foreach(Fetcher, Locals),
    Cloner = fun({_Name, Url}) ->
        ok = kitsune:git_clone(BaseDir, Url)
    end,
    ok = lists:foreach(Cloner, Remotes),
    {ok, TRef} = fire_later(),
    #state{timer=TRef}.

% Start a timer to cast a 'process' message to us at the next backup time.
fire_later() ->
    M = gen_server,
    F = cast,
    A = [kitsune_srv, process],
    {ok, Period} = application:get_env(kitsune, period),
    {ok, Frequency} = application:get_env(kitsune, frequency),
    timer:apply_after(kitsune:timer_value(Period, Frequency), M, F, A).
