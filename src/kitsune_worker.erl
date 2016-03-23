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
%% A worker pool worker process, does the fetch/clone.
%%
-module(kitsune_worker).
-behavior(gen_server).
-behavior(poolboy_worker).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% Worker API
%%
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%
%% gen_server callbacks
%%
init([]) ->
    {ok, undefined}.

handle_call({process, Name, Url, BaseDir}, _From, State) ->
    case kitsune:clone_exists(BaseDir, Name) of
        true ->
            ok = kitsune:git_fetch(BaseDir, Name),
            lager:info("updated repository ~s", [Name]);
        false ->
            ok = kitsune:git_clone(BaseDir, Url),
            lager:info("cloned repository ~s", [Name])
    end,
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    lager:notice("unexpected message: ~w", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
