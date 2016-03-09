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
%% The low-level, easily testable functions.
%%
-module(kitsune).
-export([fetch_repos/1]).

% Retrieve the repositories for the given Username. Returns a property list
% consisting of repository names as keys, with clone URLs as values.
fetch_repos(Username) ->
    Url = io_lib:format("https://api.github.com/users/~s/repos", [Username]),
    ReqHeaders = [{"User-Agent", "kitsune"}],
    fetch_repos(Url, ReqHeaders, []).

% Retrieve the repositories from the given Url, with the provided request
% headers, and previously acquired results, if any.
fetch_repos(Url, ReqHeaders, Acc) ->
    {ok, _Status, Headers, Client} = hackney:request(get, Url, ReqHeaders),
    {ok, Body} = hackney:body(Client),
    Repos = jiffy:decode(Body),
    Results = Acc ++ extract_repo_info(Repos),
    % Read the Link header to know if there are more results to fetch.
    % Link: <https://api.github.com/search/code?q=addClass+user%3Amozilla&page=2>; rel="next",
    %   <https://api.github.com/search/code?q=addClass+user%3Amozilla&page=34>; rel="last"
    case proplists:get_value(<<"Link">>, Headers) of
        undefined ->
            {ok, Results};
        LinksBin ->
            % Split the links and look for a rel="next" entry.
            Links = re:split(binary_to_list(LinksBin), ", ", [{return,list}]),
            LinkFilter = fun(Elem) ->
                string:str(Elem, "; rel=\"next\"") > 0
            end,
            case lists:filter(LinkFilter, Links) of
                [] -> {ok, Results};
                [NextLinkEntry] ->
                    % Extract the actual link from the header value.
                    {match, [_W, {Off, Len}]} = re:run(NextLinkEntry, "<(.+)>; rel=\"next\""),
                    NextLink = string:substr(NextLinkEntry, Off + 1, Len),
                    % Recursively fetch the next page of results.
                    fetch_repos(NextLink, ReqHeaders, Results)
            end
    end.

% Extract the repository names and clone URLs from the given decoded JSON
% response (a list of 1-tuples, each containing a list of properties).
% Return the result a a property list with the names as keys, and values
% are the clone URLs.
extract_repo_info(Repos) ->
    GetNameAndUrl = fun(Elem) ->
        Name = binary_to_list(proplists:get_value(<<"name">>, Elem)),
        Url = binary_to_list(proplists:get_value(<<"clone_url">>, Elem)),
        {Name, Url}
    end,
    % For some reason the proplists are inside a tuple of 1, hence the {Repo}.
    [GetNameAndUrl(Repo) || {Repo} <- Repos].

% TODO: function to test if a bare clone exists for a named repo, at a given destination
% TODO: function to update the already existing repo
% TODO: function to clone a repo to a destination
% TODO: function to return millis for next timer, given period and frequency
%       `hourly` -> 3600 * 1000 * N
%       `daily` -> 86400 * 1000 * N
%       `weekly` -> 86400 * 1000 * 7 * N
