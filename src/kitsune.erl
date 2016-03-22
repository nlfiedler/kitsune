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
-export([fetch_repos/1, timer_value/2, clone_exists/2, git_clone/2, git_fetch/2]).
-export([parallel/2]).

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
% Return the result as a property list with the names as keys, and values
% are the clone URLs.
extract_repo_info(Repos) ->
    GetNameAndUrl = fun(Elem) ->
        Name = binary_to_list(proplists:get_value(<<"name">>, Elem)),
        Url = binary_to_list(proplists:get_value(<<"clone_url">>, Elem)),
        {Name, Url}
    end,
    % For some reason the proplists are inside a tuple of 1, hence the {Repo}.
    [GetNameAndUrl(Repo) || {Repo} <- Repos].

% Test if a bare git clone exists at the given location, for the named
% repository. Returns true or false.
clone_exists(BaseDir, RepoName) ->
    DirName = RepoName ++ ".git",
    HeadName = filename:join([BaseDir, DirName, "HEAD"]),
    case file:read_file_info(HeadName) of
        {ok, _FileInfo} -> true;
        {error, enoent} -> false
        % if any other error, let it crash
    end.

% Create a bare clone of the repository identified by the given URL, storing it
% within the named directory. Will raise an error if the git executable is not
% found in the path.
git_clone(BaseDir, RepoUrl) ->
    GitBin = ensure_git_in_path(),
    Args = ["clone", "--quiet", "--mirror", RepoUrl],
    Port = erlang:open_port({spawn_executable, GitBin},
        [exit_status, {args, Args}, {cd, BaseDir}]),
    {ok, 0} = wait_for_port(Port),
    ok.

% Update the existing git mirror with the latest commits from upstream.
git_fetch(BaseDir, RepoName) ->
    GitBin = ensure_git_in_path(),
    true = clone_exists(BaseDir, RepoName),
    RepoDir = filename:join(BaseDir, RepoName ++ ".git"),
    % The combination of 'git clone --mirror' and 'git fetch --prune' should
    % remove any tags that were removed from the remote repository.
    Args = ["fetch", "--quiet", "--prune", "--all"],
    Port = erlang:open_port({spawn_executable, GitBin},
        [exit_status, {args, Args}, {cd, RepoDir}]),
    {ok, 0} = wait_for_port(Port),
    ok.

% Return the milliseconds for the given period and frequency. For instance, a
% period of 'hourly' and frequency of 12 yields 43,200,000 milliseconds.
timer_value(hourly, Frequency) when is_integer(Frequency) ->
    3600 * 1000 * Frequency;
timer_value(daily, Frequency) when is_integer(Frequency) ->
    86400 * 1000 * Frequency;
timer_value(weekly, Frequency) when is_integer(Frequency) ->
    86400 * 1000 * 7 * Frequency.

% Ensure the git executable can be found in the path. Raises an error if it is
% missing, otherwise returns the full path to the executable.
ensure_git_in_path() ->
    case os:find_executable("git") of
        false ->
            lager:error("cannot find 'git' executable"),
            error(git_not_found);
        GitBin -> GitBin
    end.

% Wait for the given Port to complete and return the exit code in the form
% of {ok, Status}. Any output received is written to the log. If the port
% experiences an error, returns {error, Reason}.
wait_for_port(Port) ->
    wait_for_port(Port, false).

% Wait for the given Port to complete and return the exit code in the form
% of {ok, Status}. Any output received is written to the log. If the port
% experiences an error, returns {error, Reason}. If Quiet is true, output
% from the port is ignored.
wait_for_port(Port, Quiet) when is_boolean(Quiet) ->
    receive
        {Port, {exit_status, Status}} ->
            ensure_port_closed(Port),
            {ok, Status};
        {Port, {data, Data}} ->
            if Quiet -> lager:notice("output from port ignored...");
                true -> lager:notice("received output from port: ~s", [Data])
            end,
            wait_for_port(Port, Quiet);
        {'EXIT', Port, Reason} ->
            lager:info("port ~w exited, ~w", [Port, Reason]),
            {error, Reason}
    end.

% Ensure that the given Port has been properly closed. Does nothing if the
% port is not open.
ensure_port_closed(Port) ->
    case erlang:port_info(Port) of
        undefined -> ok;
        _         -> erlang:port_close(Port)
    end.

% Spawn processes to invoke the given Function, each taking a single
% element from the list of Tasks. Waits for all of the processes to
% complete and returns a tuple of the accumlated results and the data
% elements that failed to be processed.
parallel(Function, Tasks) ->
    join(fork(Function, Tasks), [], []).

% Perform the fork in the fork/join algorithm. Returns a list of pids of
% the spawned processes.
fork(Function, Tasks) ->
    Parent = self(),
    Invoker = fun(Task) -> spawn(fun() -> invoke(Parent, Function, Task) end) end,
    lists:map(Invoker, Tasks).

% Perform the join in the fork/join algorithm. Returns a tuple of the
% accumulated results (in order of completion) and the data elements that
% produced errors.
join([], Results, Failed) -> {Results, Failed};
join(Pids, Results, Failed) ->
    receive
        {From, {error, Data}} -> join(lists:delete(From, Pids), Results, [Data|Failed]);
        {From, Result} -> join(lists:delete(From, Pids), [Result|Results], Failed)
    end.

% Invoke the function with the given data.
invoke(Receiver, Function, Data) ->
    try Function(Data) of
        Result -> Receiver ! {self(), Result}
    catch
        error:Error ->
            lager:error("Function(~w) caused error ~p", [Data, Error]),
            Receiver ! {self(), {error, Data}}
    end.
