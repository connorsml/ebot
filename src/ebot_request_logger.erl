%% EBOT, an erlang web crawler.
%% Copyright (C) 2012 ~ questeug@gmail.com
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
%% @author <questeug@gmail.com>
%% @copyright 2012 <questeug@gmail.com>.


-module(ebot_request_logger).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/0, log/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).


-record(logger_state,
   {
     fd
    }
  ).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
  {ok, LogFileName} = ebot_util:get_env(access_log_file), 
  {ok, FD} = file:open(LogFileName, [write, raw, append]),
  {ok, #logger_state{fd=FD}}.


log(Url, Result) ->
  gen_server:cast(?MODULE, {log, Url, Result}).


handle_cast({log, Url, {error, not_found}}, State) ->
  log_access(Url, 404, [], <<"">>, State#logger_state.fd),
  {noreply, State};

handle_cast({log, Url, {ok, {{_, Code, _}, Headers, Content}}}, State) ->
  log_access(Url, Code, Headers, Content, State#logger_state.fd),
  {noreply, State};

handle_cast({log, Url, {error, Reason}}, State) ->
  Msg = ebot_util:now_as_utc() ++ " " ++ 
        ebot_util:safe_binary_to_list(Url) ++ " "  ++ 
        ebot_util:parse_analyze_error(Reason) ++ "\n",
  report_message(State#logger_state.fd, Msg),
  {noreply, State};

handle_cast({log, Url, Result}, State) ->
  error_logger:info_report({?MODULE, ?LINE, {access_log, url, Url, unknown_result, Result}}),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

report_message(Fd, Msg) ->
  file:write(Fd, Msg).

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    file:close(State#logger_state.fd),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

log_access(Url, Code, Headers, Content, Fd) ->
  Msg = ebot_util:now_as_utc() ++ " " ++ ebot_util:safe_binary_to_list(Url) ++ " "  ++ integer_to_list(Code),
  Length1 = case lists:keyfind("content-length", 1, Headers) of
    false -> ebot_util:content_length(Content, as_list);
    {_, Length} -> Length
  end,
  report_message(Fd, Msg ++ " " ++ Length1  ++ "\n").
