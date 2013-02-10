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


-module(ebot_events_guard).

-export([start_link/0]).

-export([init/1, handle_info/2]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
  add_handler(ebot_indexer_manager),
  add_handler(ebot_url_action_manager),  
  {ok, []}.

add_handler(HandlerModule) ->
  gen_event:add_sup_handler(ebot_event_manager, HandlerModule,[]).

handle_info({gen_event_EXIT, HandlerModule, Reason}, State) ->
  %% gen_event manager sends this message if a handler was added using
  %% gen_event:add_sup_handler/3 or gen_event:swap_sup_handler/3 functions
  % io:format("~w: detected handler ~p shutdown:~n~p~n",
  %           [?MODULE, HandlerModule, Reason]),
  error_logger:info_report({?MODULE, ?LINE, {handle_info, HandlerModule, Reason}}),  
  add_handler(HandlerModule),
  {noreply, State};

handle_info(Info, State) ->
  %% This process should not receive other messages
  % io:format("~w: received unknown message:~n~p~n", [?MODULE, Other]),
  error_logger:info_report({?MODULE, ?LINE, {handle_info, unexpected, Info}}),  
  {noreply, State}.