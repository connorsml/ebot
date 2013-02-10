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


-module(ebot_event_manager).

-behavior(gen_event).

%% API
-export([start_link/0, 
         add_handler/1, 
         notify/1,
         sync_notify/1]).

%% gen_event callbacks
-export([init/1, handle_call/2, handle_event/2, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | {error,Error}
%% Description: Creates an event manager.
%%--------------------------------------------------------------------
start_link() ->
  gen_event:start_link({local, ?SERVER}).

init([]) ->
  {ok, []}.

%%--------------------------------------------------------------------
%% Function: add_handler(Module) -> ok | {'EXIT',Reason} | term()
%% Description: Adds an event handler
%%--------------------------------------------------------------------
add_handler(Module) ->
  gen_event:add_handler(?SERVER, Module, []).


%%--------------------------------------------------------------------
%% Function: add_handler(Module) -> ok | {'EXIT',Reason} | term()
%% Description: Adds an event handler
%%--------------------------------------------------------------------
add_sup_handler(Module) ->
  gen_event:add_sup_handler(?SERVER, Module, []).


%%--------------------------------------------------------------------
%% Function: notify(Event) -> ok | {error, Reason}
%% Description: Sends the Event through the event manager.
%%--------------------------------------------------------------------
notify(Event) ->
  gen_event:notify(?SERVER, Event).


%%--------------------------------------------------------------------
%% Function: sync_notify(Event) -> ok | {error, Reason}
%% Description: Sends the Event through the event manager.
%%--------------------------------------------------------------------
sync_notify(Event) ->
  gen_event:sync_notify(?SERVER, Event).


handle_info({gen_event_EXIT, HandlerModule, Reason}, State) ->
    %% gen_event manager sends this message if a handler was added using
    %% gen_event:add_sup_handler/3 or gen_event:swap_sup_handler/3 functions
    % io:format("~w: detected handler ~p shutdown:~n~p~n",
    %           [?MODULE, HandlerModule, Reason]),
    error_logger:info_report({?MODULE, ?LINE, {handle_info_gen_event_EXIT, HandlerModule, Reason}}),
    add_sup_handler(HandlerModule),
    % {ok, {handler_died, HandlerModule, Reason},HandlerModule};
    {ok, State};


handle_info(Other, State) ->
    %% This process should not receive other messages
    % io:format("~w: received unknown message:~n~p~n", [?MODULE, Other]),
    error_logger:info_report({?MODULE, ?LINE, {handle_info, Other}}),
    {ok, State}.


%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_cast(_Event, State) ->
    {noreplay, State}.


terminate(_Reason, _State) ->
    ok.   


handle_call(_Msg, State) ->
  {ok, ok, State}.


handle_event(_Event, State) ->
  {ok, State}.