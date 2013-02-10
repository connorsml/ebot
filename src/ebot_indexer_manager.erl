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


-module(ebot_indexer_manager).

-behaviour(gen_event).

-define(SERVER, ?MODULE).


%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).


%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init([]) ->
  {ok, []}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
  {ok, reply, State}.



%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
  error_logger:info_report({?MODULE, ?LINE, {handle_info, Info}}),
  {ok, State}.


%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description:Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


handle_event({index_url, Url, BodyTokens, Body, Headers, PassNumber}, State) ->
  index_url_data(Url, BodyTokens, Body, Headers, PassNumber, 0),
  {ok, State};

handle_event(_Msg, State) ->
  % error_logger:info_report({?MODULE, ?LINE, {handle_event_pass, msg, Msg}}),
  {ok, State}.

index_url_data(Url, BodyTokens, Body, Headers, PassNumber, _IndexPass) ->
  case catch ebot_elastic_search_indexer:index(Url, BodyTokens, Body, Headers, PassNumber) of
     {error, Reason} -> 
       error_logger:info_report({?MODULE, ?LINE, {index_url_failed, url, Url, reason, Reason}});
     {'EXIT', {ucs,{bad_utf8_character_code}} } -> 
       error_logger:info_report({?MODULE, ?LINE, {index_url_failed, url, Url, reason, bad_utf8_character_code}});
     %     
        % if 
        %   IndexPass == 0 ->
        %        % error_logger:info_report({?MODULE, ?LINE, {index_url_failed, exit_reason, Reason}});
        %        case ebot_html_util:try_decode_bad_html_to_utf8(Url, Body, Headers) of
        %           {ok, Data} -> 
        %               % error_logger:info_report({?MODULE, ?LINE, {index_url_failed, url, Url, converted_from_bad_encoding}}),
        %               index_url_data(Url, BodyTokens, Data, Headers, PassNumber, 1);
        %           {error, not_converted} ->
        %               error_logger:info_report({?MODULE, ?LINE, {index_url_failed, url, Url, not_converted_to_utf8_1}}),
        %               {error, not_converted}
        %        end;

        %   true -> 
        %       error_logger:info_report({?MODULE, ?LINE, {index_url_failed, url, Url, not_converted_to_utf8_2}}),
        %       {error, not_converted}

        % end;
      
      ok -> pass
  end.