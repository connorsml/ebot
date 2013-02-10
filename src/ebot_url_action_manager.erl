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

-module(ebot_url_action_manager).

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
%% Function:
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event.
%%--------------------------------------------------------------------
handle_event({url_action, <<"new">>, {Url, _}}, State) ->
  % error_logger:info_report({?MODULE, ?LINE, {handle_event, "NEW", Url}}),
  case ebot_url_util:is_site_root(Url) of
    true ->
      site_crawling_started(Url),
      error_logger:info_report({?MODULE, ?LINE, {handle_event, "NEW", Url, is_site_root}});
     _ -> 
      increment_url_counter(new, url, Url),
      error_logger:info_report({?MODULE, ?LINE, {handle_event, "NEW", Url, increment_counter}})
  end,
  {ok, State};

handle_event({url_action, <<"fetched">>, {Url, _UrlData}}, State) ->
  error_logger:info_report({?MODULE, ?LINE, {handle_event, "FETCHED", Url}}),
  increment_url_counter(fetched, url, Url),
  {ok, State};

handle_event({url_action, <<"processed">>, {Url, _UrlData}}, State) ->
  error_logger:info_report({?MODULE, ?LINE, {handle_event, "PROCESSED", Url}}),
  {ok, _Doc} = increment_url_counter(processed, url, Url),
  % cos sites links are handled by chunks and another strategy required to check if the crawling finished.
  % check_if_site_crawling_finished(Url, Doc), 
  {ok, State};

handle_event({url_action, <<"refused">>, {Url, _}}, State) ->
  error_logger:info_report({?MODULE, ?LINE, {handle_event, "REFUSED", Url}}),
  {ok, Doc} = increment_url_counter(refused, url, Url),
  check_if_site_crawling_finished(Url, Doc),  
  {ok, State};


handle_event(_Event, State) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1,
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% Description: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event
%% handler to handle the request.
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% Description: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%%--------------------------------------------------------------------
handle_info(Info, State) ->
  error_logger:info_report({?MODULE, ?LINE, {info, Info}}),
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

site_crawling_started(Url) ->
 
  SiteID = ebot_db_doc_site:site_id_for_url(Url),

  {ok, Doc} = ebot_db:open_or_create_site(SiteID), % calls create_doc

  PassNumber = ebot_db_doc:doc_get_value(<<"pass_number">>, Doc),

  delete_passed_urls(SiteID, PassNumber),
  
  mark_urls_as_unindexed(SiteID),

  % % increment ebot_visits_count for Url, so it won't be considered new during this pass
  ebot_db:open_or_create_url(Url),
  ebot_db:update_doc(Url, [{set_counter, <<"ebot_visits_count">>, PassNumber + 1}]),
  
  {ok, Doc1} = ebot_db_doc:update_doc(Doc, 
                         [{update_value, <<"url">>, SiteID},
                          {set_counter, <<"new_links_count">>, 1},
                          {set_counter, <<"fetched_links_count">>, 0},
                          {set_counter, <<"processed_links_count">>, 0},
                          {set_counter, <<"refused_links_count">>, 0},
                          {update_counter, <<"pass_number">>},
                          {update_timestamp, <<"crawling_started_at">>},
                          {update_value, <<"crawling_finished_at">>, 0},
                          {update_value, <<"robots_txt">>, ebot_robots_support:robots_source(SiteID)}
                         ]),
  
  error_logger:info_report({?MODULE, ?LINE, {site_crawling_started, SiteID, Doc1}}),
  save_site_doc(SiteID, Doc1),
  notify_site_crawling(start, SiteID).

notify_site_crawling(Action, Site) ->
  {ok, ListenerUrl} = ebot_util:get_env(site_events_listener_url),

  Query = "{\"event\":\"" ++ atom_to_list(Action) ++"\",\"url\":\"" ++ ebot_util:safe_binary_to_list(Site) ++ "\"}",
  Request  = {ListenerUrl, [], "application/json", Query},

  case httpc:request(post, Request, [], []) of 

    {Status, {{_Status1, Code, _Status2}, _, _Body}} ->
       error_logger:info_report({?MODULE, ?LINE, {notify_site_crawling, Action, Site, Status, Code}});

    {error, Reason} ->
       error_logger:info_report({?MODULE, ?LINE, {notify_site_crawling, error, Reason, action, Action}})
  end.
  


increment_url_counter(Type, url, Url) ->
  {ok, Doc} = ebot_db:open_or_create_site(Url), % calls create_doc
  store_increment_counter(Type, Doc);

increment_url_counter(Type, doc, SiteDoc) ->
  store_increment_counter(Type, SiteDoc).  


store_increment_counter(Type, Doc) ->
  Counter = list_to_binary(atom_to_list(Type) ++ "_links_count"),
  {ok, Doc1} = ebot_db_doc:update_doc(Doc, [{update_counter, Counter}]),
  save_site_doc(ebot_db_doc_site:get_doc_site_id(Doc), Doc1),
  {ok, Doc1}.


check_if_site_crawling_finished(_Url, Doc) -> 
  
  case dict:find(<<"crawling_started_at">>, Doc) of
      error -> pass;
      {ok, _} ->

            New = dict:fetch(<<"new_links_count">>, Doc),
            Fetched = dict:fetch(<<"fetched_links_count">>, Doc),
            Processed = dict:fetch(<<"processed_links_count">>, Doc),
            Refused = dict:fetch(<<"refused_links_count">>, Doc),
            Finished = dict:fetch(<<"crawling_finished_at">>, Doc),
            SiteUrl = dict:fetch(<<"url">>, Doc),
            error_logger:info_report({?MODULE, ?LINE, {checking, SiteUrl, new, New, fetched, Fetched, processed, Processed, refused, Refused, finished, Finished}}),  
            
            if
             Finished == 0 orelse Finished == <<"0">> ->
                CrawlingFinished = New > 0 andalso New == Fetched andalso Fetched == Processed + Refused,
                error_logger:info_report({?MODULE, ?LINE, {crawling_finished, SiteUrl, new, New, fetched, Fetched, processed, Processed, refused, Refused, CrawlingFinished}}),
                case CrawlingFinished of
                  true ->
                     SiteID = ebot_db_doc_site:get_doc_site_id(Doc), % TODO: pass thru func params.

                     {ok, Doc1} = ebot_db_doc:update_doc(Doc, [{update_timestamp, <<"crawling_finished_at">>}]),
                     save_site_doc(SiteID, Doc1),
                    
                     delete_passed_urls(SiteID, ebot_db_doc:doc_get_value(<<"pass_number">>, Doc)),

                     gen_server:cast(ebot_web, {site_crawling_finished, SiteID}),

                     notify_site_crawling(finish, SiteID),
                     ok;
                  false ->
                     false
                end;
              true ->
                 pass
            end
  end.

save_site_doc(SiteID, Doc) ->
  ebot_db:save_site_doc(SiteID, Doc).

delete_passed_urls(Site, PassNumber) when PassNumber > 0 ->
  ebot_db:delete_passed_urls(Site, PassNumber);

delete_passed_urls(_Site, _PassNumber) -> 
  ok.

mark_urls_as_unindexed(SiteID) ->
  ebot_db:mark_urls_as_unindexed(SiteID).

%%====================================================================
%% EUNIT TESTS
%%====================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

ebot_url_action_manager_test() -> ok.

-endif.