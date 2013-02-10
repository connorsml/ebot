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

-module(ebot_urls_buffer_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, buffer_processor/1, add_new_url/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

-record(state, {db_connect}).

-define(SITES_META_BACKET, <<"mo_ads_crawling_sites_meta">>).
-define(HOUR, 60 * 60).
% -define(HOUR, 60).

% TODO: Move to config.
-define(RIAK_HOST, "127.0.0.1").
-define(RIAK_PORT, 8087).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  % TODO: move to config
  {ok, Db} = riakc_pb_socket:start_link(?RIAK_HOST, ?RIAK_PORT),
  spawn_link(?MODULE, buffer_processor, [Db]),
  {ok, #state{db_connect=Db}}.


add_new_url(Url) ->
  gen_server:cast(?MODULE, {add_new_url, Url}).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({add_new_url, Url}, State) ->
  ebot_mq:send_input_url({Url, empty}),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, State) ->
  riakc_pb_socket:stop(State#state.db_connect),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


buffer_processor(Db) ->
  case ebot_crawler:workers_status() of
    started ->
        case ebot_mq:receive_input_url() of
          {ok, Url} ->
              handle_input_url(Db, Url);
              % timer:sleep( 3000 );
          {error, _} ->
              error_logger:info_report({?MODULE, ?LINE, {buffer_processor, no_input_urls}}),
              {ok, Sleep} = ebot_util:get_env(workers_sleep_time),    
              timer:sleep( Sleep )
        end,
        buffer_processor(Db);

    stopped ->
        error_logger:warning_report({?MODULE, ?LINE, {stopping_worker_buffer_processor}})
  end.


lookup_site_crawling_info(Db, SiteId) -> 
  case riakc_pb_socket:get(Db, ?SITES_META_BACKET, SiteId) of
    {ok, Object} -> 
        {Object, binary_to_term(riakc_obj:get_value(Object))};
    {error, notfound} -> 
       {empty, dict:new()}
  end.


handle_input_url(Db, {Url, Extra}) ->
  SiteId = ebot_db_doc_site:site_id_for_url(Url),
  
  {Object, SiteDoc} = lookup_site_crawling_info(Db, SiteId),

  case time_to_add_url_for_site(Db, Object, SiteDoc, SiteId, Url) of
    true -> 
       % continue working as usual
       error_logger:info_report({?MODULE, ?LINE, {handle_input_url, Url, sent_to_queue}}),
       ebot_mq:send_url_new({Url, Extra});
    false -> 
       % move back to queue
       % error_logger:info_report({?MODULE, ?LINE, {handle_input_url, threshold_reached_for, Url, back_to_queue}}),
       ebot_mq:send_input_url({Url, Extra})
  end.


time_to_add_url_for_site(Db, Object, SiteDoc, SiteId, Url) -> 

  % error_logger:error_report({?MODULE, ?LINE, {time_to_add_url_for_site, site, SiteId, SiteDoc1}}),

  LastUrlTime = ebot_db_doc:doc_get_value(<<"last_url_time">>, SiteDoc),
  UrlCount = ebot_db_doc:doc_get_value(<<"urls_count">>, SiteDoc),

  {Result, SiteDoc1} = case urls_threshold_reached(LastUrlTime, UrlCount, Url) of

    true -> 
      {false, empty};

    false -> 
      {ok, SiteDoc2} = ebot_db_doc:update_doc(SiteDoc, [{update_counter, <<"urls_count">>}]),
      {true, SiteDoc2};

    {ok, hour_passed} ->
      {ok, SiteDoc2} = ebot_db_doc:update_doc(SiteDoc, [
                                                        {set_counter, <<"urls_count">>, 1}, 
                                                        {update_timestamp_msec, <<"last_url_time">>}
                                                        ]),
      % error_logger:info_report({?MODULE, ?LINE, {urls_threshold_reached_hour_passed}}),
      {true, SiteDoc2}
  end,

  case SiteDoc1 of
     
     empty -> pass;

     Doc ->
        SiteDoc3 = term_to_binary(Doc, [compressed]),
        
        Object1 = case Object of
          empty -> 
            riakc_obj:new(?SITES_META_BACKET, SiteId, SiteDoc3);
          Obj ->
            riakc_obj:update_value(Obj, SiteDoc3)
        end,

        case riakc_pb_socket:put(Db, Object1) of
          ok ->
              error_logger:info_report({?MODULE, ?LINE, {time_to_add_url_for_site, SiteId, ok}});
          Else ->
              error_logger:error_report({?MODULE, ?LINE, {time_to_add_url_for_site, SiteId, error, Else}})
        end
  end,

  Result.

urls_threshold_reached(LastUrlTime, UrlsCount, Url) ->
  {Mega, Seconds, MSeconds} = erlang:now(),
  Now = Mega * 1000000 + Seconds + MSeconds/1000000, % Linux timestamp
  Delta = Now - LastUrlTime,
  if 
    Delta == 0 ->
       % error_logger:info_report({?MODULE, ?LINE, {urls_threshold_reached, zero_delta, url, Url}}),
       false;
    
    Delta >= ?HOUR ->
       error_logger:info_report({?MODULE, ?LINE, {urls_threshold_reached, hour_passed, delta, Delta, url, Url}}),
       {ok, hour_passed};

    true ->
       {ok, UrlsPerHourForSite} = ebot_util:get_env(urls_per_hour_for_site),
       Result = UrlsCount >= UrlsPerHourForSite,
       case Result of
         false ->
           error_logger:info_report({?MODULE, ?LINE, {urls_threshold_reached_false, urls_count, UrlsCount, 
                                      urs_per_site, UrlsPerHourForSite, url, Url, res, Result}});
         _Else -> pass
      end,
      Result
  end.

  