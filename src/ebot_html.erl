%% EBOT, an erlang web crawler.
%% Copyright (C) 2010 ~ matteo DOT redaelli AT libero DOT it
%%                      http://www.redaelli.org/matteo/
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
%%%-------------------------------------------------------------------
%%% File    : ebot_html.erl
%%% Author  : matteo <<matteo.redaelli@@libero.it>
%%% Description : 
%%%
%%% Created :  19 Jun 2010 by matteo <matteo@redaelli.org>
%%%-------------------------------------------------------------------
-module(ebot_html).
-author("matteo.redaelli@libero.it").

-define(SERVER, ?MODULE).
-define(TIMEOUT, 5000).
-define(WORKER_TYPE, html).

-include("ebot.hrl").

-behaviour(gen_server).

%% API
-export([
     analyze_url/2,
     analyze_url_body_plugins/3,
     check_recover_workers/0,
     run/1,
     info/0,
     remove_worker/1,
     get_workers/0,
     start_workers/0,
     start_workers/2,
     start_link/0,
     statistics/0
    ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state,{
    workers
   }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{timeout,?TIMEOUT}]).
check_recover_workers() ->
    gen_server:call(?MODULE, {check_recover_workers}).
info() ->
    gen_server:call(?MODULE, {info}).
get_workers() ->
    gen_server:call(?MODULE, {get_workers}).
start_workers() ->
    gen_server:cast(?MODULE, {start_workers}).
start_workers(Depth, Tot) ->
    gen_server:cast(?MODULE, {start_workers, Depth, Tot}).
statistics() ->
    gen_server:call(?MODULE, {statistics}).
remove_worker(Worker) ->
    gen_server:cast(?MODULE, {remove_worker, Worker}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    case ebot_util:get_env(start_workers_at_boot) of
    {ok, true} ->
        State = #state{ workers = start_workers(ebot_worker_util:create_workers(?WORKER_TYPE)) };
    {ok, false} ->
        State = #state{}
    end,
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({check_recover_workers}, _From, State) ->
    Workers = ebot_worker_util:check_recover_workers(State#state.workers),
    NewState = State#state{
         workers = Workers
        },
    {reply, Workers, NewState};

handle_call({info}, _From, State) ->
    {reply, ok, State};

handle_call({statistics}, _From, State) ->
    Reply = ebot_worker_util:statistics(State#state.workers),
    {reply, Reply, State};

handle_call({get_workers}, _From, State) ->
    {?WORKER_TYPE, Reply} = State#state.workers,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({remove_worker, Worker}, State) ->
    NewState = State#state{
         workers = ebot_worker_util:remove_worker(Worker, 
                              State#state.workers)
        },
    {noreply, NewState};

handle_cast({start_workers}, State) ->
    NewState = start_workers(State),
    {noreply, NewState};

handle_cast({start_workers, Depth, Tot}, State) ->
    NewState = State#state{
         workers = ebot_worker_util:start_workers(Depth,Tot, 
                              State#state.workers)
        },
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', From, Reason}, State) ->
  error_logger:info_report({?MODULE, ?LINE, {exit_signal_received_from, From, with, Reason, restarting}}),
  NewState = State#state{ workers = ebot_worker_util:restart_worker(From, State#state.workers) },
  {noreply, NewState};

handle_info(Info, State) ->
  error_logger:info_report({?MODULE, ?LINE, {info, Info}}),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

run({Depth, Num}) ->
    case ebot_mq:receive_url_fetched(Depth) of
    {ok, {Url, Result}} ->
        analyze_url(Url, Result);
        % 
        % gen_server:cast(ebot_html, {analyze_url, {Url, Result}, self()}),
        % 
    {error, _} ->
        error_logger:info_report({?MODULE, ?LINE, {run, no_queued_urls, depth, Depth, num, Num, waiting}}),
        timer:sleep( ?EBOT_EMPTY_QUEUE_TIMEOUT )
    end,
    case ebot_crawler:workers_status() of
    started ->
        % No need to slow down creating of new urls
        {ok, Sleep} = ebot_util:get_env(workers_sleep_time),
        timer:sleep( Sleep ),
        run({Depth, Num});
    stopped ->
        error_logger:warning_report({?MODULE, ?LINE, {stopping_worker, self()}}),
        remove_worker( {Depth, self()} )
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

analyze_url(Url,{error, Reason}) ->
    error_logger:error_report({?MODULE, ?LINE, {analyze_url, Url, skipping_url, Reason}}),
    Options = [{update_counter, <<"ebot_errors_count">>},
           % {update_value, <<"ebot_head_error">>, list_to_binary(atom_to_list(Reason))}
           {update_value, <<"ebot_head_error">>, ebot_util:parse_analyze_error(Reason)}
          ],

    %% TODO: instead of only Url, it would be nice to send {Url, Reason}
    %% like <<"https://github.com/login/,https_through_proxy_is_not_currently_supported">>
    ebot_mq:send_url_refused({Url,Reason}),
    ebot_db:update_doc(Url, Options),
    {error, Reason};

analyze_url(Url,{ok, {Status, Headers, Body}}) ->
  {_, RetCode,_} = Status,
  case RetCode of
    200 ->
        try
          analyze_url_head(Url, {Status, Headers, empty}),
          analyze_url_body(Url, {Status, Headers, Body})
        after
          ebot_mq:send_url_processed({Url, {ok, {Status, Headers, Body}} })
        end;
    _Else ->
      ebot_mq:send_url_refused({Url,empty})
  end.


analyze_url_head(Url, Result = {_Status, _Headers, empty}) ->
    {ok, H} = ebot_util:get_env(tobe_saved_headers),
    Options = [{head, Result, H}, 
           {update_timestamp,<<"ebot_head_visited">>},
           % {update_counter, <<"ebot_visits_count">>},
           {update_value, <<"ebot_errors_count">>, 0}
          ],
    ebot_db:update_doc(Url, Options).

analyze_url_body(Url, {_Status, _Headers, empty}) ->
    error_logger:info_report({?MODULE, ?LINE, {analyze_url_body, Url, empty_body}});

analyze_url_body(Url, {_Status, Headers, Body}) ->
    Tokens = mochiweb_html:tokens(Body),
    % spawn(?MODULE, analyze_url_body_plugins, [Url, Tokens, Body]),
    analyze_url_body_plugins(Url, Tokens, Body),
    error_logger:info_report({?MODULE, ?LINE, {analyze_body_plugins, Url}}),
    Links = ebot_html_util:get_links_from_tokens(Tokens, Url),
    
    {ok, SiteDoc} = ebot_db:open_or_create_site(Url),
    PassNumber = ebot_db_doc:doc_get_value(<<"pass_number">>, SiteDoc),
    ebot_event_manager:sync_notify({index_url, Url, Tokens, Body, Headers, PassNumber}),
    analyze_url_body_links(Url, Links, SiteDoc).


analyze_url_body_links(Url, Links, SiteDoc) ->
    % ebot_url_util:filter_internal_links(Url, Links),
    % Preventing from going outside. There might be a programmatical switch point for choosing whether or not to restrict going outside.

    InternalLinks = ebot_url_util:filter_internal_links(Url, Links),
    error_logger:info_report({?MODULE, ?LINE, {analyze_url_body_links_internal_links, Url, input, Links, output, InternalLinks}}),

   % Go thru all links found.
   % normalizing Links
   % error_logger:info_report({?MODULE, ?LINE, {normalizing_links_of, Url}}),
    ClearedlLinks = ebot_url_util:clear_links(InternalLinks),

    % Removing final slashes from urls
    SlashlessLinks = lists:map(fun(Link) -> ebot_url_util:url_remove_final_slash(Link) end, ClearedlLinks),

   %  NormalizedLinks = lists:map(
            % fun(U) -> 
            %   {ok, ListNormalizeOptions} = ebot_util:get_env(normalize_url),
            %   {ok, NormalizeOptions} = get_env_normalize_url(Url, ListNormalizeOptions),
            %   ebot_url_util:normalize_url(U, NormalizeOptions)
            % end,
            % InternalLinks),
    % error_logger:info_report({?MODULE, ?LINE, {normalized_links_of, Url, NormalizedLinks}}),
    %% removing duplicates
    UniqueLinks = ebot_util:remove_duplicates(SlashlessLinks),

    % Prevent double analyze of Url.
    UniqueLinks1 = lists:subtract(UniqueLinks, [ebot_url_util:url_remove_final_slash(ebot_util:safe_binary_to_list(Url)),
                                                ebot_url_util:url_add_final_slash(ebot_util:safe_binary_to_list(Url))]), 

    Links1 = UniqueLinks1,
    LinksCount = length(Links1),
    
    % Filtering disallowed links
       
    %% TODO
    %% removing unwanted urls
    %%
    
    %% removing already visited urls and not valid
    % NotVisitedLinks = lists:filter(
    %         fun(U) -> 
    %             % (not ebot_crawler:is_visited_url(U)) andalso
    %             %     ebot_url_util:is_valid_url(U)
    %             (not ebot_crawler:is_visited_url(U))

    %         end,
    %         UniqueLinks),

    % error_logger:info_report({?MODULE, ?LINE, {not_visited_links, NotVisitedLinks}}), % custom
    % Links1 = NotVisitedLinks,

    SiteID = ebot_db_doc_site:get_doc_site_id(SiteDoc), 

    %% throw out disallowed links
    RobotsTxt = ebot_db_doc:doc_get_value(<<"robots_txt">>, SiteDoc),
    AllowedLinks = ebot_robots_support:retain_allowed_links(Url, RobotsTxt, Links1, ["*", "Mobot"]),
    error_logger:info_report({?MODULE, ?LINE, {allowed_links_for_url, Url, of_site, SiteID, AllowedLinks}}), 

    %% checking if urls threshold has been reached for the url's site.
    NewLinks = ebot_db_doc:doc_get_value(<<"new_links_count">>, SiteDoc),

    Links2 = filter_by_threshold(SiteID, NewLinks, AllowedLinks, Url),

    error_logger:info_report({?MODULE, ?LINE, {links_to_process_for_url, Url, of_site, SiteID, Links2}}),

    update_url_doc(Links2, SiteDoc, Url),

    %% UPDATE ebot-body-visited
    Options = [{update_timestamp, <<"ebot_body_visited">>},
               {update_value, <<"ebot_links_count">>, LinksCount}],

    ebot_db:update_doc(Url, Options),
    ok.

add_new_url(U, Url, PassNumber, PutIntoQueue) ->
    error_logger:info_report({?MODULE, ?LINE, {adding, U, from_referral, Url}}),

    case PutIntoQueue of 
      true -> ebot_crawler:add_new_url(U);
      false -> pass
    end,

    ebot_db:update_doc(U, [{set_counter, <<"ebot_visits_count">>, PassNumber}]),

    case  needed_update_url_referral(
        ebot_url_util:is_same_main_domain(Url, U),
        ebot_url_util:is_same_domain(Url, U)) of
    true ->       
        Options =   [{referral, Url}],
        ebot_db:update_doc(U, Options);
    false ->
        ok
    end.

update_url_doc(Links, SiteDoc, Referral) ->
  if 
    length(Links) > 0 ->
              lists:foreach(
                      fun(U) ->
                          %% creating the url in the database if it doesn't exist

                          U1 = ebot_url_util:escape_uri(U),

                          U2 = ebot_url_util:add_final_slash_if_site_root(U1),

                          % TODO: reference to the wrong module.
                          case ebot_web_util:is_url_text_html_mime(U2) of
                            {ok, {U3, Res}} when Res == true orelse Res == not_found ->

                                case ebot_db:open_or_create_url(U3) of 
                                  {ok, UrlDoc} ->

                                        % After ebot_crawler:add_new_url called with add_url=<url> site's pass counter has been updated
                                        % (rethinking needed for caching)
                                        PassNumber = ebot_db_doc:doc_get_value(<<"pass_number">>, SiteDoc),
                                        UrlVisits = ebot_db_doc:doc_get_value(<<"ebot_visits_count">>, UrlDoc),
                                        IsNew = PassNumber =/= UrlVisits,

                                        error_logger:info_report({?MODULE, ?LINE, {U3, s_pass, PassNumber, u_pass, UrlVisits, new, IsNew}}),

                                        case IsNew of
                                          true ->
                                              case Res of
                                                true -> 
                                                   add_new_url(U3, Referral, PassNumber, true);
                                                not_found -> 
                                                   add_new_url(U3, Referral, PassNumber, false),
                                                   ebot_db:update_doc(U3, [{update_value, <<"http_returncode">>, <<"404">>}]),
                                                   ebot_request_logger:log(U3, {error, not_found})
                                              end;

                                          false ->
                                             pass
                                        end;

                                   {error, invalid_statement} -> 
                                        error_logger:info_report({?MODULE, ?LINE, {U3, excluded, bad_encoded_url}})
                                  end;

                            {ok, {_, false}} -> 
                                pass;

                            {error, _} ->
                               pass
                          end
                      end,
                      Links);
    true ->
      pass
  end.

filter_by_threshold(SiteID, NewLinksCnt, Links, Url) ->
  case ebot_util:get_env(site_urls_threshold) of 
      {ok, LinksThreshold} ->
          if 
            NewLinksCnt > LinksThreshold -> % NewLinks > LinksThreshold / number of the workers parsing the site.
              error_logger:info_report({?MODULE, ?LINE, {analyze_url_body_links_internal_links, site_link_count_exceeded, SiteID, LinksThreshold}}),
              [];
            true ->
              Links1 = lists:sublist(Links, LinksThreshold - NewLinksCnt + 1),
              error_logger:info_report({?MODULE, ?LINE, {analyze_url_body_links_internal_links, Url, Links1}}), 
              Links1
          end;
      undefined ->
        Links
  end.

analyze_url_body_plugins(Url, Tokens, Body) ->
    error_logger:info_report({?MODULE, ?LINE, {analyze_url_body_plugins, Url}}),
    lists:foreach(
         fun({Module, Function}) ->
             analyze_url_body_plugin(Url, Tokens, Body, Module, Function)
         end,
         ?EBOT_BODY_ANALYZER_PLUGINS
     ).

analyze_url_body_plugin(Url, Tokens, Body, Module, Function) ->  
    Options = Module:Function(Url, Tokens, Body),
    error_logger:info_report({?MODULE, ?LINE, {analyze_url_body_plugin, Url, Module, Function, Options}}),
    ebot_db:update_doc(Url, Options).

get_env_normalize_url(Url, [{RE,Options}|L]) ->
    case re:run(Url, RE, [{capture, none},caseless]) of
    match ->
        {ok, Options};
    nomatch ->
        get_env_normalize_url(Url, L)
    end;
get_env_normalize_url(_Url, []) ->
    undefined.

needed_update_url_referral(SameMainDomain, SameDomain) ->
    {ok, SaveReferralsOptions} = ebot_util:get_env(save_referrals),
    lists:any(
      fun(Option) -> needed_update_url_referral(SameMainDomain, SameDomain, Option) end,
      SaveReferralsOptions
     ).

%% needed_update_url_referral(SameMainDomain, SameDomain, domain|subdomain|external)

needed_update_url_referral(false, false, external) ->       
    true;
needed_update_url_referral(true, false, subdomain) ->       
    true;
needed_update_url_referral(_, true, domain) ->
    true;
needed_update_url_referral(_,_,_) ->
    false.

start_workers(Workers) ->
    {ok, Pool} = ebot_util:get_env(workers_pool),
    ebot_worker_util:start_workers(Pool, Workers).


%%====================================================================
%% EUNIT TESTS
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

ebot_html_test() ->
    <<"http://www.redaelli.org/matteo/ebot_test/">>.

-endif.
