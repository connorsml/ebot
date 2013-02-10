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

-module(ebot_web_mgmt_per_site_strategy).

%%%-------------------------------------------------------------------
%%% This strategy provides one ebot_web worker per site. No matter
%%% which depth the new urls queue has. Each worker fetches urls
%%% from all depth queues, looks up the handler worker for the url.
%%% (if the url doesn't belongs to the site the worker dispatches it
%%% to found or newly  created worker (method "run").)
%%%-------------------------------------------------------------------

-include("ebot.hrl").

%% API
-export([
	 create_workers/1,
	 start_workers/1,
	 start_workers/3,
	 remove_worker/2,
	 run/1,
	 strategy_cast/2,
	 strategy_call/2,
	 restart_worker/2,
	 workers_gc_collector/1
]).

%%% ebot_web's stat
-record(state,{
	  workers
	 }).

-define(WAIT_HANDLE_TIMEOUT, 500).

%%=========================================
%% Workers GC settings
%%=========================================
-define(WORKERS_GC_INACTIVITY_INTERVAL, 1000 * 60 * 60 * 5).
-define(WORKERS_GC_INTERVAL, 1000 * 60 * 6).


%%%-------------------------------------------------------------------
%%%	[{url1, pid1}, {url2, pid2}, ..., {urlN, pidN}]
%%	[{<pid1>, [{url, <url>}, {last_accessed, <...>} ]}, ...] , 
%%%-------------------------------------------------------------------
create_workers(Type) ->
	{Type, []}.

empty_worker() ->
  {start_worker(), []}.

start_workers(Workers) ->
  create_workers_gcollector(),

  % ebot_web workers doesn't start automatically at boot. They're created after new url has been retrieved 
  % from the new urls queue.
  {Type, Workers1} = Workers,
  {Type, [empty_worker() | Workers1]}.

start_workers(_Depth, _Tot, Workers) ->
  start_workers(Workers).

remove_worker(Pid, {Type,Workers}) ->
    Workers1 = case Workers of
       
       [{Pid, _}] -> 
       	  error_logger:info_report({?MODULE, ?LINE, {remove_worker, Pid, wont_be_removed_as_last}}),
          [{Pid, []}];

       _ ->
           Pid ! {stop},
      	   error_logger:info_report({?MODULE, ?LINE, {remove_worker, Pid, removed}}),
      	   lists:keydelete(Pid, 1, Workers)
    end,
    {Type, Workers1}.



run(Parent) ->

	receive 

		{handle_your_site_url, Url1} ->
		   %% To call strategy_cast({handle_your_site_url, Url, Pid}, State) with State to save self() to the workers list
		   %% of the ebot_web gen_server implementation.
		    handle_your_site_url(Url1),
		    error_logger:info_report({?MODULE, ?LINE, {run, got_msg, handle_your_site_url, Url1, self_is, self()}}),
		    Running = true;
		
		{inactivity_timeout} ->
			  error_logger:info_report({?MODULE, ?LINE, {self(), received, inactivity_timeout}}),		
			  call_parent(Parent, { remove_worker_myself, self()} ),
		    Running = false;

		{site_crawling_finished} ->
			  call_parent(Parent, { remove_worker_myself, self()} ),
			% continue executing 'cos the thread may be the last one. If not so, it will be stopped by 'stop' signal (just below).
		    Running = true;

		{stop} ->
			  error_logger:info_report({?MODULE, ?LINE, {run, got_msg, stop, self_is, self()}}),		
		    Running = false

		after ?WAIT_HANDLE_TIMEOUT ->
		   Running = true,			
		   error_logger:info_report({?MODULE, ?LINE, {run, no_msg_continue}})
	end,


    {ok, Pool} = ebot_util:get_env(workers_pool),
    QueueDepth = length(Pool),
    Workers = call_parent(Parent, {get_workers}),
    lists:foreach(fun(Depth) ->
				    case ebot_mq:receive_url_new(Depth) of
						{ok, {Url, _}} ->
						    HandlerPid = call_parent(Parent, {find_worker_for_url_site, Url}),

						    if 
							    HandlerPid == self() ->
							       error_logger:info_report({?MODULE, ?LINE, {self(), run, my_new_url, Url, self() }}),
							       % cast_parent(Parent, { handle_your_site_url, Url });
                     handle_your_site_url(Url);

							    true ->
							    	HandlerPid ! {handle_your_site_url, Url},
							    	error_logger:info_report({?MODULE, ?LINE, {self(), run, new_url, Url, sent_to, HandlerPid, from, self() }})
							end;

						{error, _Reason} ->
						    error_logger:info_report({?MODULE, ?LINE, {self(), run, no_queued_urls, Depth, skipping, workers, Workers }})
				    end
				   end,
				   lists:seq(0, QueueDepth-1)),

	% timer:sleep( ?EBOT_EMPTY_QUEUE_TIMEOUT ),
	case Running of
		true ->
		    case ebot_crawler:workers_status() of
				started ->
				    % {ok, Sleep} = ebot_util:get_env(workers_sleep_time),
				    % timer:sleep( Sleep ),
				    run(Parent);
				stopped ->
					stop_self(Parent)
		    end;
		_ ->
			empty
    end.

stop_self(Parent) ->
    error_logger:warning_report({?MODULE, ?LINE, {stopping_worker, self()}}),
    cast_parent(Parent, {remove_worker, self()}).

find_worker_for_url_site(Url, Workers) -> 
   SiteUrl = ebot_db_doc_site:site_id_for_url(Url),
   Pid = case lists:filter(fun({_Pid, Params}) ->
   	                   case Params of
   	                   	 [] -> true;
   	                   	 Else ->
            						   case lists:keysearch(url, 1, Else) of
            						   	 {value, {url, SiteUrl}} -> true;
            						   	 {value, {url, _}} -> false;
                             {value, Else1} -> 
                                error_logger:error_report({?MODULE, ?LINE, {find_worker_for_url_site_error, url, Url, reason, Else1}}),
                                empty
            						   end
						           end
					end, 
					Workers) of

   	  [] -> start_worker(self());

	    [{Pid1, []}] -> Pid1; % 1st thread has no url assigned.

	    [{Pid1, _Params}] -> Pid1;

      Else ->
        error_logger:error_report({?MODULE, ?LINE, {find_worker_for_url_site_error, url, Url, reason, Else}}),
        empty
    end,
    NewOrOldWorkers = add_url_worker(Pid, SiteUrl, Workers),
    Res = {Pid, NewOrOldWorkers},
    error_logger:warning_report({?MODULE, ?LINE, {find_worker_for_url_site, url, Url, pid, Pid, workers, NewOrOldWorkers}}),
    Res.

%%%-----------------------------------------------------------------------------------------------------------
%%% strategy_cast alternatives are invoked thru ebot_web:cast(Msg, State) -> <Stratery>:strategy_cast(Request, State)
%%%-----------------------------------------------------------------------------------------------------------
% strategy_cast({handle_your_site_url, Url, Pid}, State) ->
% 	% error_logger:info_report({?MODULE, ?LINE, {handle_your_site_url_inside, Url, pid, Pid}}),
% 	handle_your_site_url(Url);

strategy_cast({site_crawling_finished, SiteUrl}, State) ->
    {_, Workers} = State#state.workers, % cannot call ebot_web:get_workers 'cos it leads to timeout 'cos ebot_web is waiting for this method to return.
    {HandlerPid, _Workers} = find_worker_for_url_site(SiteUrl, Workers),
    HandlerPid ! {site_crawling_finished}, 
    error_logger:info_report({?MODULE, ?LINE, {site_crawling_finished, SiteUrl, sent_to, HandlerPid}}),
    State;

strategy_cast({reanimate_workers}, State) ->
  {Type, Workers} = State#state.workers,
	error_logger:info_report({?MODULE, ?LINE, {reanimate_workers, workers, Workers}}),

	Workers1 = lists:foldl(
	        fun({Pid, ProcParams}, Acc) ->
			   case erlang:is_process_alive(Pid) of
			       true ->
					   % error_logger:info_report({?MODULE, ?LINE, {reanimate_workers, status, proplists:get_value(status, process_info(Pid)) }}),
					   error_logger:info_report({?MODULE, ?LINE, {reanimate_workers, Pid, is_alive }}),
					   [{Pid, ProcParams}|Acc];
				     false ->
					   error_logger:info_report({?MODULE, ?LINE, {reanimate_workers, recovering_dead_worker, Pid}}),
					   NewPid = start_worker(),
					   [{NewPid, ProcParams}|Acc]
					end
			end,
			[],
			Workers),
	State#state{workers={Type, Workers1}}.

%%%-----------------------------------------------------------------------------------------------------------
%%% strategy_call alternatives are invoked thru ebot_web:call(Msg, State) -> <Stratery>:strategy_call(Request, State)
%%%-----------------------------------------------------------------------------------------------------------
strategy_call({remove_worker_myself, Pid}, State) ->
  Workers = remove_worker(Pid, State#state.workers),
	error_logger:info_report({?MODULE, ?LINE, {remove_worker_myself, before, State#state.workers, after_, Workers}}),    
	{ok, State#state{workers = Workers}};

strategy_call({find_worker_for_url_site, Url}, State) ->
  {Type, Workers} = State#state.workers,
	{Pid, Workers1} = find_worker_for_url_site(Url, Workers),
	{Pid, State#state{workers={Type, Workers1}}};

strategy_call({gc_workers}, State) ->
    {Type, Workers} = State#state.workers,
	NewState = State#state{workers={Type, gc_workers(Workers)}},
	error_logger:info_report({?MODULE, ?LINE, {gc_workers, before, State#state.workers, after_,  gc_workers(Workers)}}),
	{ok, NewState}.


handle_your_site_url(Url) ->
     try
		% ebot_event_manager:sync_notify({url_action, {before_fetching, Url}}),
	    ebot_event_manager:notify({url_action, {before_fetching, Url}}),
	    
	    timer:sleep( ?EBOT_EMPTY_QUEUE_TIMEOUT ), % There must be a site-dependent delay.
	    
	    ebot_crawler:add_visited_url(Url),	    
	    Result = ebot_web_util:fetch_url_with_only_html_body(Url),
	    
	  %   case Result of
		 %    {ok, {_, _Headers, Content}} ->
			%     {ok, CD} = iconv:open("koi8-r", "utf-8"),
			%     Content1 = list_to_binary(binary_to_list(Content)),
			% 	try iconv:conv(CD, Content1) of 
					
			% 		{ok, Out} ->
			% 			error_logger:info_report({?MODULE, ?LINE, {content_converted, into, Out}});

			% 		{error, Reason} ->
			% 		 	error_logger:info_report({?MODULE, ?LINE, {content_not_converted, Reason, Content1}})
				
			% 	catch
			% 		error:Reason ->
			% 			error_logger:info_report({?MODULE, ?LINE, {content_not_converted, Reason}})
			%     end;
			% _ -> pass
	  %   end,

	    % error_logger:info_report({?MODULE, ?LINE, {handle_your_site_url, url, Url, result, Result}}),

	    % It's necessary to check if the Url has already been visited during current site pass and not to call ebot_mq:send_url_fetched(...) if so.
	    % This may happen when crawling has been killed before it finised working.
	    ebot_mq:send_url_fetched({Url,Result})
	catch
	  Error	-> 
	  	error_logger:info_report({?MODULE, ?LINE, {handle_your_site_url, Url, error, Error}})
	end.

%====================================================================
%% Internal functions
%%====================================================================

add_url_worker(Pid, Url, Workers) ->
	error_logger:info_report({?MODULE, ?LINE, {add_url_worker, pid, Pid, url, Url}}),		
    
  % {Type, Workers1} = Workers,
  Workers1 = Workers,

	Workers2 = case lists:keysearch(Pid, 1, Workers1) of
	    % Check if the 1st worker has url assigned.
		{value, {Pid, []}} -> 
		   
		   WData = [{url, Url}],
		   WData1 = update_wdata(last_accessed, WData),
		   error_logger:info_report({?MODULE, ?LINE, {add_url_worker, url, Url, pid, Pid, first_assigned, workers, Workers}}),
		   lists:keyreplace(Pid, 1, Workers1, {Pid, WData1});
		
		{value, {Pid, WData}} -> 

		   % don't add, update worker data
  		  WData1 = update_wdata(last_accessed, WData),
  		  error_logger:info_report({?MODULE, ?LINE, {add_url_worker, url, Url, pid, Pid, existing_modified, workers, Workers}}),
		    lists:keyreplace(Pid, 1, Workers1, {Pid, WData1});
		
		false -> 
			 % add a new handler
		   WData1 = update_wdata(last_accessed, [{url, Url}]),
		   error_logger:info_report({?MODULE, ?LINE, {add_url_worker, url, Url, pid, Pid, new_added, workers, Workers}}),
		   [ {Pid, WData1 } | Workers1 ]

	end,

	% {Type, Workers2}.
  Workers2.


spawn_worker(Type) ->
    spawn_link(ebot_worker_util:worker_module(Type), run, [ self() ]).

start_worker() ->
    spawn_worker(web).


start_worker(Parent) ->
    spawn_worker(web, Parent).

spawn_worker(Type, Parent) ->
    spawn_link(ebot_worker_util:worker_module(Type), run, [ Parent ]).


restart_worker(WPid, {Type, Workers}) ->
    %TODO: there can be not only web-type worker but gc collector's one.
    Workers1 = lists:foldl(fun({Pid, WData}, Acc)-> 
                  if 
                    Pid == WPid -> 
                       NewPid = spawn_worker(Type),
                       error_logger:info_report({?MODULE, ?LINE, {replacing_worker, Pid, with, NewPid}}),
                       [ {NewPid, WData} | Acc ];
                    true ->  [{Pid, WData} | Acc]
                   end
                end, 
                [], 
                Workers),
    {Type, Workers1}.


%============================================================================
%% Workers garbage collection. Removing workers that din't send notification
%% on finishing site-crawling. 
%%===========================================================================
workers_gc_collector(Parent) ->
  timer:sleep(?WORKERS_GC_INTERVAL),
  error_logger:info_report({?MODULE, ?LINE, {workers_gc_collector, parent, Parent}}),  
  call_parent(Parent, {gc_workers}),
  workers_gc_collector(Parent).

create_workers_gcollector() ->
  %TODO: hardcoded dependency on web.
  % self() will return running gen_server instance id using this module.
  spawn_link(?MODULE, workers_gc_collector, [ self() ]).

gc_workers(Workers) ->
  Now = erlang:now(),

  {Alive, Dead} = lists:partition(fun({_Pid, WData})-> 
  	                                case WData of
  	                                  [] -> true; % single worker without data must live.
                				  					  _Else -> 
                				  					     case lists:keysearch(last_accessed, 1, WData) of 
                    					  						{value, {last_accessed, LastAccessed}} ->
                    					  						   timer:now_diff(Now, LastAccessed)/1000 <  ?WORKERS_GC_INACTIVITY_INTERVAL
                   					  					 end
                					  			     end
						                      end, 
						                      Workers),
  % One process must live
  {Alive1, Dead1} = case Alive of
  	[] ->

  	   [{Pid, _}| T] = Dead,
  	   { [{Pid, []}], T };

  	_Else ->
  	   {Alive, Dead}
   end,

   lists:foreach(fun({Pid, Data})-> 
          					error_logger:info_report({?MODULE, ?LINE, {inactivity_timeout, sent_to, Pid, crawling_data, Data}}),
        	  				Pid ! {inactivity_timeout}
        	  		  end, 
	  			      Dead1),

  Alive1. 
    

%=======================================================================
%% Functions to call the gen_server instance this module is a part of.
%%=======================================================================
call_parent(Parent, Cmd) ->
  gen_server:call(Parent, Cmd, infinity).

cast_parent(Parent, Cmd) ->
  gen_server:cast(Parent, Cmd).


update_wdata(last_accessed, WData) ->
  Now = erlang:now(),
  case lists:keysearch(last_accessed, 1, WData) of
  	 {value, _} -> lists:keyreplace(last_accessed, 1, WData, {last_accessed, Now});
  	 false -> [{last_accessed, Now} | WData]
  end.  
