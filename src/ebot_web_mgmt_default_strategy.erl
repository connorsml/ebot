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
%%% File    : ebot_web_mgmt_default_strategy.erl
%%% Author  : matteo <<matteo.redaelli@@libero.it>
%%% Author  : <questeug@@gmail.com>
%%% Description : 
%%%
%%% Created :  4 Oct 2009 by matteo <matteo@redaelli.org>
%%%-------------------------------------------------------------------

-module(ebot_web_mgmt_default_strategy).

%% API
-export([
	 start_workers/1,
	 start_workers/3,
	 remove_worker/2,
	 create_workers/1,
	 run/1,
	 restart_worker/2
]).

-include("ebot.hrl").

create_workers(Type) ->
  ebot_worker_util:create_workers(Type).

start_workers(Workers) ->
  {ok, Pool} = ebot_util:get_env(workers_pool),
  ebot_worker_util:start_workers(Pool, Workers).

start_workers(Depth, Tot, Workers) ->
  ebot_worker_util:start_workers(Depth,Tot, Workers).

remove_worker(Worker, Workers) ->
  ebot_worker_util:remove_worker(Worker, Workers).

remove_worker(Worker) ->
  ebot_web:remove_worker(Worker).

run({Depth, Num}) ->

	  % error_logger:info_report({?MODULE, ?LINE, {running, depth, Depth}}),

    case ebot_mq:receive_url_new(Depth) of
    	{ok, {Url, _UrlData}} ->

    	    ebot_crawler:add_visited_url(Url),
    	    case ebot_web_util:fetch_url_with_only_html_body(Url) of
            {ok, Result} -> 
                    ebot_mq:send_url_fetched({Url,{ok, Result}}),
                    ebot_request_logger:log(Url, {ok, Result});
            {error, Reason} -> 
                    ebot_mq:send_url_fetched({Url,{error, Reason}})
          end;

  		{error, _Reason} ->
  		    error_logger:info_report({?MODULE, ?LINE, {run, no_queued_urls, depth, Depth, number, Num, waiting}}),
  		    timer:sleep( ?EBOT_EMPTY_QUEUE_TIMEOUT )
    end,

    case ebot_crawler:workers_status() of
		started ->
		    {ok, Sleep} = ebot_util:get_env(workers_sleep_time),
		    timer:sleep( Sleep ),
		    run({Depth, Num});
		stopped ->
		    error_logger:warning_report({?MODULE, ?LINE, {stopping_worker, self()}}),
		    remove_worker( {Depth, self()} )
    end.

restart_worker(Pid, {Type, Workers}) ->
    ebot_worker_util:restart_worker(Pid, {Type, Workers}).