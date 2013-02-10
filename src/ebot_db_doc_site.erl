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


-module(ebot_db_doc_site).

%% API
-export([
	 create_doc/1,
	 site_id_for_url/1,
	 get_doc_site_id/1
	 % open_or_create_site/1
	 % open_or_create_site_for_url/1
	]).

create_doc(Site) ->
    Doc = dict:from_list([ 
		   {<<"new_links_count">>, 0},
		   {<<"fetched_links_count">>, 0},
		   {<<"processed_links_count">>, 0},
		   {<<"refused_links_count">>, 0},
		   {<<"robots_txt">>, <<>>},
		   {<<"pass_number">>, 0},
		   {<<"url">>, site_id_for_url(Site)},
       {<<"domain">>, ebot_url_util:url_domain(ebot_util:safe_binary_to_list(Site))}
		 ]),
    {ok, Doc}.

%%====================================================================
%% 
%%====================================================================
% open_or_create_site(Url) ->
%   Site = "http://" ++ ebot_url_util:url_add_final_slash(ebot_url_util:url_domain(Url)),
%   {ok, Doc} = ebot_db:open_or_create_site(Site), % calls create_doc
%   {ok, Site, Doc}.

% open_or_create_site_for_url(Url) ->
%   open_or_create_site(Url).

site_id_for_url(Url) ->
  list_to_binary(ebot_url_util:url_add_final_slash("http://" ++ ebot_url_util:url_domain(ebot_util:safe_binary_to_list(Url)))).

get_doc_site_id(SiteDoc) ->
  ebot_db_doc:doc_get_value(<<"url">>, SiteDoc).