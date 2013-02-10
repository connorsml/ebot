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

%% @doc TEMPLATE.

-module(ebot_db_backend_mongo).

% TODO: move to config
-define(PAGES_COLLECTION, pages). 

% %% API
 -export([
     open_or_create_db/2,
   	 open_doc/2,
   	 open_doc/3,
     open_doc_url_by_content_md5/2,
     open_doc_url_by_url/2,
   	 save_doc/3,
   	 save_doc/4,
     delete_passed_urls/3
	]).

save_doc(Db, Url, Doc) ->
    save_doc(Db, ?PAGES_COLLECTION, Url, Doc).

save_doc(Db, Collection, Key, Doc) ->
   {ok, DBName} = ebot_util:get_env(db_name),
    Doc1 = dict:store(url, Key, Doc),
    MongoDoc = crawler_doc_to_mongo_doc(Doc1),
    mongo:do(safe, 
	         master, 
	         Db,
	         DBName, 
	         fun() -> mongo:repsert(ebot_util:atomize(Collection), {url, Key} , MongoDoc) end),
	{ok, Doc}.  
  

open_doc(Db, Id) ->
  open_doc(Db, Id, ?PAGES_COLLECTION, url).

open_doc(Db, Id, Collection) ->
  open_doc(Db, Id, Collection, url).

open_doc(Db, Id, Collection, Key) ->
  {ok, DBName} = ebot_util:get_env(db_name),
	case mongo:do(safe, 
	         master, 
	         Db,
	         DBName, 
	         fun() -> mongo:find_one(ebot_util:atomize(Collection), {Key, ebot_util:safe_list_to_binary(Id)}) end) of

	  {ok, {}} ->
	     {error, not_found};

	  {ok, {Doc}} ->
	    {ok, mongo_doc_to_crawler_doc(Doc)}
    end.  

open_doc_url_by_content_md5(Db, Digest) ->
  open_doc(Db, Digest, ?PAGES_COLLECTION, digest).

open_doc_url_by_url(Db, Url) ->
  open_doc(Db, Url, ?PAGES_COLLECTION, url).

mongo_doc_to_crawler_doc(Doc) -> 
  traverse_mongo_doc(tuple_to_list(Doc), dict:new()).


traverse_mongo_doc([], CrawlerDoc) ->
  CrawlerDoc;

traverse_mongo_doc([Key, Value|Tail], CrawlerDoc) ->
  Key1 = list_to_binary(atom_to_list(Key)),
  CrawlerDoc1 = dict:store(Key1, Value, CrawlerDoc),
  traverse_mongo_doc(Tail, CrawlerDoc1).


crawler_doc_to_mongo_doc(Doc) ->
  Doc1 = dict:fold(fun(Key, Value, Acc) ->
  	                 % Exclude _id field, so MongoDB generate it itself.
  	                 case Key of
  	                   <<"_id">> -> 
  	                      Acc;
  	                   _ ->
  	                 	 [Value, ebot_util:atomize(Key)|Acc]     
  	                 end
                   end, 
                   [], 
                   Doc),
  list_to_tuple(lists:reverse(Doc1)).
   

delete_passed_urls(Db, SiteUrl, Pass) ->
  {ok, DBName} = ebot_util:get_env(db_name),
  case mongo:do(safe, 
           master, 
           Db,
           DBName, 
           fun() -> mongo:delete (?PAGES_COLLECTION, {ebot_domain, ebot_util:safe_list_to_binary(SiteUrl), ebot_visits_count, { '$ne', Pass} } ) end) of
    {ok, ok} ->
       {ok};
    Else ->
       {error, Else}
    end.  

  


%%====================================================================
%% Db Creation
%%====================================================================
open_or_create_db(HostName, Port) -> 
   mongo:connect({HostName, Port}).

% test_mongo() ->
%   {ok, Conn} = open_or_create_db(),
%   % {ok, {db_name, DBName}} = ebot_util:get_env(mongodb),
  
%   mongo:do(safe, 
%            master, 
%            Conn,
%            mo_ads_development, 
%            fun() ->
% 			 mongo:delete (foo, {}),
% 			 mongo:insert (foo, {'_id',222, x,1, y,2}),
% 			 mongo:find_one (foo, {'_id',222}),
% 			 mongo:insert (foo, {x,333, y,2}),
% 			 mongo:find_one (foo, {x,333}) 
% 		   end
% 		   ).  

%%====================================================================
%% EUNIT TESTS
%%====================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

ebot_db_backend_mongo_test() ->
	mongo_doc_to_crawler_doc_test(),
	crawler_doc_to_mongo_doc_test().

% Not sure if it still works.
% mongo_doc_to_crawler_doc_test() ->
% 	CrawlerDoc = dict:new(),
% 	CrawlerDoc1 = dict:store("k1", "v1", CrawlerDoc),
% 	CrawlerDoc2 = dict:store("k2", "v2", CrawlerDoc1),
% 	FromMongoDoc = mongo_doc_to_crawler_doc({'_id', "dummyId", "k1", "v1", "k2", "v2"}),
%     ?assertEqual(CrawlerDoc2, FromMongoDoc).


crawler_doc_to_mongo_doc_test() ->
	CrawlerDoc = dict:new(),
	CrawlerDoc1 = dict:store("k1", "v1", CrawlerDoc),
	CrawlerDoc2 = dict:store("k2", "v2", CrawlerDoc1),
	FromCrawlerDoc = crawler_doc_to_mongo_doc(CrawlerDoc2),
  ?assertEqual(FromCrawlerDoc, {"k1", "v1", "k2", "v2"}).

test_save_doc() ->
	{ok, Db} = open_or_create_db(),
	Doc = dict:new(),
	Doc1 = dict:store(k1, v1, Doc),
	Doc2 = dict:store(k2, v2, Doc1),
	save_doc(Db, foo, <<"http://localhost.ru">>, Doc2).

test_open_doc() ->
	{ok, Db} = open_or_create_db(),
	open_doc(Db, <<"http://localhost.ru">>, foo).

-endif.
