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


-module(ebot_db_backend_postgresql).

%%====================================================================
%% API.
%%====================================================================
 -export([
     open_or_create_db/1,
 	   open_doc/2,
 	   open_doc/3,
     open_doc_url_by_content_md5/4,
     open_doc_url_by_url/2,
 	   save_doc/3,
 	   save_doc/4,
     delete_passed_urls/3,
     mark_urls_as_unindexed/2
	]).

-define(PAGES_TABLE, pages). 
-define(PASSES_TABLE, site_passes).

-define(RETRY_TIMEOUT, 2000). 

open_or_create_db({host, Host, db, DBName, login, Login, password, Pwd}) ->
  {ok, Conn} = pgsql:connect(Host, [Login], [Pwd], [{database, DBName}]),

  Db = create_db_storage(),
  Db1 = store_db_connect(Db, Conn),
  Db2 = query_tables_meta(Db1, [?PAGES_TABLE, ?PASSES_TABLE]),
  error_logger:info_report({?MODULE, ?LINE, {open_or_create_db, db, Db2}}),
  {ok, Db2}.

query_tables_meta(Db, Tables) ->
    lists:foldl(fun(Tbl, CommonMeta) ->
                       case query_tbl_meta(fetch_db_connect(Db), Tbl) of
                          {ok, _, Columns} ->
                              Columns1 = lists:foldl(fun(Name, Acc) ->  [Name | Acc] end, [], Columns),
                              error_logger:info_report({?MODULE, ?LINE, {open_or_create_db, Tbl, meta, Columns1}}),
                              store_tbl_meta(CommonMeta, Tbl, Columns1);
                          Else ->
                            throw({query_tables_meta_error, Else})
                       end
                    end,
                    Db, 
                    Tables).


query_tbl_meta(Db, TblName) ->
  Query = "SELECT column_name FROM information_schema.columns WHERE table_name = '" ++ atom_to_list(TblName) ++ "' ORDER BY ordinal_position;",
  pgsql:equery(Db, Query).

create_db_storage() -> 
  dict:new().

store_db_connect(Db, Conn) ->
  dict:store(<<"conn">>, Conn, Db).

fetch_db_connect(Db) ->
  dict:fetch(<<"conn">>, Db).

store_tbl_meta(Db, TblName, TblMeta) ->
  Meta = fetch_tables_meta(Db),
  TblName1 = list_to_binary(atom_to_list(TblName)),
  Meta1 = dict:store(TblName1, TblMeta, Meta),
  dict:store(<<"meta">>, Meta1, Db).

fetch_tables_meta(Db) ->
  case dict:find(<<"meta">>, Db) of
    {ok, Value} -> Value;
    error -> dict:new()
  end.

fetch_table_meta(Db, Table) ->
  TablesMeta = fetch_tables_meta(Db),
  case dict:find(list_to_binary(atom_to_list(Table)), TablesMeta) of
    {ok, Value} -> Value;
    error -> dict:new()
  end.


open_doc(Db, Id) ->
  case open_doc(Db, Id, ?PAGES_TABLE, url) of 
  	{ok, [Doc]} ->
  	 	{ok, Doc};
    {error, not_found} ->
       {error, not_found};
    {error, invalid_statement} ->
       {error, invalid_statement};
    _  -> 
      ebot_util:repeatable_calls(?MODULE, open_doc, [Db, Id], ?RETRY_TIMEOUT)
  end.

open_doc(Db, Id, Collection) ->
  case open_doc(Db, Id, Collection, url) of
  	{ok, [Doc]} ->
  	 	{ok, Doc};
    {error, not_found} ->
       {error, not_found};
    {error, invalid_statement} ->
       {error, invalid_statement};
    _ -> 
      ebot_util:repeatable_calls(?MODULE, open_doc, [Db, Id, Collection], ?RETRY_TIMEOUT)
  end.

open_doc(Db, Id, <<"sites">>, Key) ->
  open_doc_from_table(Db, ?PASSES_TABLE, [{Id, Key}]);

open_doc(Db, Id, Table, Key) ->
  open_doc_from_table(Db, Table, [{Id, Key}]).

open_doc_url_by_content_md5(Db, SiteDomain, Digest, PassNumber) ->
  case open_doc_from_table(Db, ?PAGES_TABLE, [{SiteDomain, ebot_domain}, {Digest, digest},{PassNumber, ebot_visits_count}]) of
     {ok, Docs} ->
        {ok, Docs};
     {error, not_found} ->
        {error, not_found};
     {error, invalid_statement} ->
        {error, invalid_statement};
     _ -> 
        ebot_util:repeatable_calls(?MODULE, open_doc_url_by_content_md5, [Db, {Digest, PassNumber}], ?RETRY_TIMEOUT)
  end.

open_doc_url_by_url(Db, Url) ->
  case open_doc(Db, Url, ?PAGES_TABLE, url) of
    {ok, [Doc]} ->
      {ok, Doc};
     {error, not_found} ->
        {error, not_found};
    {error, invalid_statement} ->
       {error, invalid_statement};
    _ ->     % {error, timeout}
      ebot_util:repeatable_calls(?MODULE, open_doc_url_by_url, [Db, Url], ?RETRY_TIMEOUT)
  end.  

save_doc(Db, Url, Doc) ->
  case save_doc(Db, ?PAGES_TABLE, Url, Doc) of
     {ok, _} ->
        {ok, Doc};
     % {error, timeout} -> 
     _ -> 
        ebot_util:repeatable_calls(?MODULE, save_doc, [Db, Url, Doc], ?RETRY_TIMEOUT)
  end.

save_doc(Db, <<"sites">>, Url, Doc) ->
  case save_doc_to_table(Db, ?PASSES_TABLE, Url, Doc) of
     {ok, _} ->
        {ok, Doc};

     _ -> 
        ebot_util:repeatable_calls(?MODULE, save_doc, [Db, <<"sites">>, Url, Doc], ?RETRY_TIMEOUT)
  end;

save_doc(Db, Table, Url, Doc) ->
  case save_doc_to_table(Db, Table, Url, Doc) of
     {ok, _} ->
        {ok, Doc};
     _ -> 
        ebot_util:repeatable_calls(?MODULE, save_doc, [Db, Table, Url, Doc], ?RETRY_TIMEOUT)
  end.

delete_passed_urls(Db, SiteUrl, Pass) ->
  Query = "delete from " ++ atom_to_list(?PAGES_TABLE) ++ " where ebot_domain = '" ++ SiteUrl ++ "'" ++ 
          " and not ebot_visits_count = " ++ right_value(Pass),
  error_logger:info_report({?MODULE, ?LINE, {delete_passed_urls, SiteUrl, Pass, list_to_binary(Query)}}), 
  Db1 = fetch_db_connect(Db),  
  case pgsql:equery(Db1, Query) of
     {ok, _} -> ok;
     % {error, timeout} -> 
     _ -> 
        ebot_util:repeatable_calls(?MODULE, delete_passed_urls, [Db, SiteUrl, Pass], ?RETRY_TIMEOUT)
  end.

mark_urls_as_unindexed(Db, SiteID) ->
  Query = "update " ++ atom_to_list(?PAGES_TABLE) ++ " set indexed = false where ebot_domain = '" ++ SiteID ++ "'",
  error_logger:info_report({?MODULE, ?LINE, {mark_urls_as_unindexed, SiteID, Query}}),  
  Db1 = fetch_db_connect(Db),  
  case pgsql:equery(Db1, Query) of
     {ok, _} -> ok;
     % {error, timeout} -> 
     _ -> 
        ebot_util:repeatable_calls(?MODULE, mark_urls_as_unindexed, [Db, SiteID], ?RETRY_TIMEOUT)
  end.


%%====================================================================
%% Internal functions.
%%====================================================================
save_doc_to_table(Db, Table, Url, Doc) ->
   Db1 = fetch_db_connect(Db),

   PgDoc = crawler_doc_to_pg_doc(Db, Doc, Table),

   Where = "where url = '" ++ ebot_util:safe_binary_to_list(Url) ++ "'",
   Table1 = atom_to_list(Table),
   
   InsertQuery = construct_insert_statement(PgDoc, Table1, Where),
   case pgsql:equery(Db1, InsertQuery) of 
	   {ok, _} ->
     		 error_logger:info_report({?MODULE, ?LINE, {save_doc_to_table, Url, InsertQuery}}), 
  	     {ok, Doc};

     {error,{error, error, <<"23505">>, _, _}} ->
     		 Set = construct_set_statement(PgDoc),
  		   UpdateQuery = "update " ++ Table1 ++ " set " ++ Set ++ " " ++ Where,
      	 case pgsql:equery(Db1, UpdateQuery) of 
      		 {ok, _} ->
      				error_logger:info_report({?MODULE, ?LINE, {save_doc_to_table, Url, ebot_util:safe_list_to_binary(UpdateQuery)}}),
              {ok, Doc};

           {error, timeout} ->
             {error, timeout};

      		 Else ->
      			    error_logger:info_report({?MODULE, ?LINE, {save_doc_to_table, failed_to_update, Url, UpdateQuery, Else, treated_as_timeout}}),
                {error, timeout}
      	 end;

	  {error, {error,error,<<"22021">>, Error1, Error2 }} ->
	   	error_logger:info_report({?MODULE, ?LINE, {save_doc_to_table, Url, failed_to_insert_bad_text_content, Error1, Error2}}),
      {ok, Doc};

    {error, timeout} ->
      {error, timeout};

	  {error, Error} ->
	 	  error_logger:info_report({?MODULE, ?LINE, {save_doc_to_table, Url, list_to_binary(InsertQuery), failed_with_error, Error}}),
      {ok, Doc};

     _ ->
      {error, timeout}
   end.



construct_where_for(KeyValuePairs) ->
  construct_where_for(KeyValuePairs, []).

construct_where_for([], Acc) ->
  "where " ++  string:join(Acc, " and ");

construct_where_for([{Id, Key}|Rest], Acc) ->
  Pair = atom_to_list(ebot_util:atomize(Key)) ++ "=" ++ right_value(Id),
  construct_where_for(Rest, [Pair | Acc]).


open_doc_from_table(Db, Table, KeyValuePairs) ->
  Where = construct_where_for(KeyValuePairs),
  Table1 = atom_to_list(Table),
  Query = "select * from " ++ Table1  ++ " " ++ Where,
  Db1 = fetch_db_connect(Db),  
  case pgsql:equery(Db1, Query) of
	  {ok, _, []} ->
		    error_logger:info_report({?MODULE, ?LINE, {open_doc_from_table, Query, not_found}}),
        {error, not_found};   

    {ok, Headers, Docs} ->
      error_logger:info_report({?MODULE, ?LINE, {open_doc_from_table, Query, Docs}}),   
      Docs1 = lists:foldl(fun(Doc, Acc) -> [pg_doc_to_crawler_doc(Headers, Doc)|Acc] end, [], Docs),
      {ok, Docs1};

    {error,{error, error, <<"22021">>, ErrorMsg, _}} ->
      error_logger:info_report({?MODULE, ?LINE, {open_doc_from_table, Query, error, ErrorMsg}}),
      {error, invalid_statement};

    {error, Reason} -> 
      error_logger:info_report({?MODULE, ?LINE, {open_doc_from_table, Query, error, Reason}}),
      {error, Reason};

    Error ->
     error_logger:info_report({?MODULE, ?LINE, {open_doc_from_table, Query, unknown_error, Error}}),
     {error, timeout}
  end.  


pg_doc_to_crawler_doc(Headers, Row) ->
  pg_doc_to_crawler_doc(Headers, tuple_to_list(Row), dict:new()).

pg_doc_to_crawler_doc([], [], Acc) ->
  Acc;
  
pg_doc_to_crawler_doc([Header|HT], [Value|RT], Acc) ->
  {column, Name, _, _, _, _} = Header,
  % Acc1 = case Name of
  % 	<<"id">> ->
  % 		Acc;
  %   Else ->
 	% dict:store(Else, Value, Acc)
  % end,
  % pg_doc_to_crawler_doc(HT, RT, Acc1).
  pg_doc_to_crawler_doc(HT, RT, dict:store(Name, Value, Acc)).


crawler_doc_to_pg_doc(Db, Doc, Table) ->
  Columns = fetch_table_meta(Db, Table),
  Doc1 = dict:fold(fun(Key, Value, Acc) ->
  	                 % Exclude _id field
  	                 case Key of
  	                   <<"_id">> -> 
  	                      Acc;
                       <<"id">> -> 
                          Acc;
  	                   _ ->
                         case lists:keyfind(Key, 1, Columns) of 
                            false -> Acc;
  	                 	       _ -> [Value, Key|Acc]     
                         end
  	                 end
                   end, 
                   [], 
                   Doc),

  lists:reverse(Doc1).

construct_set_statement(Pairs) ->
	construct_set_statement(Pairs, []).

construct_set_statement([], Acc) -> 
   string:join(Acc, ",");

construct_set_statement([Column, Value|T], Acc) -> 
   Elem = ebot_util:safe_binary_to_list(Column) ++ "=" ++ right_value(Value),
   construct_set_statement(T, [Elem | Acc]).


construct_insert_statement(Doc, Table, _Where) ->
   {Columns, Values} = separate_columns_and_values(Doc),
   Columns1 = string:join(Columns, ","),
   Values1 = string:join(Values, ","),
   % "insert into " ++ Table ++ " (" ++ Columns1 ++ ")" ++ " select " ++ Values1 ++ " where not exists (select 1 from " ++ Table ++ " " ++ Where ++ ")".
   "insert into " ++ Table ++ " (" ++ Columns1 ++ ") values (" ++ Values1 ++ ")".


separate_columns_and_values(Doc) ->
   separate_columns_and_values(Doc, [], []).

separate_columns_and_values([Col, Val|T], ColAcc, ValAcc) ->
  separate_columns_and_values(T, [column_name(Col)|ColAcc], [right_value(Val)|ValAcc]);

separate_columns_and_values([], ColAcc, ValAcc) ->	
  {ColAcc, ValAcc}.


column_name(Column)->
  ebot_util:safe_binary_to_list(Column).


right_value(Candidate) when Candidate == null ->
   "''";

right_value(Candidate) when is_atom(Candidate)->
   "'" ++ atom_to_list(Candidate) ++ "'";

right_value(Candidate) when is_binary(Candidate) ->
   "'" ++ ebot_util:safe_binary_to_list(Candidate) ++ "'";

right_value(Candidate) when is_integer(Candidate) ->
   "'" ++ integer_to_list(Candidate) ++ "'";

right_value(Candidate) when is_list(Candidate) ->
   "'" ++ binary_to_list(list_to_binary(Candidate)) ++ "'";

right_value(Candidate) ->
   "'" ++ Candidate ++ "'".


