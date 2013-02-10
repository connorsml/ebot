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

-module(ebot_elastic_search_indexer).

-include("deps/jsonerl/src/jsonerl.hrl").

-export([index/5]).

-define(INDEX_NAME, "mo_ads_pages").
-define(INDEX_TYPE, "document").

-record(json_data, {url, id, host, title, content, content_length, updated_at}).

index(Url, Tokens, Body, Headers, PassNumber) ->
  {ok, {url, [{host, Host},{port,Port}]}} = ebot_util:get_env(elastic_search),

  % error_logger:info_report({?MODULE, ?LINE, {send_to_elastic_search_for, Url}}),
  TokensBody = extract_body(Tokens),
  case lists:keyfind("content-type", 1, Headers) of

    {_, CType} ->
      case re:run(CType, "^text/html",[{capture, none},caseless] ) of
        match ->

          case is_content_already_indexed(Url, Body, PassNumber) of
              {true, Doc} ->

                 DupUrl = ebot_db_doc:doc_get_value(<<"url">>, Doc),
                 error_logger:info_report({?MODULE, ?LINE, {content_already_indexed, Url, under_url, DupUrl}});

              false ->

                  error_logger:info_report({?MODULE, ?LINE, {content_is_new, Url}}),            
                  Id = ebot_util:md5_for(Url),
                  Title = extract_title(Tokens),
                  JsonData = #json_data{url = ebot_util:safe_list_to_binary(Url), 
                                    id = Id, 
                                    host = extract_host(Url), 
                                    title = Title, 
                                    content = TokensBody,
                                    content_length = list_to_binary(ebot_util:content_length(TokensBody, as_list)),
                                    updated_at = list_to_binary( ebot_util:now_as_utc() )},
                  Query = binary_to_list(?record_to_json(json_data, JsonData)),
                  Path = "http://" ++ Host ++ ":" ++ Port ++ "/" ++ ?INDEX_NAME ++ "/" ++ ?INDEX_TYPE ++ "/" ++ binary_to_list(Id),
                  Request  = {Path, [], "application/json", Query},
                  Response = httpc:request(post, Request, [], []),

                  ebot_db:update_doc(Url, [{update_value, <<"indexed">>, true}]),

                  error_logger:info_report({?MODULE, ?LINE, {send_to_elastic_search_result, Url, response, Response}})
          end;
            
        nomatch ->
          pass
      end
  end,
  ok.

extract_title([]) -> 
  <<"">>;

extract_title([{start_tag,<<"title">>, _, _}|Rest]) -> 
  extract_title(Rest, start_title);

extract_title([_| Rest]) -> 
  extract_title(Rest).


extract_title([{end_tag, <<"title">>}|_], start_title) ->
  <<"">>;

extract_title([{data, Title, _}|_], start_title) ->
  Title.


extract_body(Tokens) -> 
  ToSkip =  [ <<"script">>, <<"style">> ],
  extract_body(Tokens, [], scanning, ToSkip).

% Data ended unexpectedly.
extract_body([], Acc, _, _) ->
  case Acc of
    [] -> <<>>;
    Else -> join_body_accum(Else)
  end;

extract_body([{start_tag,<<"body">>, _, _}| Rest ],   Acc, _, ToSkip) ->
  extract_body(Rest,   Acc, collecting, ToSkip);

extract_body([{start_tag, TagName, _, _}| Rest ], Acc, _, ToSkip) ->
  Mode = case lists:member(TagName, ToSkip) of
    true -> skipping;
    false -> collecting
  end,
  extract_body(Rest, Acc, Mode, ToSkip);

extract_body([{end_tag, TagName}| Rest], Acc,  skipping, ToSkip) -> 
  Mode = case lists:member(TagName, ToSkip) of
    true -> collecting;
    false -> skipping
  end,
  extract_body(Rest,   Acc, Mode, ToSkip);

extract_body([{end_tag,<<"body">>}| _], Acc, collecting, _)-> 
  join_body_accum(Acc);

extract_body([{data, Data, _}|Rest], Acc, collecting, ToSkip)-> 
  % extract_body(Rest,   [ebot_html_util:to_utf8(ebot_util:safe_list_to_binary(Data), Enc) | Acc], collecting);
  extract_body(Rest,   [Data | Acc], collecting, ToSkip);

extract_body([ _ | Rest ], Acc, Mode, ToSkip) ->
  extract_body(Rest, Acc, Mode, ToSkip).


join_body_accum(Acc) ->
  ebot_util:bjoin(lists:reverse(Acc)).

extract_host(Url) ->
  ebot_util:md5_for(ebot_url_util:url_main_domain(Url) ++ "/").


is_content_already_indexed(Url, Data, PassNumber) ->
  case ebot_db:open_doc_url_by_content_md5(Url, ebot_util:md5_for(Data), PassNumber) of
    {error, _Reason} ->
      false;

    {ok, Docs} ->
      % error_logger:info_report({?MODULE, ?LINE, {is_content_already_indexed, Url, for_docs, Docs}}),
      % There might be a number of urls docs with the same digest. Sending the one with the lowerest id.
      Docs1 = lists:sort(fun(D1, D2) -> 
                              IdFieldName = <<"id">>, % ebot_db_doc:get_id_field_name(...)
                              ID1 = ebot_db_doc:doc_get_value(IdFieldName, D1), 
                              ID2 = ebot_db_doc:doc_get_value(IdFieldName, D2),
                              % error_logger:info_report({?MODULE, ?LINE, {is_content_already_indexed, Url, for_docs, Docs, id1, ID1, id2, ID2}}),
                              ID1 =< ID2
                            end, 
                            Docs),
      [Doc | _] = Docs1,
      UrlFromDoc = ebot_db_doc:doc_get_value(<<"url">>, Doc), % think on using id instead of url.

      % Only first met url with the digest is sent for indexing.
      case ebot_util:safe_list_to_binary(UrlFromDoc) == ebot_util:safe_list_to_binary(Url) of
         true -> false;        
         false -> {true, Doc}
      end

  end.



%%====================================================================
%% EUNIT TESTS
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

mo_ads_web_test() -> ok.

-endif.

