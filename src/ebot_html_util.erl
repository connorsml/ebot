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
%%% File    : ebot_html_util.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  3 Apr 2010 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_html_util).

%% API
-export([
	 get_links/2,
	 get_links_from_tokens/2,
	 get_images/2,
	 get_images_from_tokens/2,
	 get_start_tags_attributes/2,
	 get_start_tags_attributes/3,
	 get_start_tags_data/2,
	 decode_html_to_utf8/3,
   try_decode_bad_html_to_utf8/3
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: get_links_from_tokens 
%% Description:
%%--------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% IMG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% {start_tag,<<"img">>,
%%            [{<<"src">>,<<"images/matteo.jpg">>}],
%%            true},
%% {start_tag,<<"img">>,
%%            [{<<"class">>,<<"image">>},
%%             {<<"src">>,
%%              <<"http://upload.wikimedia.org/wikipedia/commons/thumb/2/21/Carate_bria"...>>},
%%             {<<"width">>,<<"120">>},
%%             {<<"alt">>,<<"Icon">>}],
%%            true}]

get_images(Html, ParentUrl) ->
    Tokens = mochiweb_html:tokens(Html),
    get_images_from_tokens(Tokens, ParentUrl).

get_images_from_tokens(Tokens, ParentUrl) when is_binary(ParentUrl) ->
    Images = get_images_from_tokens(Tokens, binary_to_list(ParentUrl)),
    lists:map( fun list_to_binary/1, Images);
get_images_from_tokens(Tokens, ParentUrl) ->
    Urls = lists:flatten(get_start_tags_attributes(Tokens, <<"img">>, <<"src">>)),
    List = lists:foldl(
	     fun(Url, Links) -> 
		     case ebot_url_util:is_valid_image(Url) of
			 true ->
			     AbsoluteUrl = ebot_url_util:convert_to_absolute_url( 
					     binary_to_list(Url), 
					     ParentUrl
					    ),
			     [AbsoluteUrl|Links];
			 false ->
			     Links
		     end
	     end,
	     [], 
	     Urls
	    ),
    ebot_util:remove_duplicates(List).

get_links(Html, ParentUrl) ->
    Tokens = mochiweb_html:tokens(Html),
    get_links_from_tokens(Tokens, ParentUrl).

get_links_from_tokens(Tokens, ParentUrl) when is_binary(ParentUrl) ->
    Links = get_links_from_tokens(Tokens, binary_to_list(ParentUrl)),
    lists:map( fun list_to_binary/1, Links);
get_links_from_tokens(Tokens, ParentUrl) ->
    Urls = lists:flatten(get_start_tags_attributes(Tokens, <<"a">>, <<"href">>)),
    List = lists:foldl(
	     fun(Url, Links) -> 
		     case ebot_url_util:is_valid_link(Url) of
			 true ->
			     AbsoluteUrl = ebot_url_util:convert_to_absolute_url( 
					     binary_to_list(Url), 
					     ParentUrl
					    ),
			     [AbsoluteUrl|Links];
			 false ->
			     Links
		     end
	     end,
	     [], 
	     Urls
	    ),
    ebot_util:remove_duplicates(List).
    
%%====================================================================
%% Internal functions
%%====================================================================

%% [{<<"href">>, url1},{<<"img">>, file}]
%% there should be only one attribute with that name, but 
%% to be suse, will be returned a list
 
get_attribute_value_list(Attributes, Attribute) ->
    lists:foldl(
      fun(Elem, Results) -> 
	      case Elem of 
		  {Attribute, Value} ->
		      [Value|Results];
		  _Else ->
		      Results
	      end
      end,
      [], 
      Attributes
     ).

get_start_tags_indexes(Tokens, TagName) ->
    lists:foldl(
      fun(Index, Results) -> 
	      Token = lists:nth(Index, Tokens),
	      case Token of 
		  {start_tag,TagName,_,_} ->
		      [Index|Results];
		  _Else ->
		      Results
	      end
      end,
      [], 
      lists:seq(1, length(Tokens))
     ).

get_start_tags_attributes(Tokens, TagName) ->
    Indexes = get_start_tags_indexes(Tokens, TagName),
    lists:map(
      fun(Index) ->
	      {start_tag, TagName, Values, _} = lists:nth(Index, Tokens),
	      Values
      end,
      Indexes).

get_start_tags_attributes(Tokens, TagName, Attribute) ->
    AllTagsAttributes = get_start_tags_attributes(Tokens, TagName), 
    DeepList = lists:foldl(
		 fun(TagAttributes, Results) ->
			 NewValues =  get_attribute_value_list(TagAttributes, Attribute),
			 case NewValues of 
			     [] ->
				 Results;
			     _Else ->
				 [NewValues|Results]
			 end
		 end,
		 [], 
		 AllTagsAttributes
		),
    lists:flatten(DeepList).

get_start_tags_data(Tokens, TagName) ->
    Indexes = get_start_tags_indexes(Tokens, TagName),
    DeepList = lists:foldl(
		 fun(Index, Results) ->
			 case lists:nth(Index + 1, Tokens) of
			     {data, Data, _Whitespace} ->
				 [Data|Results];
			     _Else ->
				 error_logger:warning_report({?MODULE, ?LINE, {get_start_tags_data, data_token_notFound}}),
				 Results
			 end
		 end,
		 [],
		 Indexes),
    lists:flatten(DeepList).


% uniconvert(String) ->
%   try xmerl_ucs:from_utf8(String) of
%     _ ->
%       list_to_binary(String)
%   catch
%     exit:{ucs,{bad_utf8_character_code}} ->
%       list_to_binary(xmerl_ucs:to_utf8(String))
%   end.

%%====================================================================
%% Internal encoding-related functions.
%% Use https://github.com/Vagabond/erlang-iconv.
%%====================================================================

%%====================================================================
%% Converts Content into utf-8 according to the meta containing either
%% in <meta> tag or in the header. If not found, the Content is assumed
%% to be in utf-8 and all other symbols are thrown out off it.
%%====================================================================
convert_to_utf8_with_enconv(Bin) ->
  os:cmd("echo \"" ++ escape_quotes(Bin) ++ "\" | enconv -L ru -x utf8").

convert_to_utf8_with_iconv(FromEnc, Data) ->
  os:cmd("echo \"" ++ escape_quotes(Data) ++ "\" | iconv -f " ++ FromEnc ++ " -t utf8").

check_if_utf8(Html) ->
  Converted = os:cmd("echo \"" ++ escape_quotes(Html) ++ "\" | iconv -f utf8 -t utf8"),
  % case binary:match(Converted, <<"iconv: illegal input sequence at position">>) of
  %   nomatch -> true;
  %   _ -> false
  % end.
  Res = case re:run(Converted, "iconv: illegal input sequence at position", [global, {capture, none,list}]) of
    match -> false;
    _ -> true
  end,
  % error_logger:info_report({?MODULE, ?LINE, check_if_utf8, data, Html, result, Res}),
  Res.

decode_html_to_utf8(_Url, Data, _Headers) ->
  convert_to_utf8_with_enconv(Data).

try_decode_bad_html_to_utf8(Url, Data, Headers) ->
  % META tag has the highest priority.
  Converted = case extract_encoding_from_meta(Data) of
    {ok, Enc} -> 
                Data1 = convert_to_utf8_with_iconv(Enc, Data),
                case check_if_utf8(Data1) of
                   true -> 
                      error_logger:info_report({?MODULE, ?LINE, decode_html_to_utf8, url, Url, using_iconv, enc, Enc}),
                      {ok, Data1};
                   false -> {error, empty}
                end;

    {error, not_found} -> 
        error_logger:info_report({?MODULE, ?LINE, decode_html_to_utf8, url, Url, no_encoding_from_meta}),
        {error, empty}
  end,

  Converted1 = case Converted of 
    {ok, Data2} -> {ok, Data2};

    {error, empty} -> 

       case extract_encoding_from_headers(Headers) of

         {ok, Enc1} ->
                Data2 = convert_to_utf8_with_iconv(Enc1, Data),
                case check_if_utf8(Data2) of
                   true -> 
                      error_logger:info_report({?MODULE, ?LINE, decode_html_to_utf8, url, Url, using_iconv, enc, Enc1}),
                      {ok, Data2};
                   false -> 
                      Data3 = convert_to_utf8_with_enconv(Data),
                      case check_if_utf8(Data3) of
                        true -> 
                            error_logger:info_report({?MODULE, ?LINE, decode_html_to_utf8, url, Url, using_enconv, 1}),
                            {ok, Data3};
                        false -> 
                           error_logger:info_report({?MODULE, ?LINE, decode_html_to_utf8, url, Url, retaining_ascii, 1}),
                           {ok, retain_ascii(binary_to_list(Data)) }
                      end

                end;

          {error, not_found} -> 
                Data4 = convert_to_utf8_with_enconv(Data),
                case check_if_utf8(Data4) of
                  true -> 
                     error_logger:info_report({?MODULE, ?LINE, decode_html_to_utf8, url, Url, using_enconv, 2}),
                     {ok, Data4};
                  false -> 
                      Data6 = retain_ascii(binary_to_list(Data)),
                      case check_if_utf8(Data6) of
                        true -> 
                            error_logger:info_report({?MODULE, ?LINE, decode_html_to_utf8, url, Url, retaining_ascii, 2}),
                            {ok, Data6};
                        false -> {error, not_converted}
                      end
                end
       end
  end,

  case Converted1 of
    {ok, Data5} -> 
         % error_logger:info_report({?MODULE, ?LINE, decode_html_to_utf8, data, Data, headers, Headers, result, Data7}),
         {ok, ebot_util:safe_list_to_binary(Data5)};

    Else -> 
        error_logger:info_report({?MODULE, ?LINE, decode_html_to_utf8, url, Url, empty_result}),
        Else
  end.


retain_ascii(Str) -> 
  retain_utf8(Str).


extract_encoding_from_content_type(CType) ->
   case re:run(CType, "charset=([a-zA-z0-9-]+)", [{capture, [1], list}, caseless]) of
   	 {match, [Enc]} -> {ok, Enc};
   	 nomatch -> {error, not_found}
   end.

extract_encoding_from_headers(Headers) ->
   error_logger:warning_report({?MODULE, ?LINE, {extract_encoding_from_headers, Headers}}),  
   case lists:filter(fun({Name, _}) -> Name == "content-type" end, Headers) of
      [] -> {error, not_found};
      [{_, Enc}] -> extract_encoding_from_content_type(Enc)
   end.


extract_encoding_from_meta(Body) ->
  Body1 = ebot_util:safe_binary_to_list(Body),
  case re:run(Body1, "<meta\.+charset=([a-zA-z0-9-]+)", [dotall, {capture, [1], list}, caseless]) of
    {match, [Enc]} -> {ok, Enc};
    nomatch -> {error, not_found}
  end.

escape_quotes(Data) when is_binary(Data) ->
  Data1 = binary:replace(Data, <<"\"">>, <<"\\\"">>, [global]),
  Data2 = binary:replace(Data1, <<"$">>, <<"\\$">>, [global]),
  binary_to_list(Data2);

escape_quotes(Data) when is_list(Data) ->
  Data1 = re:replace(Data, "\"", "\\\"", [global, {return,list}]),
  re:replace(Data1, "$", "\\$", [global, {return,list}]).


% extract_encoding_from_meta(Tokens) -> 
%   case get_start_tags_attributes(Tokens, <<"meta">>) of
%    	[Meta] ->
% 	   Meta1 = lists:flatten(Meta),
% 	   Meta2 = lists:foldl(fun({Header, Content}, Acc) ->
% 	   	                     if 
% 		   	                    Header == <<"content">> ->
% 									case extract_encoding(ebot_util:safe_binary_to_list(Content)) of 
% 										{ok, Enc} -> [Enc | Acc];
% 										{error, not_found} -> Acc
% 									end;
								
% 								true -> Acc
% 							  end
% 							end,
% 							[],
% 				            Meta1),
	   
% 	   case Meta2 of
% 	   	  [Enc] -> {ok, Enc};
% 	   	  _ ->
% 			  {error, not_found}
% 	   end;

% 	_ -> 
% 	  {error, not_found}
%   end.

retain_utf8(String) ->
  retain_encoding(String, 'utf-8').

retain_encoding(String, Enc) ->
  String1 = lists:foldl(fun(Sym, Acc) ->
    try xmerl_ucs:is_incharset(Sym, Enc) of
      true -> [Sym | Acc];
      _ -> Acc
    catch
       exit:{ucs, {bad_utf8_character_code}} -> Acc
    end
  end,
  [],
  ebot_util:safe_binary_to_list(String)),
  list_to_binary(lists:reverse(String1)).

%%====================================================================
%% EUNIT TESTS
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

ebot_html_util_test() ->
    Url = <<"http://www.redaelli.org/">>,
    Html = <<"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"> 
<html xmlns=\"http://www.w3.org/1999/xhtml\"> 
<head> 
<title>Fratelli Redaelli</title>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /> 
<title>Redaelli.Org</title> 
<meta name=\"keywords\" content=\"redaelli,carate brianza,opensource,linux,italy\" /> 
<meta name=\"description\" content=\"Website dei Fratelli Redaelli\" /> 
<link href=\"templatemo_style.css\" rel=\"stylesheet\" type=\"text/css\" /> 
<meta name=\"google-site-verification\" content=\"SdYRvDUubCbqDXBUbur7qnC1Gh9cmVC3GUisrpqGBT0\" /> 
</head> 
<body> 
<img src=\"images/fratelli-redaelli.jpg\" /> 
<img class=\"image\" src=\"http://upload.wikimedia.org/test.JPG\" width=\"120\" alt=\"Icon\" /> 
                    <ul class=\"templatemo_list bullet_arrow\"> 
                    	<li><a href=\"http://www.alpinicarate.it\">Alpini Carate</a></li> 
						<li><a href=\"matteo/\">Matteo</a></li> 
						<li><a href=\"http://www.vvfcarate.it/\">Vigili del Fuoco</a></li> 
                    </ul> 
</body> 
</html>">>,
    Tokens = mochiweb_html:tokens(Html),

    ?assertEqual( [<<"http://www.alpinicarate.it">>,
		   <<"http://www.redaelli.org/matteo">>,
		   <<"http://www.vvfcarate.it/">>],
		   get_links_from_tokens(Tokens, Url)),

    ?assertEqual( [<<"http://upload.wikimedia.org/test.JPG">>,
		   <<"http://www.redaelli.org/images/fratelli-redaelli.jpg">>],
		  get_images_from_tokens(Tokens, Url)),

    ?assertEqual( [[{<<"name">>,<<"google-site-verification">>},
		    {<<"content">>,
		     <<"SdYRvDUubCbqDXBUbur7qnC1Gh9cmVC3GUisrpqGBT0">>}],
		   [{<<"name">>,<<"description">>},
		    {<<"content">>,
		     <<"Website dei Fratelli Redaelli">>}],
		   [{<<"name">>,<<"keywords">>},
		    {<<"content">>,
		     <<"redaelli,carate brianza,opensource,linux,italy">>}],
		   [{<<"http-equiv">>,<<"Content-Type">>},
		    {<<"content">>,
		     <<"text/html; charset=utf-8">>}]
		  ],
    		  get_start_tags_attributes(Tokens, <<"meta">>)).

-endif.
