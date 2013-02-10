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
%%% File    : ebot_web_util.erl
%%% Author  : matteo <<matteo.redaelli@@libero.it>
%%% Description : 
%%%
%%% Created :  4 Oct 2009 by matteo <matteo@redaelli.org>
%%%-------------------------------------------------------------------
-module(ebot_web_util).
-author("matteo.redaelli@libero.it").

-include("ebot.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
	 fetch_url_with_only_html_body/1,
	 fetch_url_links/1,
   is_url_text_html_mime/1
	]).

%%--------------------------------------------------------------------
%% Functions:  
%%
%% Description: Handling call messages
%%--------------------------------------------------------------------
is_url_text_html_mime(Url) ->
  Result = fetch_url(Url, head),
  case Result of
    {ok, {{_, RetCode, _}, Headers, _}} -> 
        case RetCode of
          200 -> {ok, {Url, is_text_html_mime_url(Headers)}};
          404 -> {ok, {Url, not_found}};
          301 -> 
              case is_internal_redirect(Url, Headers) of
                 {true, NewUrl} -> {ok, {NewUrl, is_text_html_mime_url(Headers)}};
                 false -> {error, {external_redirect}}
              end;
          302 ->
              case is_internal_redirect(Url, Headers) of
                 {true, NewUrl} -> {ok, {NewUrl, is_text_html_mime_url(Headers)}};
                 false -> {error, {external_redirect}}
              end;
          Else ->
            {error, {Else}}
        end;
    {error, Reason} -> 
        % If we failed to get headers, let's assume the content has the wrong type.
        {error, {Reason}}
  end.


fetch_url_with_only_html_body(Url) ->
  fetch_content(Url).
  
fetch_url_links(URL) ->
    case fetch_url(URL, get) of
    	{error, Reason} ->
    	    {error, Reason};
    	{ok, {_Status, _Headers, Body}} -> 
    	    Links = ebot_html_util:get_links(Body, URL),
    	    {ok, Links}
    end.
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
fetch_url(Url, Command) when is_binary(Url) ->
    fetch_url(binary_to_list(Url), Command);
fetch_url(Url, Command) ->
    {ok, Http_header} = ebot_util:get_env(web_http_header),
    {ok, Request_options} = ebot_util:get_env(web_request_options),
    {ok, Http_options} = ebot_util:get_env(web_http_options),

   %   Http_header = [
   %        {"User-Agent", "Mozilla/5.0 ebot/1.0-snapshot"},
   %        {"Accept-Charset", "utf-8"},
   %        {"Accept", "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8"},
   %        {"Accept-Language", "en-us,en;q=0.5"}
   %       ],


   % Request_options = [],

   % Http_options = [ {autoredirect, false}, 
   %                  {timeout, 10000} ],

    try 
		% case httpc:request(Command, {Url,Http_header},Http_options,[{sync, false}|Request_options]) of
		%     {ok, RequestId} ->
		% 		receive 
		% 		    {http, {RequestId, Result}} -> 
		% 				case Result of 
		% 				    {error, AReason} ->
		% 						error_logger:error_report({?MODULE, ?LINE, {fetch_url, Url, error, AReason}}),
		% 						Result;
		% 				    Result ->
		% 						{ok, Result}
		% 				end
		% 		after ?EBOT_WEB_TIMEOUT
		% 		      -> 
		% 			error_logger:error_report({?MODULE, ?LINE, {fetch_url, Url, timeout}}),
		% 			{error, timeout} 
		% 		end;
		%     Error ->
		% 		error_logger:error_report({?MODULE, ?LINE, {fetch_url, Url, error, Error}}),
		% 		Error
		% end

  		Request_options1 = [{sync, true}|Request_options],
  		Request_options2 = [{body_format, binary}|Request_options1],

  		case httpc:request(Command, {Url,Http_header},Http_options,Request_options2) of

  			{ok, Result} -> 
  				 % error_logger:error_report({?MODULE, ?LINE, {fetch_url, Url, content, Result}}),
  			     {ok, Result};
      			    
  		    {error, AReason} ->
  				% error_logger:error_report({?MODULE, ?LINE, {fetch_url, Url, error, AReason}}),
  				{error, AReason};

  		    Error ->
  				error_logger:error_report({?MODULE, ?LINE, {fetch_url, Url, error, Error}}),
  				Error
  				
  		end
		
    catch
		  Reason -> 
		    error_logger:error_report({?MODULE, ?LINE, {fetch_url, Url, cannot_fetch_url, Reason}}),
		    {error, Reason}
    end.

is_text_html_mime_url(Headers) ->
    Contenttype = proplists:get_value("content-type", Headers),
    case Contenttype of
	undefined ->
	    false;
	Contenttype ->
	    case re:run(Contenttype, "^text/html",[{capture, none},caseless] ) of
		match ->
		    true;
		nomatch ->
		    false
	    end
    end.


fetch_content(Url) -> 
  case fetch_url(Url, get) of
    {ok, {Status, Headers, Content}} ->
      {ok, {Status, Headers, ebot_html_util:decode_html_to_utf8(Url, Content, Headers)}};
    Else -> 
      error_logger:warning_report({?MODULE, ?LINE, {fetch_url_with_only_html_body, Url, else, Else}}),
      Else
  end.

is_internal_redirect(Url, Headers) ->
  case lists:keyfind("location", 1, Headers) of
    {_, Location} -> 
         case ebot_url_util:is_same_domain(Location, Url) of
            true -> {true, Location};
            false -> false
        end;
    false -> false
  end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

ebot_web_util_test() ->
    Url =  <<"http://www.redaelli.org/matteo/ebot_test/">>,

    H1 = [{"connection","Keep-Alive"},
	  {"content-type","text/html"},
	  {"keep-alive","timeout=15, max=99"}],

    H2 = [{"connection","Keep-Alive"},
	  {"content-length","725"},
	  {"content-type","text/txt"}],

    ?assertEqual(true, is_text_html_mime_url(H1)),
    ?assertEqual(false, is_text_html_mime_url(H2)),

    {ok,{_Status, Headers, _Body}} = fetch_url_with_only_html_body(Url),
    ?assertEqual(true, is_text_html_mime_url(Headers)),

    ExpectedUrlLinks = [<<"http://code.google.com/p/oreste/">>,
			     <<"http://github.com/matteoredaelli/ebot">>,
			     <<"http://www.redaelli.org/">>,
			     <<"http://www.redaelli.org/matteo">>,
			     <<"http://www.redaelli.org/matteo/ebot_test/dir1">>
		       ],
    UrlLinks = fetch_url_links(Url),
    ?assertEqual( {ok, ExpectedUrlLinks}, UrlLinks),
    ExpectedExternalLinks = [<<"http://code.google.com/p/oreste/">>,
			     <<"http://github.com/matteoredaelli/ebot">>],
    ?assertEqual(ExpectedExternalLinks,  
		 ebot_url_util:filter_external_links(Url, ExpectedUrlLinks)).

-endif.
