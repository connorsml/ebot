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
%%% File    : ebot_url_util.erl
%%% Author  : matteo <<matteo.redaelli@@libero.it>
%%% Description : 
%%%
%%% Created :  4 Oct 2009 by matteo <matteo@redaelli.org>
%%%-------------------------------------------------------------------
-module(ebot_url_util).
-author("matteo.redaelli@@libero.it").

%% API
-export([
	 convert_to_absolute_url/2,
	 filter_external_links/2,
     filter_internal_links/2,
	 is_external_link/2,
	 is_same_domain/2,
	 is_same_main_domain/2,
	 is_subdomain/2,
	 is_valid_image/1,
	 is_valid_link/1,
	 is_valid_url/1,
	 normalize_url/2,
	 parse_url/1,
	 url_context/1,
	 url_depth/1,
	 url_domain/1,
	 url_main_domain/1,
     url_add_final_slash/1,
     url_remove_final_slash/1,
     clear_links/1,
     escape_uri/1,
     is_site_root/1,
     add_final_slash_if_site_root/1
	]).

%%--------------------------------------------------------------------
%% Function: convert_to_absolute_url
%% Input: an url and its referral/parent url
%% Description: converts relative urls to absolute
%%--------------------------------------------------------------------
	      
convert_to_absolute_url( Url, ParentUrl) ->
    %% does the string start with http:// or https:// ?
    %% remember that in RE s? matches "s" or ""
    case re:run(Url, "^https?://") of
	{match, _L} ->
	    Url;
	nomatch ->
	    {Domain,Folder,_File,_Query} = parse_url(ParentUrl),
	    case re:run(Url, "^/") of
		{match, _L} ->
		    Domain ++ Url;
		nomatch ->
		    Domain ++ normalize_path(Folder ++ Url)
	    end
    end.

filter_external_links(Url, Links) ->
    lists:filter( fun(L) -> is_external_link(Url, L) end, Links).

filter_internal_links(Url, Links) ->
    lists:filter( fun(L) -> is_internal_link(Url, L) end, Links).

is_same_domain(Url1, Url2) ->
    url_domain(Url1) == url_domain(Url2).

is_same_main_domain(Url1, Url2) ->
    url_main_domain(Url1) == url_main_domain(Url2).

is_subdomain(Url1, Url2) ->
    is_same_main_domain(Url1, Url2)  andalso
	url_domain(Url1) =/= url_domain(Url2).
			  
is_external_link(Url1, Url2) ->
    not is_internal_link(Url1, Url2).

is_internal_link(Url1, Url2) ->
    is_same_domain(Url1, Url2).
    % LUrl1 = ebot_util:safe_binary_to_list(Url1), 
    % LUrl2 = ebot_util:safe_binary_to_list(Url2), 
    % case re:run(LUrl2, "^" ++ LUrl1, [{capture, none},caseless]) of
    %     match -> true;
    %     nomatch -> false
    % end.
    

is_valid_image(Url) when is_binary(Url) ->
    is_valid_image(binary_to_list(Url));

is_valid_image(Url) ->
    {ok, Options} =  ebot_util:get_env(is_valid_image),
    is_valid(Url, Options).

is_valid_link(Url) when is_binary(Url) ->
    is_valid_link(binary_to_list(Url));

is_valid_link(Url) ->
    {ok, Options} =  ebot_util:get_env(is_valid_link),
    is_valid(Url, Options).

is_valid_url(Url) when is_binary(Url) ->
    is_valid_url(binary_to_list(Url));

is_valid_url(Url) ->
    {ok, Options} =  ebot_util:get_env(is_valid_url),
    is_valid(Url, Options).

%%--------------------------------------------------------------------
%% Function: normalize_url/2
%% Input: an url and its options (see config files)
%% Description: converts relative urls to absolute
%%--------------------------------------------------------------------

normalize_url(Url, Options) when is_binary(Url) ->
    NewUrl = normalize_url(binary_to_list(Url), Options),
    list_to_binary(NewUrl);

normalize_url(Url, [{plugin, Module, Function}|Options]) ->
    NewUrl = Module:Function(Url),
    normalize_url(NewUrl, Options);

normalize_url(Url, [{replace_string, RElist}|Options]) ->
    NewUrl = ebot_util:string_replacements_using_regexps(Url, RElist),
    normalize_url(NewUrl, Options);

normalize_url(Url, [add_final_slash|Options]) ->
    NewUrl = url_add_final_slash(Url),
    normalize_url(NewUrl, Options);

normalize_url(Url, [{max_depth,MaxDepth}|Options]) ->
    NewUrl = url_using_max_depth(Url, MaxDepth),
    normalize_url(NewUrl, Options);

normalize_url(Url, [strip|Options]) ->
    NewUrl =  string:strip(Url,  both, $ ),
    normalize_url(NewUrl, Options);

normalize_url(Url, [without_internal_links|Options]) ->
    NewUrl = url_without_internal_links(Url),
    normalize_url(NewUrl, Options);

normalize_url(Url, [without_queries|Options]) ->
    NewUrl = url_without_queries(Url),
    normalize_url(NewUrl, Options);

normalize_url(Url, [Opt|Options]) ->
    error_logger:error_report({?MODULE, ?LINE, {normalize_url, Url, unknown_option, Opt}}),
    normalize_url(Url, Options);
normalize_url(Url, []) ->
    Url.

parse_path(Path) ->
    Sep = string:rstr(Path,"/"),
    {string:sub_string(Path,1, Sep), string:sub_string(Path, Sep + 1)}.

parse_url(Url) when is_binary(Url) ->
    parse_url( binary_to_list(Url));

parse_url(Url) ->
    case http_uri:parse(Url) of
    	{error, Result} ->
    	    {error, Result};
        % Erlang R15 clause
        {ok, {Protocol,_,Root,Port,Path,Query}} ->
            construct_parsed_url(Protocol,Root,Port,Path,Query);
    	{Protocol,_,Root,Port,Path,Query} -> 
            construct_parsed_url(Protocol,Root,Port,Path,Query)
    end.

construct_parsed_url(Protocol,Root,Port,Path,Query) ->
    %% TODO: should check protocol/port and not only port
    case Port of
    80 ->
        P = "";
    443 ->
        P = "";
    _Else ->
        P = ":" ++ integer_to_list(Port)
    end,
    Domain = atom_to_list(Protocol) ++ "://" ++ Root ++ P,
    {Folder, File} = parse_path(Path),
    {Domain,Folder,File,Query}.


url_context(Url) ->
    {Domain,Folder,_File,_Query} = parse_url(Url),
    Domain ++ Folder.

url_depth(Url) ->
    {_Domain,Folder,_File,_Query} = parse_url(Url),
    length(string:tokens(Folder, "/")).

url_domain(Url) ->
    {Domain,_,_,_} = ebot_url_util:parse_url(Url),
    
    Domain1 = case string:substr(Domain, 1, 7) of
       "http://" -> 
           string:substr(Domain, 8);
       _ -> 
           Domain
    end,

    % Domain2 = case string:substr(Domain1, 1, 4) of
    %    "www." -> 
    %        string:substr(Domain1, 5);
    %    _ -> 
    %        Domain1
    % end,

    UrlParts = re:split(Domain1,"([//])",[{return,list}]),
    [Domain2 | _] = UrlParts,
    Domain2.

url_main_domain(Url) ->
    {Domain,_,_,_} = ebot_url_util:parse_url(Url),
    [Protocol, Host] = re:split(Domain, "//", [{return,list}]),
    List = re:split(Host, "[.]", [{return,list}]),
    case lists:reverse(List) of
	[Dom1, Dom2|_] ->
	    Protocol ++ "//" ++ Dom2 ++ "." ++ Dom1;
	Else ->
	    error_logger:error_report({?MODULE, ?LINE, {url_main_domain, Url, too_short_url, Else}}),
	    {error, too_short_url}
    end.

%%====================================================================
%% EBOT_Url specific Internal functions
%%====================================================================

is_valid(Url, [{validate_any_mime_regexps, RElist}|Options]) ->
    Mime = mochiweb_util:guess_mime(Url),
    ebot_util:is_valid_using_any_regexps(Mime, RElist) andalso 
	is_valid(Url, Options);
is_valid(Url, [{validate_all_url_regexps, RElist}|Options]) ->
    ebot_util:is_valid_using_all_regexps(Url, RElist) andalso 
	is_valid(Url, Options);
is_valid(Url, [{validate_any_url_regexps, RElist}|Options]) ->
    ebot_util:is_valid_using_any_regexps(Url, RElist) andalso 
	is_valid(Url, Options);
is_valid(Url, [{plugin, Module, Function}|Options]) ->
    Module:Function(Url)  andalso 
	is_valid(Url, Options);
is_valid(_Url, []) ->
    true.

normalize_path(Path) ->
    Tokens = lists:reverse(string:tokens(Path,"/")),
    case normalize_path( Tokens, {0,[]}) of
	{ok, ""} ->
	    "/";
	{ok, NewTokens} ->
	    "/" ++ string:join(NewTokens,"/");
	{error, _} ->
	    "/"
    end.

normalize_path([".."|L], {Cont,NewList}) ->
    normalize_path(L, {Cont + 1,NewList});

normalize_path([_|L], {Cont,NewList}) when Cont > 0 ->
    normalize_path(L, {Cont - 1,NewList});

% skipping unuseful ./
normalize_path(["."|L], {Cont,NewList}) when Cont == 0 ->
    normalize_path(L, {Cont,NewList});
	
normalize_path([E|L], {Cont,NewList}) when Cont == 0 ->
    normalize_path(L, {Cont,[E|NewList]});

normalize_path([], {0,NewList}) ->
    {ok,NewList};	    
normalize_path([], {_,_}) ->
    error_logger:info_report({?MODULE, ?LINE, {normalize_path, too_many_backs}}),
    {error, too_many_backs}.

url_add_final_slash(Url) ->
    Url1 = ebot_util:safe_binary_to_list(Url),
    case re:run(Url1, "^http://.+/") of
	{match, _} ->
	    Url1;
	nomatch ->
	    Url1 ++ "/"
    end.


url_remove_final_slash(Url) ->
    Url1 = ebot_util:safe_binary_to_list(Url),
    case re:run(Url1, "(.+)/$", [{capture, all_but_first, list}]) of 
        {match, [WithoutSlash]} -> WithoutSlash;
        nomatch -> Url1
    end.


url_unparse({Domain,Folder,File,Query}) ->
    Domain ++ Folder ++ File ++ Query.

url_using_max_depth(Url, MaxDepth) ->
    Depth = url_depth(Url),
    url_using_max_depth(Url, MaxDepth, Depth).

url_using_max_depth(Url, MaxDepth, Depth) when Depth =< MaxDepth->
    Url;
url_using_max_depth(Url, MaxDepth, _Depth) ->
    {Domain,Folder,_File,_Query} = parse_url(Url),
    Tokens = string:tokens(Folder, "/"),
    NewTokens = lists:sublist(Tokens, MaxDepth),
    NewFolder = "/" ++ string:join(NewTokens,"/") ++ "/",
    NewUrl = url_unparse({Domain,NewFolder,"",""}),
    error_logger:info_report({?MODULE, ?LINE, {renamed_url, Url, NewUrl}}),
    NewUrl.

url_without_internal_links(Url) ->
    {Scheme, Netloc, Path, Query, _Fragment} = mochiweb_util:urlsplit(Url),
    mochiweb_util:urlunsplit({Scheme, Netloc, Path, Query,[]}).

url_without_queries(Url) ->
    {Scheme, Netloc, Path, _Query, _Fragment} = mochiweb_util:urlsplit(Url),
    mochiweb_util:urlunsplit({Scheme, Netloc, Path, [],[]}).


clear_links(Links) ->
  lists:foldl(fun(Url, Acc) -> 
                NoGrids = re:replace(Url, "#(.*)", "", [{return,list}]),
                [NoGrids|Acc] 
              end, 
              [], 
              Links).  

escape_uri(Url) ->
    {Scheme, Netloc, Path, Query, Fragment} = mochiweb_util:urlsplit(Url),
    
    Parts = lists:foldl(
        fun(Part, Acc) -> [edoc_lib:escape_uri(Part)| Acc] end,
        [],
        string:tokens(Path, "/")),
    Path1 = "/" ++ string:join(lists:reverse(Parts), "/"),

    QuotelessQ = re:replace(Query, "'", "%27", [global, {return, list}]),
    QuotelessQ1 = re:replace(QuotelessQ, "\"", "%22", [global, {return, list}]),    

    mochiweb_util:urlunsplit({Scheme, Netloc, Path1, QuotelessQ1, Fragment}).

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
is_site_root(Url) ->
  case ebot_url_util:parse_url(Url) of 
    {_Domain,"/",[],[]} ->
       true;
    _ ->
       false
  end.

add_final_slash_if_site_root(Url) ->
  case is_site_root(Url) of
    true -> url_add_final_slash(Url);
    false -> Url
  end.

%%====================================================================
%% EUNIT TESTS
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

ebot_url_util_test() ->
    HomeDomain = "redaelli.org",
    Domain = "http://www.redaelli.org",
    Home = "http://www.redaelli.org/",
    Utest = "http://www.redaelli.org/matteo/ebot_test/",
    Udir1 = "http://www.redaelli.org/matteo/ebot_test/dir1/",
    Udir11 = "http://www.redaelli.org/matteo/ebot_test/dir1/dir11/",
    Utyre1 = "http://www.tyres-pneus-online.co.uk/car-tyres-FORTUNA/F1000/155,65,R13,73,T.html",

     ?assertEqual(Home, url_add_final_slash(Domain)),

     ?assertEqual(Home, convert_to_absolute_url("../../", Utest)),

     ?assertEqual(0, ebot_url_util:url_depth(Domain)),
     ?assertEqual(0, ebot_url_util:url_depth(Home)),
     ?assertEqual(0, ebot_url_util:url_depth(Home ++ "index.html")),
     ?assertEqual(1, ebot_url_util:url_depth(Home ++ "matteo/index.html")),
     ?assertEqual(2, ebot_url_util:url_depth(Utest)),
     ?assertEqual(3, ebot_url_util:url_depth(Udir1)),
     ?assertEqual(4, ebot_url_util:url_depth(Udir11)),
     ?assertEqual(2, ebot_url_util:url_depth(Utyre1)),

     ?assertEqual(HomeDomain, url_domain(Home)),
     ?assertEqual(HomeDomain, url_domain(Utest)),
     ?assertEqual(HomeDomain, url_domain(Udir1)),

     ?assertEqual(Utest, url_using_max_depth(Udir1, 2)),
     ?assertEqual(Utest, url_using_max_depth(Udir11, 2)),
     ?assertEqual(Udir1, url_using_max_depth(Udir1, 3)),
     ?assertEqual(Udir11, url_using_max_depth(Udir11, 4)),
     ?assertEqual(Utyre1, url_using_max_depth(Utyre1, 4)),
     ?assertEqual(Utyre1, url_using_max_depth(Utyre1, 4)),
     ?assertEqual(Utyre1, url_using_max_depth(Utyre1, 2)),
     ?assertEqual("http://www.tyres-pneus-online.co.uk/car-tyres-FORTUNA/", url_using_max_depth(Utyre1, 1)),

     ?assertEqual(true, is_external_link( <<"http://github.com/matteoredaelli/ebot">>,  
					  <<"http://www.redaelli.org/">>)),
     ?assertEqual(false, is_external_link( <<"http://www.redaelli.org/matteo/">>,  
					  <<"http://www.redaelli.org/">>)),
     ?assertEqual(true, is_internal_link( <<"http://www.redaelli.org/matteo/">>,  
                      <<"http://www.redaelli.org/">>)),
     ?assertEqual(true, is_internal_link( <<"http://redaelli.org/matteo/">>,  
                      <<"http://www.redaelli.org/">>)),


     ?assertEqual(false, is_same_domain( <<"http://github.com/matteoredaelli/ebot">>,  
					  <<"http://www.redaelli.org/">>)),
     ?assertEqual(true, is_same_domain( <<"http://www.redaelli.org/matteo/">>,  
					  <<"http://www.redaelli.org/">>)),
     ?assertEqual(false, is_same_domain( <<"http://redaelli.org/">>,  
                      <<"http://www.redaelli.org/">>)),

     ?assertEqual( "http://redaelli.org", url_main_domain(<<"http://www.redaelli.org/aa/">>)),
     ?assertEqual( "http://redaelli.org", url_main_domain(<<"http://www.matteo.redaelli.org/aa/a">>)),
     ?assertEqual( "http://redaelli.org", url_main_domain(<<"http://redaelli.org/aa/a">>)),

     ?assertEqual(true, is_same_main_domain(<<"http://www.redaelli.org/matteo/">>,  
					    <<"http://matteo.redaelli.org/">>)),
     ?assertEqual(true, is_same_main_domain(<<"http://redaelli.org/matteo/">>,  
					    <<"http://matteo.redaelli.org/">>)),
     ?assertEqual(true, is_same_main_domain(<<"http://redaelli.org/matteo/">>,  
					    <<"http://www.matteo.redaelli.org/">>)),
     ?assertEqual(false, is_same_main_domain(<<"http://redaelli.org/matteo/">>,  
					     <<"http://matteoredaelli.wordpress.com/">>)),
     
     ?assertEqual(true, is_subdomain(<<"http://redaelli.org/matteo/">>, 
				     <<"http://www.matteo.redaelli.org/">>)),
     ?assertEqual(true, is_subdomain(<<"http://aaa.redaelli.org/matteo/">>,  
				     <<"http://www.matteo.redaelli.org/">>)),
     ?assertEqual(false, is_subdomain(<<"http://www.redaelli.org/matteo/blog/">>,  
				      <<"http://www.redaelli.org/">>)),
     ?assertEqual(false, is_subdomain(<<"http://www.redaelli.org/matteo/">>,  
				      <<"http://matteoredaelli.wordpress.com/">>)),

    test_site_root().

test_site_root() ->
  ?assertEqual(is_site_root(<<"http://nnov.ru">>), true),
  ?assertEqual(is_site_root(<<"http://03pc.nnov.ru">>), true),
  ?assertEqual(is_site_root(<<"http://43pc.03pc.nnov.ru">>), true).

-endif.
