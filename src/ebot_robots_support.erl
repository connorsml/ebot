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

-module(ebot_robots_support).

%% API
-export([
     is_url_allowed_for_agents/3,
     robots_source/1,
     retain_allowed_links/4
    ]).


robots_source(HostUrl) ->
   HostUrl1 = ebot_util:safe_binary_to_list(HostUrl),
   
   case httpc:request(HostUrl1 ++ "/robots.txt") of

     {_Status, {{_Status1, Code, _Status2}, _, Body}} ->

       case Code of
         200 ->
           Body1 = ebot_util:safe_binary_to_list(Body),
           case is_valid_robots_txt(Body1) of
              true -> Body1;
              false -> <<"">>
           end;

         _ -> 
           <<"">>
        end;

     _ ->
       <<"">>
   end.

parse_robots_txt(Url, RobotsSource) ->
   RobotsSource1 = ebot_util:safe_binary_to_list(RobotsSource),
   case is_valid_robots_txt(RobotsSource1) of 
      true ->
         % Settings = lists:map(fun string:strip/1, string:tokens(RobotsSource1, "\r\n")),
         % Settings1 = lists:filter(fun(S) -> 
         %                            length(S) > 0 
         %                          end, 
         %                          Settings),
         % error_logger:error_report({?MODULE, ?LINE, {parse_robots_txt, url, Url, after_filtering_empty, Settings1}}),

         % Settings2 = lists:filter(fun(S) ->
         %            case re:run(S, "^#(.*)" , [{capture, none}]) of
         %              match -> false;
         %              _ -> true
         %            end
         %           end, 
         %           Settings1),
         % error_logger:error_report({?MODULE, ?LINE, {parse_robots_txt, url, Url, after_filtering_comments, Settings2}}),
         % SplitPair = fun(Str) ->
         %    Pair = lists:map(fun string:strip/1, string:tokens(Str, ":")),
         %    if 
         %      length(Pair) == 1 -> 
         %        [Key|_] = Pair,
         %        {Key, ""};
         %      true -> 
         %        [Key, Value|_] = Pair,
         %        {Key, Value}
         %    end
         % end,

         % Pairs = lists:map(SplitPair, Settings2),
         % Pairs1 = lists:map(fun(Pair) -> {Key, Value} = Pair, {Key, Value} end, Pairs),

         % re:run("sfsdf : dir \n .....User-agent : mobot \n ..............Allowed:          ", "([A-za-z\-]+)\s*:\s*(.*)" , [global, {capture, all_but_first, list}]).
         
         RobotsSource2 = lists:map(fun string:strip/1, string:tokens(RobotsSource1, "\r\n")),
         RobotsSource3 = lists:filter(fun(S) ->
                              case re:run(S, "^#(.*)" , [{capture, none}]) of
                                match -> false;
                                _ -> true
                              end
                             end, 
                             RobotsSource2),
         RobotsSource4 = string:join(RobotsSource3, "\n"),
         case re:run(RobotsSource4, "([A-za-z\-]+)\s*:\s*(.*)", [global, {capture, all_but_first, list}]) of
            {match, Pairs} -> 
                    error_logger:error_report({?MODULE, ?LINE, {parse_robots_txt, url, Url, pairs, Pairs}}),
                    Pairs1 = lists:map(fun([Key, []]) -> {Key, ""};
                                           ([Key, Value]) -> {Key, string:strip(Value)} 
                                        end,
                                        Pairs),

                    try scan_agent_settings(Pairs1, {}, []) of
                       Settings3 -> 
                        {ok, Settings3}
                    catch
                       Reason ->
                          error_logger:error_report({?MODULE, ?LINE, {parse_robots_txt_error, url, Url, reason, Reason, robots, RobotsSource}}),
                          no_settings
                    end;

            _ -> error_logger:error_report({?MODULE, ?LINE, {no_settings_for_url, Url, robots, RobotsSource}}),
            no_settings
         end;

     false -> 
       no_settings
 end.

% list has become empty
scan_agent_settings([], AgentAcc, TotalAcc) -> 
  {Agent, Settings} = AgentAcc,
  lists:reverse([ {Agent, lists:reverse(Settings) } | TotalAcc]);

% new user-agent
scan_agent_settings([ {"User-agent", UserAgent} | Rest], {}, TotalAcc) ->
  scan_agent_settings(Rest, {UserAgent, []}, TotalAcc);

% another user-agent met, saving current
scan_agent_settings([ {"User-agent", UserAgent} | Rest], AgentAcc, TotalAcc) ->
  {Agent, Settings} = AgentAcc,
  scan_agent_settings(Rest, {UserAgent, []}, [ {Agent, lists:reverse(Settings) } | TotalAcc]);

% collecting current user-agent params
scan_agent_settings([ {Key, Value} | Rest], AgentAcc, TotalAcc) ->
  {Agent, Settings} = AgentAcc,
  scan_agent_settings(Rest, {Agent, [ {Key, Value} | Settings]}, TotalAcc).

find_settings_for_agent(Settings, UserAgent) ->
  case lists:keysearch(UserAgent, 1, Settings) of
     {value, AgentSettings} -> 
        {_, AgentSettings1} = AgentSettings,
        AgentSettings1;
     false -> []
  end.

url_matched(Url, Pattern) ->
  Url1 = remove_url_prefix(Url),

  Pattern1 = "^" ++ re:replace(Pattern, "\\*" , ".*", [{return,list}]),
  Pattern2 = re:replace(Pattern1, "\/$" , "", [{return,list}]),
  Res = re:run(Url1, Pattern2, []),
  case Res of 
     {match, _} -> true;
     nomatch  -> false
  end.

 is_url_allowed_for_agents(Settings, Url, Agents) ->
    lists:any(fun(Agent) -> 
                AgentSettings = find_settings_for_agent(Settings, Agent),
                if 
                   length(AgentSettings) =/= 0 ->
                      is_url_allowed(Url, AgentSettings);
                   true ->
                      false
                end
              end,
              Agents).

 is_url_allowed(_, []) ->
    true;

 is_url_allowed(Url, [{"Disallow", Value}|Settings]) ->
    case Value of
      [] -> true;
      Else ->
        try url_matched(Url, Else) of
          true ->
            error_logger:info_report({?MODULE, ?LINE, {rejecting, Url, disallow_reason, Else}}),
            false;
          false ->
            is_url_allowed(Url, Settings)
        catch 
          Reason ->
            error_logger:info_report({?MODULE, ?LINE, {is_url_allowed_failed, Url, reason, Reason}}),
            true
        end
    end;

 is_url_allowed(Url, [{"Allow", Value}|Settings]) ->
    try url_matched(Url, Value) of
      true ->
        error_logger:info_report({?MODULE, ?LINE, {accepting, Url, allow_reason, Value}}),      
        true;
      false ->
        is_url_allowed(Url, Settings)
    catch
        Reason ->
          error_logger:info_report({?MODULE, ?LINE, {is_url_allowed_failed, Url, reason, Reason}}),
          true
    end;
 
 is_url_allowed(Url, [{_Option, _Value}|Settings]) ->
    is_url_allowed(Url, Settings).


remove_url_prefix(Url) ->
  re:replace(Url, "(http://)*([^\/]+)", "",  [{return,list}]).


retain_allowed_links(Url, RobotsTxt, Links, Agents) ->
  case catch parse_robots_txt(Url, RobotsTxt) of
    {ok, Settings} ->
       lists:filter(fun(Link) -> is_url_allowed_for_agents(Settings, Link, Agents) end, Links);
    no_settings ->
       Links;
    {'EXIT', {{badmatch,_, Where}}} ->
       error_logger:info_report({?MODULE, ?LINE, {retain_allowed_links_bad_match, url, Url, where, Where}}),
       Links;
    {'EXIT', Reason} ->
       error_logger:info_report({?MODULE, ?LINE, {retain_allowed_links_exit, url, Url, reason, Reason}}),
       Links
  end.

is_valid_robots_txt(RobotsTxt) ->
  case re:run(RobotsTxt, "(\s)*User-agent", [dotall, {capture, all, list}]) of 
    {match, _} -> true;
    nomatch -> false
  end.

%%====================================================================
%% EUNIT TESTS
%%====================================================================
-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

ebot_robots_support_test() ->
  % test_getting_robots(),
  test_parsing_robots(),  
  test_disallow().

test_disallow() ->
  Host = "http://server.com",
  Settings = [{"Disallow", "/admin"}, {"Disallow", "/photo/"}],
  ?assertEqual(is_url_allowed(Host ++ "/admin", Settings), false),
  ?assertEqual(is_url_allowed(Host ++ "/photo/", Settings), false),
  ?assertEqual(is_url_allowed(Host ++ "/user", Settings), true),

  Settings1 = [{"Disallow", "/*/admin"}],
  ?assertEqual(is_url_allowed(Host ++ "/admin", Settings1), true),  
  ?assertEqual(is_url_allowed(Host ++ "/suburl/admin", Settings1), false),  
  ?assertEqual(is_url_allowed(Host ++ "/suburl/admin/tail", Settings1), false),


  RobotsTxt = "User-agent: * \n Disallow: /admin \n Disallow: /photo \n",
  {ok, Settings2} = parse_robots_txt("", RobotsTxt),
  ?assertEqual(is_url_allowed_for_agents(Settings2, Host ++ "/admin", ["*"]), false),
  ?assertEqual(is_url_allowed_for_agents(Settings2, Host ++ "/admin", ["*", "Bot"]), false).


test_parsing_robots() ->
  Robots_txt = "User-agent: * \n Disallow: /sendto_form \n Disallow: /folder_factories \n  Disallow: /search-tag \n  Disallow: /info/ \n  Disallow: /*/info/ \n   Crawl-delay: 1 \n Clean-param: searchterm / \n User-agent: Mobot \n Crawl-delay: 0.5 \n",

  {ok, Settings} = parse_robots_txt("", Robots_txt),
  AgentSettings = find_settings_for_agent(Settings, "*"),
  ?assertEqual([{"Disallow", "/sendto_form"}, 
                {"Disallow", "/folder_factories"}, 
                {"Disallow", "/search-tag"}, 
                {"Disallow", "/info/"}, 
                {"Disallow", "/*/info/" },
                {"Crawl-delay","1"},
                {"Clean-param","searchterm /"} ], 
                AgentSettings),

  Robots_txt1 = "Very bad robots.txt content",
  ?assertEqual(no_settings, parse_robots_txt("", Robots_txt1)).


% test_getting_robots() ->
%   {Status, _} = robots_source("http://google.com/"),
%   ?assertEqual(Status, ok).

-endif.  