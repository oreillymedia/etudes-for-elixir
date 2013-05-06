-module(weather).
-behaviour(gen_server).
-include_lib("xmerl/include/xmerl.hrl").
-export([start_link/0]). % convenience call for startup
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]). % gen_server callbacks
-define(SERVER, ?MODULE). % macro that just defines this module as server

%%% convenience method for startup
start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks
init([]) ->
  inets:start(),
  {ok, []}.

handle_call(Request, _From, State) ->
  {Reply, NewState} = get_weather(Request, State),
  {reply, Reply, NewState}.

handle_cast(_Message, State) ->
  io:format("Most recent requests: ~p\n", [State]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  inets:stop(),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Internal functions

%% Given a 4-letter station code as the Request, return its basic
%% weather information as a {key,value} list. If successful, add the
%% station name to the State, which will keep track of recently-accessed
%% weather stations.

get_weather(Request, State) ->
  URL = "http://w1.weather.gov/xml/current_obs/" ++ Request ++ ".xml",
  {Result, Info} = httpc:request(URL),
  case Result of
    error -> {{Result, Info}, State};
    ok ->
      {{_Protocol, Code, _CodeStr}, _Attrs, WebData} = Info,
      case Code of
        404 ->
          {{error, 404}, State};
        200 ->
          Weather = analyze_info(WebData),
          {{ok, Weather}, [Request | lists:sublist(State, 10)]}
      end
  end.

%% Take raw XML data and return a set of {key, value} tuples

analyze_info(WebData) ->
  %% list of fields that you want to extract
  ToFind = [location, observation_time_rfc822, weather, temperature_string],
  
  %% get just the parsed data from the XML parse result
  Parsed = element(1, xmerl_scan:string(WebData)),
  
  %% This is the list of all children under <current_observation>
  Children = Parsed#xmlElement.content,
  
  %% Find only XML elements and extract their names and their text content.
  %% You need the guard so that you don't process the newlines in the
  %% data (they are XML text descendants of the root element).
  ElementList = [{El#xmlElement.name, extract_text(El#xmlElement.content)}
    || El <- Children, element(1, El) == xmlElement],
    
  %% ElementList is now a keymap; get the data you want from it.
  lists:map(fun(Item) -> lists:keyfind(Item, 1, ElementList) end, ToFind).


%% Given the parsed content of an XML element, return its first node value
%% (if it's a text node); otherwise return the empty string.

extract_text(Content) ->
  Item = hd(Content),
  case element(1, Item) of
    xmlText -> Item#xmlText.value;
    _ -> ""
  end.
  

