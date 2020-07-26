%%%-------------------------------------------------------------------
%%% File    : cache_tab_app.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : Cache tab application
%%%
%%% Created : 8 May 2013 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% Copyright (C) 2002-2017 ProcessOne, SARL. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
-module(cache_tab_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, get_nodes/0]).

-define(PG, cache_tab).

-include("ets_cache.hrl").

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    case cache_tab_sup:start_link() of
        {ok, Pid} ->
            pg2:create(?PG),
            pg2:join(?PG, Pid),
	    application:start(p1_utils),
	    init_ets_cache_options(),
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

get_nodes() ->
    [node(P) || P <- pg2:get_members(?PG)].

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_ets_cache_options() ->
    p1_options:start_link(ets_cache_options),
    p1_options:insert(ets_cache_options, max_size, global, ?DEFAULT_MAX_SIZE),
    p1_options:insert(ets_cache_options, life_time, global, ?DEFAULT_LIFE_TIME),
    p1_options:insert(ets_cache_options, cache_missed, global, ?DEFAULT_CACHE_MISSED).
