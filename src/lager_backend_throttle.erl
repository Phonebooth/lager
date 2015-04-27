%% Copyright (c) 2011-2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc A simple gen_event backend used to monitor mailbox size and
%% switch log messages between synchronous and asynchronous modes.
%% A gen_event handler is used because a process getting its own mailbox
%% size doesn't involve getting a lock, and gen_event handlers run in their
%% parent's process.

-module(lager_backend_throttle).

-include("lager.hrl").

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
        code_change/3]).

-record(state, {
        hwm,
        window_min,
        sieve_threshold,
        sieve_window,
        async = true,
        sieve = false
    }).

init([Hwm, Window, SieveThreshold, SieveWindow]) ->
    lager_config:set(async, true),
    lager_config:set(sieve, false),
    {ok, #state{hwm=Hwm, window_min=Hwm - Window,
                sieve_threshold=SieveThreshold, sieve_window=SieveWindow}}.

handle_call({set_sieve, {Threshold, Window}}, State) ->
    {ok, ok, State#state{sieve_threshold=Threshold, sieve_window=Window}};
handle_call({set_sieve_threshold, Threshold}, State) ->
    {ok, ok, State#state{sieve_threshold=Threshold}};
handle_call({set_sieve_window, Window}, State) ->
    {ok, ok, State#state{sieve_window=Window}};
handle_call(get_loglevel, State) ->
    {ok, undefined, State};
handle_call({set_loglevel, _Level}, State) ->
    {ok, ok, State};
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, _Message},State_) ->
    {message_queue_len, Len} = erlang:process_info(self(), message_queue_len),

    State = case {Len > State_#state.sieve_threshold, Len < State_#state.sieve_window, State_#state.sieve} of
        {true, _, false} ->
            %% need to flip to sieve mode
            lager_config:set(sieve, true),
            State_#state{sieve=true};
        {_, true, true} ->
            %% need to flip to sieve mode off
            lager_config:set(sieve, false),
            State_#state{sieve=false};
        _ ->
            %% nothing needs to change
            State_
    end,

    case {Len > State#state.hwm, Len < State#state.window_min, State#state.async} of
        {true, _, true} ->
            %% need to flip to sync mode
            lager_config:set(async, false),
            {ok, State#state{async=false}};
        {_, true, false} ->
            %% need to flip to async mode
            lager_config:set(async, true),
            {ok, State#state{async=true}};
        _ ->
            %% nothing needs to change
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

