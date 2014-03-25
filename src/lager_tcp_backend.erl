-module(lager_tcp_backend).
-behaviour(gen_event).

-export([
        init/1, 
        handle_call/2, 
        handle_event/2, 
        handle_info/2, 
        terminate/2,
        code_change/3
    ]).

-include("../include/lager.hrl").

-compile([{parse_transform, lager_transform}]).

-record(state, {level, formatter, host, port, socket}).

-define(RECONNECT_TIMEOUT, 5000).

init(Config) ->
    Level = proplists:get_value(level, Config, utrace),
    Formatter = proplists:get_value(formatter, Config, undefined),
    Host = proplists:get_value(host, Config, "localhost"),
    Port = proplists:get_value(port, Config, 62785),
    lager:info("level=~p, formatter=~p, host=~p, port=~p", [Level, Formatter, Host, Port]),
    State = #state{
        level=Level,
        formatter=Formatter, 
        host=Host,
        port=Port},
    {ok, connect_tcp_socket(State)}.

handle_call({set_loglevel, Level}, State=#state{}) ->
    {ok, ok, State#state{level=Level}};
    
handle_call(get_loglevel, State=#state{level=Level}) ->
    {ok, Level, State};

handle_call({set_formatter, Formatter}, State) ->
    {ok, ok, State#state{formatter=Formatter}};

handle_call(get_formatter, State=#state{formatter=Formatter}) ->
    {ok, Formatter, State};
    
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, Msg}, State=#state{level=Level, socket=Socket}) when Socket =/= undefined ->
    case lager_util:is_loggable(Msg, lager_util:level_to_num(Level), ?MODULE) of
        true ->
            {ok, do_log(Msg, State)};
        _ ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_info(timeout, State) ->
    {ok, connect_tcp_socket(State)};
handle_info({tcp_closed, _}, State) ->
    {ok, connect_tcp_socket(State#state{socket=undefined})};
handle_info({tcp_error, _Socket, Reason}, State) ->
    lager:error("error ~p on tcp connection, will attempt reconnect", [Reason]),
    {ok, connect_tcp_socket(State)};
handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_log(_Msg, State=#state{socket=undefined}) ->
    State;
do_log(_Msg={lager_msg, _Dest, Metadata, Severity, _DateTime, Timestamp, Content}, 
        State=#state{level=Severity, socket=Socket, formatter=Formatter}) ->
    case format(Formatter, Metadata, Timestamp, Content) of
        Bin when is_binary(Bin) ->
            case gen_tcp:send(Socket, Bin) of
                {error, timeout} ->
                    %% Usually, it's a good idea to give up in case of a 
                    %% send timeout, as you never know how much actually 
                    %% reached the server, maybe only a packet header
                    connect_tcp_socket(State);
                _ ->
                    State
            end;
        _Other ->
            State
    end;
do_log(_Msg, State=#state{socket=_Socket}) ->
    State.

format(undefined, _Metadata, _Timestamp, Content) ->
    Content;
format(Module, Metadata, Timestamp, Content) when is_atom(Module) ->
    try Module:format(Metadata, Timestamp, Content) of
        Result ->
            Result
        catch _:_Y ->
            Content
    end;
format(_Module, _, _, Content) ->
    Content.

connect_tcp_socket(State=#state{host=Host, port=Port, socket=undefined}) ->
    case gen_tcp:connect(Host, Port, [{keepalive, true},
                                        {nodelay, true},
                                        {send_timeout, 500}
                                    ], 5000) of
        {error, _Error} ->
            %lager:error("unable to connect to microtrace host: ~p port: ~p error: ~p", [Host, Port, Error]),
            timed_reconnect(?RECONNECT_TIMEOUT),
            State;
        {ok, Socket} ->
            lager:info("connected to microtrace host: ~p port: ~p", [Host, Port]),
            State#state{socket=Socket}
    end;
connect_tcp_socket(State=#state{socket=Socket}) ->
    gen_tcp:close(Socket),
    connect_tcp_socket(State#state{socket=undefined}).

timed_reconnect(Time) ->
    erlang:send_after(Time, self(), timeout).
