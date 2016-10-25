%%%-------------------------------------------------------------------
%%% @author Anatoly Yakovenko
%%% @copyright (C) 2015, Mesosphere
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2016 11:44 AM
%%%-------------------------------------------------------------------
-module(ip_vs_conn_monitor).
-author("Anatoly Yakovenko").

-behaviour(gen_server).

-export([get_connections/0]).
-export([start_link/0]).

-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-include("include/ip_vs_conn.hrl").
-define(SERVER, ?MODULE).

-record(state, {
    syns = maps:new() %% all the connection that have seen a syn_recv
    }).

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(init(term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    process_flag(trap_exit, true),
    erlang:send_after(splay_ms(), self(), poll_proc),
    {ok, #state{}}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(get_connections, _From, State) ->
    {reply, {ok, State#state.syns}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(poll_proc, State) ->
    ProcFile = ip_vs_conn_config:proc_file(),
    New = ip_vs_conn:fold(
              fun(Conn, Map) -> 
                      ip_vs_conn_map:update(State#state.syns, Conn, Map)
              end,
              maps:new(),
              ProcFile),
    NewState = State#state{syns = New},
    erlang:send_after(splay_ms(), self(), poll_proc),
    {noreply, NewState};
   
handle_info(_Info, State) ->
    {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
    ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_connections() -> gen_server:call(?SERVER, get_connections).

%% TODO: borrowed from minuteman, should probably be a util somewhere
-spec(splay_ms() -> integer()).
splay_ms() ->
    MsPerMinute = ip_vs_conn_config:interval_seconds() * 1000,
    NextMinute = -1 * erlang:monotonic_time(milli_seconds) rem MsPerMinute,

    SplayMS = ip_vs_conn_config:splay_seconds() * 1000,
    FlooredSplayMS = max(1, SplayMS),
    Splay = rand:uniform(FlooredSplayMS),

    NextMinute + Splay.
