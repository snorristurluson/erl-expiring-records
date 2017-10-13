%%%-------------------------------------------------------------------
%%% @author snorri
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2017 08:44
%%%-------------------------------------------------------------------
-module(expiring_records).
-author("snorri").

-include("expiring_records.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0, stop/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop(Pid) ->
    gen_server:stop(Pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #expiring_records_state{}} | {ok, State :: #expiring_records_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #expiring_records_state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #expiring_records_state{}) ->
    {reply, Reply :: term(), NewState :: #expiring_records_state{}} |
    {reply, Reply :: term(), NewState :: #expiring_records_state{}, timeout() | hibernate} |
    {noreply, NewState :: #expiring_records_state{}} |
    {noreply, NewState :: #expiring_records_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #expiring_records_state{}} |
    {stop, Reason :: term(), NewState :: #expiring_records_state{}}).
handle_call(Request, _From, State) ->
    D = State#expiring_records_state.data,
    case Request of
        {add, Record} ->
            D2 = dict:store(
                Record#expiring_records_record.key,
                Record#expiring_records_record.value,
                D),
            {reply, ok, #expiring_records_state{data=D2}};

        {fetch, Key} ->
            Value = dict:fetch(Key, D),
            {reply, {ok, Value}, State};

        _ ->
            {reply, unknown_command, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #expiring_records_state{}) ->
    {noreply, NewState :: #expiring_records_state{}} |
    {noreply, NewState :: #expiring_records_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #expiring_records_state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #expiring_records_state{}) ->
    {noreply, NewState :: #expiring_records_state{}} |
    {noreply, NewState :: #expiring_records_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #expiring_records_state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #expiring_records_state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #expiring_records_state{},
    Extra :: term()) ->
    {ok, NewState :: #expiring_records_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
