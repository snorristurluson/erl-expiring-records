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
-export([start/0, stop/1, store/4, fetch/2, size/1]).

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
-spec(start() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @end
%%--------------------------------------------------------------------
stop(Pid) ->
    gen_server:stop(Pid).

%%--------------------------------------------------------------------
%% @doc
%% Stores the value for the key.
%%
%% Stores a Key/Value pair. If Key already exists its value is
%% replaced. The Key/Value has an expiration time and will be
%% removed at that time.
%%
%% @end
%%--------------------------------------------------------------------
-spec(store(Key :: term(),
    Value :: term(),
    ExpiresAt :: integer(),
    Pid :: pid()) ->
    ok ).
store(Key, Value, ExpiresAt, Pid) ->
    Record = {Key, Value, ExpiresAt},
    gen_server:call(Pid, {add, Record}).

%%--------------------------------------------------------------------
%% @doc
%% Fetches a value for the given key.
%%
%% If the value is found, it is guaranteed not to be expired at the
%% time of the call.
%% @end
%%--------------------------------------------------------------------
-spec(fetch(Key :: term(), Pid :: pid()) ->
    {ok, Value :: term()} | not_found ).

fetch(Key, Pid) ->
    gen_server:call(Pid, {fetch, Key}).

%%--------------------------------------------------------------------
%% @doc
%% Get an estimated size (some records may be expired).
%%
%% @end
%%--------------------------------------------------------------------
-spec(size(Pid :: pid()) ->
    Size :: integer()).

size(Pid) ->
    gen_server:call(Pid, size).

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
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_call({add, {Key, Value, ExpiresAt}}, _From, State) ->
    D = State#state.data,
    D2 = dict:store(Key, {Value, ExpiresAt}, D),
    {reply, ok, #state{data=D2}};

handle_call({fetch, Key}, _From, State) ->
    D = State#state.data,
    case dict:find(Key, D) of
        {ok, {Value, ExpiresAt}} ->
            Now = erlang:system_time(second),
            case Now < ExpiresAt of
                true ->
                    {reply, {ok, Value}, State};
                _ ->
                    D2 = dict:erase(Key, D),
                    {reply, not_found, #state{data=D2}}
            end;
        error ->
            {reply, not_found, State}
    end;

handle_call(size, _From, State) ->
    D = State#state.data,
    {reply, dict:size(D), State};

handle_call(_Request, _From, State) ->
    {reply, unknown_command, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
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
    State :: #state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
