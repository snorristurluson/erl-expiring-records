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
-export([start/0, stop/0, clear/0, store/3, fetch/1, size/0, trim/0]).

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
    prepare_table(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:stop(?MODULE).

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
    ExpiresAt :: integer()) ->
    ok ).
store(Key, Value, ExpiresAt) ->
    Record = {Key, Value, ExpiresAt},
    gen_server:call(?MODULE, {add, Record}).

%%--------------------------------------------------------------------
%% @doc
%% Fetches a value for the given key.
%%
%% If the value is found, it is guaranteed not to be expired at the
%% time of the call.
%% @end
%%--------------------------------------------------------------------
-spec(fetch(Key :: term()) ->
    {ok, Value :: term()} | not_found ).

fetch(Key) ->
    gen_server:call(?MODULE, {fetch, Key}).

%%--------------------------------------------------------------------
%% @doc
%% Get an estimated size (some records may be expired).
%%
%% @end
%%--------------------------------------------------------------------
-spec(size() ->
    Size :: integer()).

size() ->
    gen_server:call(?MODULE, size).

trim() ->
    gen_server:call(?MODULE, trim).

clear() ->
    gen_server:call(?MODULE, clear).

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
    Trans = fun() ->
        Record = #record{key=Key, value=Value, expires_at = ExpiresAt},
        mnesia:write(expiring_records, Record, write)
    end,
    {atomic, ok} = mnesia:transaction(Trans),
    {reply, ok, State};

handle_call({fetch, Key}, _From, State) ->
    Trans = fun() ->
        Result = mnesia:match_object(expiring_records, #record{key = Key, value = '_', expires_at = '_'}, read),
        case Result of
            [{record, Key, Value, ExpiresAt}] ->
                Now = erlang:system_time(second),
                case Now < ExpiresAt of
                    true ->
                        {ok, Value};
                    _ ->
                        mnesia:delete(expiring_records, Key, write),
                        not_found
                end;
            [] ->
                not_found
        end
    end,
    {atomic, Result} = mnesia:transaction(Trans),
    {reply, Result, State};

handle_call(size, _From, State) ->
    Size = mnesia:table_info(expiring_records, size),
    {reply, Size, State};

handle_call(trim, _From, State) ->
    Trans = fun() ->
        Now = erlang:system_time(second),
        MatchHead = #record{key='$1', expires_at = '$2', _='_'},
        Guard = {'>', Now, '$2'},
        Result = '$1',
        ExpiredKeys = mnesia:select(expiring_records, [{MatchHead, [Guard], [Result]}]),
        delete_records(ExpiredKeys)
    end,
    {atomic, ok} = mnesia:transaction(Trans),
    {reply, ok, State};

handle_call(clear, _From, State) ->
    mnesia:clear_table(expiring_records),
    {reply, ok, State};

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

prepare_table() ->
    DbNodes = mnesia:system_info(db_nodes),
    case catch mnesia:table_info(expiring_records, attributes) of
        {'EXIT', _} ->
            %% Table does not exist - create it
            {atomic, ok} = mnesia:create_table(
                expiring_records, [
                    {attributes, record_info(fields, record)},
                    {record_name, record},
                    {disc_copies, DbNodes}
                ]
            ),
            ok;
        _Attributes ->
            ok
    end,
    mnesia:wait_for_tables([expiring_records], infinite).

delete_records([]) ->
    ok;

delete_records([Head | Tail]) ->
    mnesia:delete(expiring_records, Head, write),
    delete_records(Tail).
