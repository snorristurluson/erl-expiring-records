%%%-------------------------------------------------------------------
%% @doc erl-expiring-records public API
%% @end
%%%-------------------------------------------------------------------

-module(expiring_records_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    expiring_records_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
