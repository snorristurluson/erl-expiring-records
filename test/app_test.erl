%%%-------------------------------------------------------------------
%%% @author snorri
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2017 08:40
%%%-------------------------------------------------------------------
-module(app_test).
-author("snorri").

-include_lib("eunit/include/eunit.hrl").
-include("expiring_records.hrl").

simple_test() ->
    ?assert(true).

expiring_records_test_() ->
    {foreach, fun start/0, fun stop/1, [
        fun undefined_command/1,
        fun did_start/1,
        fun add_record/1,
        fun get_non_expired_record/1
    ]}.

start() ->
    {ok, Pid} = expiring_records:start_link(),
    Pid.

stop(Pid) ->
    expiring_records:stop(Pid).

did_start(Pid) ->
    ?_assert(Pid > 0).

undefined_command(Pid) ->
    unknown_command = gen_server:call(Pid, bingo),
    ?_assert(true).

add_record(Pid) ->
    Record = #expiring_records_record{
        key = "bingo",
        value = "bongo",
        expires_at = 42
    },
    ok = gen_server:call(Pid, {add, Record}),
    ?_assert(true).

get_non_expired_record(Pid) ->
    Record = #expiring_records_record{
        key = "bingo",
        value = "bongo",
        expires_at = 42
    },
    ok = gen_server:call(Pid, {add, Record}),
    {ok, "bongo"} = gen_server:call(Pid, {fetch, "bingo"}),

    ?_assert(true).
