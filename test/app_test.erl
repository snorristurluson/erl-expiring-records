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
        fun get_non_expired_record/1,
        fun get_expired_record/1
    ]}.

start() ->
    erlang:display("start"),
    {ok, Pid} = expiring_records:start_link(),
    Pid.

stop(Pid) ->
    erlang:display("stop"),
    expiring_records:stop(Pid).

did_start(Pid) ->
    erlang:display("did_start"),
    ?_assert(Pid > 0).

undefined_command(Pid) ->
    erlang:display("undefined_command"),
    unknown_command = gen_server:call(Pid, bingo),
    ?_assert(true).

add_record(Pid) ->
    erlang:display("add_record"),
    Record = {"bingo", "bongo",erlang:system_time(second) + 3600},
    ok = gen_server:call(Pid, {add, Record}),
    ?_assert(true).

get_non_expired_record(Pid) ->
    erlang:display("get_non_expired_record"),
    Record = {"bingo", "bongo",erlang:system_time(second) + 3600},
    ok = gen_server:call(Pid, {add, Record}),
    {ok, "bongo"} = gen_server:call(Pid, {fetch, "bingo"}),

    ?_assert(true).

get_expired_record(Pid) ->
    erlang:display("get_expired_record"),
    Record = {"bingo", "bongo",erlang:system_time(second) + 1},
    ok = gen_server:call(Pid, {add, Record}),
    timer:sleep(2),
    not_found = gen_server:call(Pid, {fetch, "bingo"}),

    ?_assert(true).
