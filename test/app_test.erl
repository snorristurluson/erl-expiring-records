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

-include_lib("common_test/include/ct.hrl").
-include("expiring_records.hrl").

-compile(export_all).

all() -> [
    undefined_command,
    did_start,
    add_record,
    get_non_expired_record,
    get_expired_record,
    get_non_existing_record,
    expired_record_is_removed
].

init_per_testcase(_, Config) ->
    {ok, Pid} = expiring_records:start_link(),
    [{pid, Pid} | Config].

end_per_testcase(_, Config) ->
    Pid = ?config(pid, Config),
    expiring_records:stop(Pid).


did_start(Config) ->
    Pid = ?config(pid, Config),
    Pid > 0.

undefined_command(Config) ->
    Pid = ?config(pid, Config),
    unknown_command = gen_server:call(Pid, bingo).

add_record(Config) ->
    Pid = ?config(pid, Config),
    Record = {"bingo", "bongo", erlang:system_time(second) + 3600},
    ok = gen_server:call(Pid, {add, Record}).

get_non_expired_record(Config) ->
    Pid = ?config(pid, Config),
    Record = {"bingo", "bongo", erlang:system_time(second) + 3600},
    ok = gen_server:call(Pid, {add, Record}),
    {ok, "bongo"} = gen_server:call(Pid, {fetch, "bingo"}).

get_expired_record(Config) ->
    Pid = ?config(pid, Config),
    Record = {"bingo", "bongo", erlang:system_time(second) + 1},
    ok = gen_server:call(Pid, {add, Record}),
    timer:sleep(2000),
    not_found = gen_server:call(Pid, {fetch, "bingo"}).

get_non_existing_record(Config) ->
    Pid = ?config(pid, Config),
    not_found = gen_server:call(Pid, {fetch, "bingo"}).

expired_record_is_removed(Config) ->
    Pid = ?config(pid, Config),
    Record = {"bingo", "bongo", erlang:system_time(second)},
    ok = gen_server:call(Pid, {add, Record}),
    timer:sleep(1000),
    not_found = gen_server:call(Pid, {fetch, "bingo"}),
    0 = gen_server:call(Pid, size).

