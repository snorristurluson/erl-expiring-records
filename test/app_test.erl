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
    {ok, Pid} = expiring_records:start(),
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
    ok = expiring_records:store("bingo", "bongo", erlang:system_time(second) + 3600, Pid).

get_non_expired_record(Config) ->
    Pid = ?config(pid, Config),
    ok = expiring_records:store("bingo", "bongo", erlang:system_time(second) + 3600, Pid),
    {ok, "bongo"} = expiring_records:fetch("bingo", Pid).

get_expired_record(Config) ->
    Pid = ?config(pid, Config),
    ok = expiring_records:store("bingo", "bongo", erlang:system_time(second) + 1, Pid),
    timer:sleep(2000),
    not_found = expiring_records:fetch("bingo", Pid).

get_non_existing_record(Config) ->
    Pid = ?config(pid, Config),
    not_found = expiring_records:fetch("bingo", Pid).

expired_record_is_removed(Config) ->
    Pid = ?config(pid, Config),
    ok = expiring_records:store("bingo", "bongo", erlang:system_time(second), Pid),
    timer:sleep(1000),
    not_found = expiring_records:fetch("bingo", Pid),
    0 = expiring_records:size(Pid).

