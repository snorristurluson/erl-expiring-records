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
    expired_record_is_removed,
    size_is_one_after_adding_one_record
].

init_per_testcase(_, Config) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    {ok, Pid} = expiring_records:start(),
    expiring_records:clear(),
    [{pid, Pid} | Config].

end_per_testcase(_, _Config) ->
    expiring_records:stop().


did_start(Config) ->
    Pid = ?config(pid, Config),
    Pid > 0.

undefined_command(Config) ->
    Pid = ?config(pid, Config),
    unknown_command = gen_server:call(Pid, bingo).

add_record(_Config) ->
    ok = expiring_records:store("bingo", "bongo", erlang:system_time(second) + 3600).

get_non_expired_record(_Config) ->
    ok = expiring_records:store("bingo", "bongo", erlang:system_time(second) + 3600),
    {ok, "bongo"} = expiring_records:fetch("bingo").

get_expired_record(_Config) ->
    ok = expiring_records:store("bingo", "bongo", erlang:system_time(second) + 1),
    timer:sleep(2000),
    not_found = expiring_records:fetch("bingo").

get_non_existing_record(_Config) ->
    not_found = expiring_records:fetch("bingo").

expired_record_is_removed(_Config) ->
    ok = expiring_records:store("bingo", "bongo", erlang:system_time(second)),
    timer:sleep(1000),
    not_found = expiring_records:fetch("bingo"),
    0 = expiring_records:size().

size_is_one_after_adding_one_record(_Config) ->
    ok = expiring_records:store("bingo", "bongo", erlang:system_time(second) + 3600),
    1 = expiring_records:size().