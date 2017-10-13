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

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export[did_start/1, undefined_command/1, add_record/1,
    get_expired_record/1, get_non_expired_record/1].

all() -> [
    undefined_command,
    did_start,
    add_record,
    get_non_expired_record,
    get_expired_record
].

init_per_testcase(_, Config) ->
    erlang:display("start"),
    {ok, Pid} = expiring_records:start_link(),
    [{pid, Pid} | Config].

end_per_testcase(_, Config) ->
    erlang:display("stop"),
    Pid = ?config(pid, Config),
    expiring_records:stop(Pid).


did_start(Config) ->
    erlang:display("did_start"),
    Pid = ?config(pid, Config),
    Pid > 0.

undefined_command(Config) ->
    erlang:display("undefined_command"),
    Pid = ?config(pid, Config),
    unknown_command = gen_server:call(Pid, bingo).

add_record(Config) ->
    erlang:display("add_record"),
    Pid = ?config(pid, Config),
    Record = {"bingo", "bongo", erlang:system_time(second) + 3600},
    ok = gen_server:call(Pid, {add, Record}).

get_non_expired_record(Config) ->
    erlang:display("get_non_expired_record"),
    Pid = ?config(pid, Config),
    Record = {"bingo", "bongo", erlang:system_time(second) + 3600},
    ok = gen_server:call(Pid, {add, Record}),
    {ok, "bongo"} = gen_server:call(Pid, {fetch, "bingo"}).

get_expired_record(Config) ->
    erlang:display("get_expired_record"),
    Pid = ?config(pid, Config),
    Record = {"bingo", "bongo", erlang:system_time(second) + 1},
    ok = gen_server:call(Pid, {add, Record}),
    timer:sleep(2000),
    not_found = gen_server:call(Pid, {fetch, "bingo"}).
