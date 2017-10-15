Continuing with my [expiring records](https://github.com/snorristurluson/erl-expiring-records)
module (see [my previous blog](https://ccpsnorlax.blogspot.is/2017/10/expiring-records-in-erlang.html)),
I've now switched it over to using 
[Mnesia](http://erlang.org/doc/man/mnesia.html), rather than a 
dictionary object stored in the state of my *gen_server* instance.

It is still not a truly global, cross-node key/value store for
expiring records, but it is getting there. I wanted to focus first
on getting my tests to pass again with a RAM based table on one node.
I just need to tweak the values passed in when creating the Mnesia 
table, I think, to make this work with a disc based table,
replicated across nodes.

Currently the table is created like this:
```erlang
prepare_table() ->
    case catch mnesia:table_info(expiring_records, attributes) of
        {'EXIT', _} ->
            %% Table does not exist - create it
            erlang:display("Creating table"),
            mnesia:create_table(
                expiring_records, [
                    {attributes, record_info(fields, record)},
                    {record_name, record}
                ]
            ),
            ok;
        _Attributes ->
            ok
    end,
    mnesia:wait_for_tables([expiring_records], infinite).
```

I've also cleaned up the API, wrapping the *gen_server:call* calls:
```erlang
get_non_expired_record(_Config) ->
    ok = expiring_records:store("bingo", "bongo", erlang:system_time(second) + 3600),
    {ok, "bongo"} = expiring_records:fetch("bingo").

get_expired_record(_Config) ->
    ok = expiring_records:store("bingo", "bongo", erlang:system_time(second) + 1),
    timer:sleep(2000),
    not_found = expiring_records:fetch("bingo").
```

I've also broken the *handle_call* callback into multiple
definitions rather than using a case statement:
```erlang
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
```

This looks much better, and is getting closer to my intended
functionality. My next session will focus on getting a disc
based, replicated Mnesia table. Then I also need add to add
trimming, to remove expired records from the table even if they
aren't being looked up.