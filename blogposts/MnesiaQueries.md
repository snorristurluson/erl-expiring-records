I've added search and trim to my
[expiring records](https://github.com/snorristurluson/erl-expiring-records)
module in Erlang. This started out as an 
[in-memory](https://ccpsnorlax.blogspot.is/2017/10/expiring-records-in-erlang.html) 
key/value store, that I then migrated over to
[using Mnesia](https://ccpsnorlax.blogspot.is/2017/10/mnesia.html)
and eventually to a
[replicated Mnesia](https://ccpsnorlax.blogspot.is/2017/10/replicated-mnesia.html)
table.

The *fetch/1* function is already doing a simple query, with 
[match_object](http://erlang.org/doc/man/mnesia.html#match_object-1).
```erlang
Result = mnesia:match_object(expiring_records, #record{key = Key, value = '_', expires_at = '_'}, read)
```
The three parameters there are the name of the table - *expiring_records*,
the matching pattern and the lock type (read lock).

The *fetch/1* function looks up the key as it was added to the table with
*store/3*. If the key is a tuple, we can also do a partial match:
```erlang
Result = mnesia:match_object(expiring_records, #record{key = {'_', "bongo"}, value = '_', expires_at = '_'}, read)
```
I've added a *search/1* function the module that takes in a matching
pattern and returns a list of items where the key matches the pattern.
Here's the test for the *search/1* function:
```erlang
search_partial_key(_Config) ->
    ok = expiring_records:store({"bingo", "parlor"}, "bongo", erlang:system_time(second) + 3600),
    ok = expiring_records:store("bingo", "bongo", erlang:system_time(second) + 3600),
    ok = expiring_records:store({"smoking", "parlor"}, "bongo", erlang:system_time(second) + 3600),
    ok = expiring_records:store("smoking", "bongo", erlang:system_time(second) + 3600),
    ok = expiring_records:store("parlor", "bongo", erlang:system_time(second) + 3600),
    ok = expiring_records:store({"reading", "parlor"}, "bongo", erlang:system_time(second) - 1),
    [A, B] = expiring_records:search({'_', "parlor"}),
    {"bingo", "parlor"} = A#record.key,
    {"smoking", "parlor"} = B#record.key.
```
For more complex queries we can use 
[select](http://erlang.org/doc/man/mnesia.html#select-2).
This function takes in a match specification that goes beyond
the pattern matching done by *match_object*. The *trim/0* function
finds records where the expiration time has passed:
```erlang
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
```
The *MatchHead* specifies the things we care about in the record and
gives them labels that we can refer in the other parts of the match
specification. The *key* is labelled **$1** and is referred to in the
*Result*. The *expires_at* field is labelled **$2** and is referred to
in the *Guard*. This guard expression is quite simple - *Now* should be
larger than the expiration time of the record. This *select* call returns
a list of keys for records that have expired, that will in turn get
deleted.

I need to experiment with the performance of these sort of queries.
Mnesia tables can have secondary indices that ought to help, but I'm 
sure queries end up being a sequential scan of all entries, applying 
the pattern matching or guard expression to each entry in turn.