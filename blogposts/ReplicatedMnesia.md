I'm still working on my 
[expiring records](https://github.com/snorristurluson/erl-expiring-records)
module in Erlang (see [here](https://ccpsnorlax.blogspot.is/2017/10/mnesia.html) 
and [here](https://ccpsnorlax.blogspot.is/2017/10/expiring-records-in-erlang.html) 
for my previous posts on this).

Previously, I had started using Mnesia, but only a RAM based table. I've now
switched it over to a replicated disc based table. That was easy enough, but it
took a while to figure out how to do, nonetheless.

I had assumed that simply adding
```erlang
    ...
    {disc_copies, [node()]}
    ...
```
to the arguments to *mnesia:create_table* would be enough. This resulted in an
error:
```erlang
        {app_test,init_per_testcase,
            {{badmatch,
                 {aborted,
                     {bad_type,expiring_records,disc_copies,nonode@nohost}}},
        ...
```
After some head-scratching and lots of Googling I realized that I was
missing a call to *mnesia:create_schema* to allow it to create disc
based tables.

My tests for this module are done with 
[common_test](http://erlang.org/doc/apps/common_test/introduction.html)
so I set up a per suite initialization function like this:
```erlang
init_per_suite(Config) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    Config.

end_per_suite(Config) ->
    Config.
```
With this change, my tests now pass, with a disc based table. Now I want
to have the table replicated across multiple nodes.

Let's first set up a test environment. Using *rebar3 shell* is
convenient as it automatically builds the module upon entering the
Erlang shell and allows me to call the module functions from the
command line.

```erlang
Snorris-MBP-2:erl-expiring-records snorri$ rebar3 shell
===> Verifying dependencies...
===> Compiling expiring_records
Erlang/OTP 20 [erts-9.1.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V9.1.1  (abort with ^G)
1> mnesia:create_schema([node()]).
ok
2> mnesia:start().
ok
3> expiring_records:start().
{ok,<0.156.0>}
4> expiring_records:store("bingo", "bongo", erlang:system_time(seconds)+300).
ok
5> expiring_records:fetch("bingo").
{ok,"bongo"}
6> 
```
Note that I have to manually initialize Mnesia as I'm not running the
test framework. A proper application will have to do this setup somewhere
once at startup - it doesn't make sense to do it in the module.

Anyway, this is still just one node. Let's shut this one down and start
two new ones - this time I start it up with an extra argument to give it
a name.

```erlang
Snorris-MBP-2:erl-expiring-records snorri$ rebar3 shell --sname=bilbo
===> Verifying dependencies...
===> Compiling expiring_records
Erlang/OTP 20 [erts-9.1.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V9.1.1  (abort with ^G)
(bilbo@Snorris-MBP-2)1> 
```
And the second one:
```erlang
Snorris-MacBook-Pro-2:erl-expiring-records snorri$ rebar3 shell --sname=gandalf
===> Verifying dependencies...
===> Compiling expiring_records
Erlang/OTP 20 [erts-9.1.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V9.1.1  (abort with ^G)
(gandalf@Snorris-MBP-2)1> 
```
Note that the prompt changes to show you the name of the node - very
convenient. In one of the nodes, I have to tell Mnesia to create a schema
that includes both nodes:
```erlang
(gandalf@Snorris-MBP-2)1> mnesia:create_schema(['bilbo@Snorris-MBP-2', 'gandalf@Snorris-MBP-2']).
ok
```
Note that this has to happen before the *mnesia:start()* call, and that
both nodes have to be up and running. This isn't really clear in the
[Mnesia documentation](http://erlang.org/doc/man/mnesia.html). I really
recommend [http://learnyousomeerlang.com/mnesia#whats-mnesia]() for a
better overview of Mnesia - use the official documentation for a detailed
reference.

That change to the *create_table* arguments also needs tweaking - I want
the table to have a disc copy on all nodes:
```erlang
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
```

Anyway, let's fire up Mnesia on both nodes, as well as the module:
```erlang
(gandalf@Snorris-MBP-2)2> mnesia:start().
ok
(gandalf@Snorris-MBP-2)3> expiring_records:start().
{ok,<0.162.0>}

...

(bilbo@Snorris-MBP-2)1> mnesia:start().
ok
(bilbo@Snorris-MBP-2)2> expiring_records:start().
{ok,<0.155.0>}
```
Now I should be able to store a record in one node and fetch it on the
other:
```erlang
(bilbo@Snorris-MBP-2)3> expiring_records:store("bingo", "bongo", erlang:system_time(seconds)+60).  
ok

...

(gandalf@Snorris-MBP-2)4> expiring_records:fetch("bingo").
{ok,"bongo"}
```
This feels like a big victory! I know I haven't really done much here
myself - I'm really just wrapping basic functionality from Erlang, the
OTP and Mnesia, but these are really powerful tools.

Alright, that's all for now - next, I'll look at trimming expired values.
That'll give me a chance to do a simple query in Mnesia.