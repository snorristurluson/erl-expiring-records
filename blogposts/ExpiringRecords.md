I'm continuing my experiments with Erlang - this time trying out
*gen_server* with a simple key/value store with a twist - the values
have an expiration date.

As a first iteration I'm simply using a dictionary to store the
values, and only expiring records when they are looked up. My plan
is to extend this later on so that this can be a global key/value
store across multiple Erlang nodes but for now I'm focusing on two
things - get something going using *gen_server*, and try out the
*common_test* testing framework.

The code is here: [https://github.com/snorristurluson/erl-expiring-records]()

Let's first take a look at a couple of the test functions, to show
the usage of this:
```erlang
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
```
I should probably wrap the gen_server:call calls to make this more
readable - I'm just realizing that now as I write this, but I want
this blog to reflect my progress on learning Erlang, rather than
just presenting some final result. 

Here's the handle_call:

```erlang
handle_call(Request, _From, State) ->
    D = State#state.data,
    case Request of
        {add, {Key, Value, ExpiresAt}} ->
            D2 = dict:store(Key, {Value, ExpiresAt}, D),
            {reply, ok, #state{data=D2}};

        {fetch, Key} ->
            case dict:find(Key, D) of
                {ok, {Value, ExpiresAt}} ->
                    Now = erlang:system_time(second),
                    case Now < ExpiresAt of
                        true ->
                            {reply, {ok, Value}, State};
                        _ ->
                            D2 = dict:erase(Key, D),
                            {reply, not_found, #state{data=D2}}
                    end;
                error ->
                    {reply, not_found, State}
            end;


        size ->
            {reply, dict:size(D), State};

        _ ->
            {reply, unknown_command, State}
    end.

```
Coming from a long background of writing in C++ and Python, the
notion of having no object with a state still feels a bit weird.
The *gen_server* process replaces that by passing the state around
so it kind of boils down to the same thing. I just have to remember
to return the new state when changing the dict.

## Tests
I kept running into problems with *eunit* when trying to set up a
fixture for running the various tests, all starting with a fresh
instance of the *expiring_records* server. Looking at 
[Common Test](http://erlang.org/doc/apps/common_test/basics_chapter.html)
it seemed it might be more suitable so I've set up my tests with
it this time around. I recommend [this section](http://learnyousomeerlang.com/common-test-for-uncommon-tests)
of the [Learn You Some Erlang for Great Good](http://learnyousomeerlang.com/content) tutorial
for getting started with Common Test.

Note that Travis CI by default runs eunit when testing Erlang
projects - I had to add the following to my *.travis.yml* file:
```
script:
    rebar3 ct --suite app_test
```

## What's next?
This is still very much a work in progress - I want to look at Mnesia
for storing the data, rather than a simple dict. I figure that is
the easiest way to achieve my goal of having this a global store
across multiple nodes.

I also want to add a way to prune expired records without looking
them up, to prevent the accumulation of expired records.
