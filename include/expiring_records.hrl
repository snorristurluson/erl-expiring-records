%%%-------------------------------------------------------------------
%%% @author snorri
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2017 09:33
%%%-------------------------------------------------------------------
-author("snorri").

-record(state, {
    data=dict:new()
}).

-record(record, {
    key,
    value,
    expires_at
}).