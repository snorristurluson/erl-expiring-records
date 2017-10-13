%%%-------------------------------------------------------------------
%%% @author snorri
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2017 09:33
%%%-------------------------------------------------------------------
-author("snorri").

-record(expiring_records_state, {data=dict:new()}).
-record(expiring_records_record, {
    key,
    value,
    expires_at
}).
