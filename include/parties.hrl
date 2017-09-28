-type party_name()  :: atom().
-opaque cabinet()   :: [party_name()].

-type seat_share()  :: number().

-type vote_share()  :: undefined | number().

-record(party_result,
{
    name        :: party_name(),
    seat_share  :: seat_share(),
    vote_share  :: vote_share()
}).
-type party_result() :: #party_result{}.
-export_type([
    cabinet/0,
    party_result/0
]).
