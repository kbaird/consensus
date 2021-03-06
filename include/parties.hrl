-type party_name()  :: binary().
-type cabinet()     :: [party_name()].

-type seat_share()  :: number().

% In practice, 0.0 means "unused"; effectively undefined.
-type vote_share()  :: number().

-record(party_result,
{
    name        :: party_name(),
    seat_share  :: seat_share(),
    vote_share  :: vote_share()
}).
-type party_result() :: #party_result{}.
