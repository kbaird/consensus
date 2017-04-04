-module(consensus_party).

%% API exports
-export([
    make/2,
    make/3,
    name/1,
    seat_share/1,
    vote_share/1
]).

-include("parties.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec make(party_name(), seat_share()) -> party_result().
make(Name, SeatShare) ->
    make(Name, SeatShare, undefined).

-spec make(party_name(), seat_share(), vote_share()) -> party_result().
make(Name, SeatShare, VoteShare) ->
    #party_result{name       = Name,
                  seat_share = SeatShare,
                  vote_share = VoteShare}.

name(#party_result{name = Name}) -> Name.

seat_share(#party_result{seat_share = Seats}) -> Seats.

vote_share(#party_result{vote_share = Votes}) -> Votes.

%%====================================================================
%% Internal functions
%%====================================================================

