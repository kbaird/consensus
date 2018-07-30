-module(consensus_party).

%% API exports
-export([
    effective_number/1,
    make/2,
    make/3,
    name/1,
    seat_share/1,
    vote_share/1
]).

-include("include/parties.hrl").

%%====================================================================
%% API functions
%%====================================================================

% See consensus:effective_num_parties for references
-spec effective_number([party_result()]) -> number().
effective_number(PartyShares) -> 1 / sum_for(PartyShares).

-spec make(party_name(), seat_share()) -> party_result().
make(Name, SeatShare) -> make(Name, SeatShare, undefined).

-spec make(party_name(), seat_share(), vote_share()) -> party_result().
make(Name, SeatShare, VoteShare) ->
    #party_result{name       = Name,
                  seat_share = SeatShare,
                  vote_share = VoteShare}.

-spec name(party_result()) -> party_name().
name(#party_result{name = Name}) -> Name.

-spec seat_share(party_result()) -> seat_share().
seat_share(#party_result{seat_share = Seats}) -> Seats.

-spec vote_share(party_result()) -> vote_share().
vote_share(#party_result{vote_share = Votes}) -> Votes.

%%====================================================================
%% Internal functions
%%====================================================================

-spec sum_for([party_result()]) -> pos_integer().
sum_for(PartyShares) ->
    lists:foldl(fun sum_seat_share_squares/2, 0, PartyShares).

-spec sum_seat_share_squares(party_result(),
                             pos_integer()) -> pos_integer().
sum_seat_share_squares(PartyResult, Acc) ->
    Seats = consensus_party:seat_share(PartyResult),
    (Seats * Seats) + Acc.

