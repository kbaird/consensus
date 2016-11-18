-module(consensus_party).

%% API exports
-export([
    make/2,
    make/3
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

%%====================================================================
%% Internal functions
%%====================================================================

