-module('effective_number_of_parties_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

effective_num_parties_test_() ->
    [
        fun effective_num_parties_dictatorship_case/0,
        fun effective_num_parties_even_two_case/0,
        fun effective_num_parties_even_ten_case/0,
        fun effective_num_parties_near_even_two_case/0,
        fun effective_num_parties_skewed_two_case/0,
        fun effective_num_parties_skewed_three_case/0,
        fun effective_num_parties_2023_new_zealand_case/0,
        fun effective_num_parties_2024_european_parliament_case/0
    ].

effective_num_parties_dictatorship_case() ->
    Count = consensus:effective_num_parties(single_party_state()),
    ?assertEqual(1.0, Count).

effective_num_parties_even_two_case() ->
    Count = consensus:effective_num_parties(even_two_state()),
    ?assertEqual(2.0, Count).

effective_num_parties_even_ten_case() ->
    Count = consensus:effective_num_parties(even_ten_state()),
    ?assert(Count > 9.99),
    ?assert(Count < 10.0).

effective_num_parties_near_even_two_case() ->
    Count = consensus:effective_num_parties(near_even_two_state()),
    ?assert(Count > 1.98),
    ?assert(Count < 1.99).

effective_num_parties_skewed_two_case() ->
    Count = consensus:effective_num_parties(skewed_two_state()),
    ?assert(Count > 1.72),
    ?assert(Count < 1.73).

effective_num_parties_skewed_three_case() ->
    Count = consensus:effective_num_parties(skewed_three_state()),
    ?assert(Count > 2.59),
    ?assert(Count < 2.60).

% https://en.wikipedia.org/wiki/2024_European_Parliament_election
% Group composition at the constitutive session (16-19 July 2024), 720 seats.
effective_num_parties_2024_european_parliament_case() ->
    Count = consensus:effective_num_parties(
        [ consensus_party:make(Group, Seats / 720) ||
          {Group, Seats, _VotePercent} <- index_fixtures:ep2024_group_results() ]),
    ?assert(Count > 6.51),
    ?assert(Count < 6.52).

% https://en.wikipedia.org/wiki/2023_New_Zealand_general_election
% The 17 registered parties that contested the party vote (2,851,211 valid
% party votes) give an effective number of parties of about 4.10; the 122
% seats of the 54th Parliament give about 3.81.
effective_num_parties_2023_new_zealand_case() ->
    VoteCount = consensus:effective_num_parties(nz2023_vote_shares()),
    ?assert(VoteCount > 4.10),
    ?assert(VoteCount < 4.11),
    SeatCount = consensus:effective_num_parties(nz2023_seat_shares()),
    ?assert(SeatCount > 3.81),
    ?assert(SeatCount < 3.82).

%%% PRIVATE FUNCTIONS

nz2023_vote_shares() ->
    [ consensus_party:make(Name, VoteShare) ||
      {Name, VoteShare, _Seats} <- index_fixtures:nz2023_party_results() ].

nz2023_seat_shares() ->
    [ consensus_party:make(Name, Seats / 122) ||
      {Name, _VoteShare, Seats} <- index_fixtures:nz2023_party_results() ].

single_party_state()  -> [ consensus_party:make(single_party, 1) ].
even_two_state()      -> [
    consensus_party:make(left,  0.5),
    consensus_party:make(right, 0.5)
].
even_ten_state()      -> [
    consensus_party:make(Name, Share) ||
    {Name, Share} <- [ {a, 0.1}, {b, 0.1}, {c, 0.1}, {d, 0.1}, {e, 0.1},
                       {f, 0.1}, {g, 0.1}, {h, 0.1}, {i, 0.1}, {j, 0.1} ]
].
near_even_two_state() -> [
    consensus_party:make(left,  0.55),
    consensus_party:make(right, 0.45)
].
skewed_two_state()    -> [
    consensus_party:make(strong, 0.7),
    consensus_party:make(weak,   0.3)
].
skewed_three_state()  -> [
    consensus_party:make(strong, 0.45),
    consensus_party:make(middle, 0.4),
    consensus_party:make(weak,   0.15)
].

