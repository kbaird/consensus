-module(borda).

%% API exports
-export([
    rankings/2
]).

-include("include/elections.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec rankings(atom(), [ballot(), ...]) -> [{atom(), number()}, ...].
rankings(Label, Ballots) ->
    Candidates = ballot:candidates(hd(Ballots)),
    Map = rankings(Label, Ballots, length(Candidates), #{}),
    maps:to_list(Map).

%%====================================================================
%% Internal functions
%%====================================================================
add_votes(Label, [C | Cs], Acc) when is_map(Acc) ->
    add_votes(Label, [C | Cs], Acc, 1).

add_votes(_,     [],       Acc, _)        when is_map(Acc) -> Acc;
add_votes(Label, [C | Cs], Acc, Position) when is_map(Acc) ->
    CN = candidate:name(C),
    VotesByPosition = maps:get(CN, Acc, #{}),
    VotesAtPosition = maps:get(Position, VotesByPosition, 0),
    NewVotes = maps:put(Position, VotesAtPosition + 1, VotesByPosition),
    Acc2 = maps:put(CN, NewVotes, Acc),
    add_votes(Label, Cs, Acc2, Position + 1).

convert_to_borda_values(Label, CandCount, Acc) when is_atom(Label) andalso is_map(Acc) ->
    CandidateNames = maps:keys(Acc),
    convert_to_borda_values(Label, CandCount, CandidateNames, Acc, #{}).

convert_to_borda_values(_label, _CandCount, [], #{}, Final) -> Final;
convert_to_borda_values(Label, CandCount, [CN | CNs], From, Acc) when is_map(From) andalso is_map(Acc) ->
    Votes = maps:get(CN, From),
    From2 = maps:without([CN], From),
    Acc2 = maps:put(CN, tally_votes(Label, CandCount, Votes), Acc),
    convert_to_borda_values(Label, CandCount, CNs, From2, Acc2).

rankings(Label, [],       CandCount, Acc) -> convert_to_borda_values(Label, CandCount, Acc);
rankings(Label, [B | Bs], CandCount, Acc) when is_map(Acc) ->
    Candidates = ballot:candidates(B),
    Acc2 = add_votes(Label, Candidates, Acc),
    rankings(Label, Bs, CandCount, Acc2).

tally_votes(Label, CandCount, Votes) when is_map(Votes) ->
    Values = [ value(Label, Idx, CandCount) * maps:get(Idx, Votes) || Idx <- maps:keys(Votes) ],
    lists:sum(Values).

value(base0, Last, Last) -> 0;
value(base0, Position, CandCount) -> value(base0, Position + 1, CandCount) + 1;

value(base1, Position, CandCount) -> value(base0, Position, CandCount) + 1;

value(naura, 1,        _CandCount) -> 1;
value(naura, Position, _CandCount) -> 1.0 / Position.
