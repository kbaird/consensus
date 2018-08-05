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

borda_values(Label, CandCount, Acc) when is_atom(Label) andalso is_map(Acc) ->
    borda_values(Label, CandCount, maps:keys(Acc), Acc, #{}).

borda_values(_label, _CandCount, [], #{}, Final) -> Final;
borda_values(Label, CandCount, [CandName | CNs], From, Acc) when is_map(From) andalso is_map(Acc) ->
    Votes = maps:get(CandName, From),
    From2 = maps:without([CandName], From),
    Acc2 = maps:put(CandName, value_of_votes(Label, CandCount, Votes), Acc),
    borda_values(Label, CandCount, CNs, From2, Acc2).

rankings(Label, [],       CandCount, Acc) -> borda_values(Label, CandCount, Acc);
rankings(Label, [B | Bs], CandCount, Acc) when is_map(Acc) ->
    Candidates = ballot:candidates(B),
    Acc2 = add_votes(Label, Candidates, Acc),
    rankings(Label, Bs, CandCount, Acc2).

value(base0, Position, CandCount) -> CandCount - Position;

value(base1, Position, CandCount) -> value(base0, Position, CandCount) + 1;

value(naura, 1,        _CandCount) -> 1;
value(naura, Position, _CandCount) -> 1.0 / Position.

value_of_votes(Label, CandCount, Votes) when is_map(Votes) ->
    Values = lists:map(fun(Idx) ->
                            EachValue = value(Label, Idx, CandCount),
                            VoteCount = maps:get(Idx, Votes),
                            EachValue * VoteCount
                       end, maps:keys(Votes)),
    lists:sum(Values).
