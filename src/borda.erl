-module(borda).

%% API exports
-export([
    rankings/2
]).

-include("include/common.hrl").
-include("include/elections.hrl").

%%====================================================================
%% Guard Macros
%%====================================================================

-define(IS_VALID_LABEL(Label), Label =:= base0 orelse
                               Label =:= base1 orelse
                               Label =:= dowdell orelse
                               Label =:= nauru).

%%====================================================================
%% API functions
%%====================================================================

% https://en.wikipedia.org/wiki/Borda_count#Example
-spec rankings(label(), [ballot(), ...]) -> [{name(), number()}, ...] | binary().
rankings(Label, Ballots) when ?IS_VALID_LABEL(Label) ->
    Candidates = ballot:candidates(hd(Ballots)),
    Map = rankings(Label, Ballots, length(Candidates), #{}),
    L = maps:to_list(Map),
    lists:sort(fun by_votes/2, L);
rankings(_, _) -> <<"Please restrict the first argument to base0, base1, dowdell, or nauru.">>.

%%====================================================================
%% Internal functions
%%====================================================================
-spec add_votes(label(), [candidate(), ...], map()) -> map().
add_votes(Label, [C | Cs], Acc) when is_map(Acc) -> add_votes(Label, [C | Cs], Acc, 1).

add_votes(_,     [],       Acc, _) -> Acc;
add_votes(Label, [C | Cs], Acc, Position) ->
    CN = candidate:name(C),
    Acc2 = add_votes_at_position(CN, Position, Acc),
    add_votes(Label, Cs, Acc2, Position + 1).

-spec add_votes_at_position(name(), pos_integer(), map()) -> map().
add_votes_at_position(CN, Position, Acc) ->
    VotesByPosition = maps:get(CN, Acc, #{}),
    VotesAtPosition = maps:get(Position, VotesByPosition, 0),
    NewVotes = maps:put(Position, VotesAtPosition + 1, VotesByPosition),
    maps:put(CN, NewVotes, Acc).

-spec borda_values(label(), pos_integer(), map()) -> map().
borda_values(Label, CandCount, Acc) when is_atom(Label) andalso is_map(Acc) ->
    borda_values(Label, CandCount, maps:keys(Acc), Acc, #{}).

borda_values(_label, _CandCount, [], #{}, Final) -> Final;
borda_values(Label, CandCount, [CandName | CNs], From, Acc) ->
    Votes = maps:get(CandName, From),
    From2 = maps:without([CandName], From),
    Acc2 = maps:put(CandName, value_of_votes(Label, CandCount, Votes), Acc),
    borda_values(Label, CandCount, CNs, From2, Acc2).

by_votes({_, V1}, {_, V2}) -> V1 > V2.

rankings(Label, [],       CandCount, Acc) -> borda_values(Label, CandCount, Acc);
rankings(Label, [B | Bs], CandCount, Acc) when is_map(Acc) ->
    Acc2 = add_votes(Label, ballot:candidates(B), Acc),
    rankings(Label, Bs, CandCount, Acc2).

-spec value(label(), pos_integer(), pos_integer()) -> non_neg_integer().
value(base0, Position, CandCount) -> CandCount - Position;

value(base1, Position, CandCount) -> value(base0, Position, CandCount) + 1;

% Cf. https://en.wikipedia.org/wiki/Borda_count#Dowdall_system_(Nauru)
value(dowdell, Position,  CandCount) -> value(nauru, Position, CandCount);

value(nauru, 1,        _CandCount) -> 1;
value(nauru, Position, _CandCount) -> 1.0 / Position.

-spec value_of_votes(label(), pos_integer(), map()) -> non_neg_integer().
value_of_votes(Label, CandCount, Votes) when is_map(Votes) ->
    Values = lists:map(fun(Idx) ->
                            EachValue = value(Label, Idx, CandCount),
                            VoteCount = maps:get(Idx, Votes),
                            EachValue * VoteCount
                       end, maps:keys(Votes)),
    lists:sum(Values).
