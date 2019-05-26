-type name() :: atom().

-record(candidate,
    {
        name :: name(),
        party :: binary() | undefined
    }).
-type candidate() :: #candidate{}.

-type preferences() :: #{
    candidate() => #{
        candidate() => pos_integer()
    }
}.

-record(ballot,
    {
        candidates :: [candidate(), ...]
    }).
-type ballot() :: #ballot{}.

% https://en.wikipedia.org/wiki/Mixed-member_proportional_representation
-record(mmp_ballot,
    {
        candidates :: [candidate(), ...],
        party_list_choice :: binary()
    }).
-type mmp_ballot() :: #mmp_ballot{}.

-export_type([
    ballot/0,
    mmp_ballot/0
]).

