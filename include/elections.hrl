-type candidate_name() :: atom().

-record(candidate,
    {
        name :: candidate_name(),
        party :: undefined | binary()
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
