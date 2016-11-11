-type name() :: atom().

-record(candidate,
    {
        name :: name()
    }).
-type candidate() :: #candidate{}.

-record(ballot,
    {
        candidates :: [candidate(), ...]
    }).
-type ballot() :: #ballot{}.

