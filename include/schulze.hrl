-type name() :: atom().

-record(candidate,
    {
        name :: name()
    }).

-record(ballot,
    {
        candidates :: [candidate(), ...]
    }).

-type candidate() :: #candidate{}.
-type ballot()    :: #ballot{}.

