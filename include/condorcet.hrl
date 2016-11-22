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
-opaque ballot() :: #ballot{}.

-export_type([
    ballot/0
]).
