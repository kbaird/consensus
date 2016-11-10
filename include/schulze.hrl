-opaque name()      :: atom().

-record(candidate,
    {
        name :: name()
    }).

-record(ballot,
    {
        candidates :: [candidate(), ...]
    }).

-opaque candidate() :: #candidate{}.
-opaque ballot()    :: #ballot{}.

-export_type([
    ballot/0,
    candidate/0,
    name/0
]).
