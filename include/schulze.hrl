-record(candidate,
    {
        name :: atom()
    }).

-record(ballot,
    {
        candidates :: [candidate(), ...]
    }).

-opaque candidate() :: #candidate{}.
-opaque ballot()    :: #ballot{}.

-export_type([
    ballot/0,
    candidate/0
]).
