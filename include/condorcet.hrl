-type name() :: atom().

-record(candidate,
    {
        name :: name()
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
-opaque ballot() :: #ballot{}.

-export_type([
    ballot/0
]).

