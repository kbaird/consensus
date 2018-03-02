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
-type ballot() :: #ballot{}.

-export_type([
    ballot/0
]).

