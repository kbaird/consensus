-opaque candidate() :: atom().
-opaque ballot()    :: [candidate(), ...].

-export_type([
    ballot/0,
    candidate/0
]).
