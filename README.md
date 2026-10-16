# consensus

An OTP library implementing consensus-oriented political functions, including the
[Schulze voting method](https://en.wikipedia.org/wiki/Schulze_method), the
[Gallagher Index](https://en.wikipedia.org/wiki/Gallagher_Index), the
[d'Hondt/Jefferson Method](https://en.wikipedia.org/wiki/D%27Hondt_method),
various [Borda Counts](https://en.wikipedia.org/wiki/Borda_count#Example) and the
[Webster/Sainte-Laguë Method](https://en.wikipedia.org/wiki/Webster/Sainte-Lagu%C3%AB_method#Description_of_the_method).

This code is heavily based on ideas found in [Patterns of Democracy](http://wikisum.com/w/Lijphart:_Patterns_of_democracy) by
[Arend Lijphart](https://en.wikipedia.org/wiki/Arend_Lijphart).

The
[main non-trivial Condorcet test file](https://github.com/kbaird/schulze-vote-erlang/blob/master/test/tn_capital_test.erl)
uses the
[TN Capital example](https://en.wikipedia.org/wiki/Condorcet_method#Example:_Voting_on_the_location_of_Tennessee.27s_capital)
from Wikipedia.

## Build

    $ rebar3 compile

## Eunit

    $ rebar3 eunit

## Dialyzer

    $ rebar3 dialyzer

## Elvis

Download [elvis](https://github.com/inaka/elvis), then

    $ elvis rock
