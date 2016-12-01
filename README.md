# consensus

[![Build Status](https://semaphoreci.com/api/v1/kbaird/consensus/branches/master/badge.svg)](https://semaphoreci.com/kbaird/consensus)

[![Code Climate](https://codeclimate.com/github/kbaird/consensus/badges/gpa.svg)](https://codeclimate.com/github/kbaird/consensus)

[![Ebert](https://ebertapp.io/github/kbaird/consensus.svg)](https://ebertapp.io/github/kbaird/consensus)

An OTP library implementing consensus-oriented political functions, including the
[Schulze voting method](https://en.wikipedia.org/wiki/Schulze_method) and the
[Gallagher Index](https://en.wikipedia.org/wiki/Gallagher_Index).

The [main non-trivial Schulze test
file](https://github.com/kbaird/schulze-vote-erlang/blob/master/test/tn_capital_test.erl)
uses the [TN Capital
example](https://en.wikipedia.org/wiki/Condorcet_method#Example:_Voting_on_the_location_of_Tennessee.27s_capital)
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

## TODO

- Schulze Method
  - currently requires every ballot to be complete
