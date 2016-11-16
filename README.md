# schulze-vote-erlang

[![Build Status](https://semaphoreci.com/api/v1/kbaird/schulze-vote-erlang/branches/master/badge.svg)](https://semaphoreci.com/kbaird/schulze-vote-erlang)

An OTP library implementing consensus-oriented political functions, mainly the [Schulze voting
system](https://en.wikipedia.org/wiki/Schulze_method)

The [main non-trivial test
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
