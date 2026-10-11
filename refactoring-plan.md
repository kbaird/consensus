# Refactoring Plan

## Scope

Entire repository: ~1,300 lines under `src/`, ~1,700 lines under `test/`.
This document is analysis and a proposed sequence only. No source changes have
been made.

Verification commands available in this repo:

- Tests: `rebar3 eunit`
- Typecheck: `rebar3 dialyzer`
- Style lint: `./elvis rock`
- All-in-one: `./full_check.sh`

## Priority Tiers

- **Tier 1 (correctness / API):** changes observable behavior or type contracts.
- **Tier 2 (duplication / structure):** internal cleanup, behavior-preserving.
- **Tier 3 (tests):** test-only cleanup, behavior-preserving.
- **Tier 4 (tooling):** changes what CI enforces.

---

## Tier 1 - Correctness and API

### 1.1 Heterogeneous return type in `borda:rankings/2`

`src/borda.erl:26-31` returns either a sorted list or a bare error binary
`<<"Please restrict the first argument to base0, base1, dowdell, or nauru.">>`.
Callers cannot distinguish success from failure without matching on a binary.

Recommendation: raise `error(badarg)` or return
`{ok, Rankings} | {error, Reason}`. This is the highest-impact API smell.

Fixed

### 1.2 Inaccurate type specs (fixed)

Dialyzer does not currently flag these because they are internal functions whose
specs do not conflict with call sites, but the specs were wrong:

- `src/consensus_cabinet.erl:117` `is_winner/2` spec was
  `([cabinet()], [cabinet()])`, but the first argument is one coalition and the
  second is the seat-share list.
- `include/parties.hrl:2` defined `cabinet() :: [party_name()]`, yet cabinet
  functions pass lists of `#party_result{}`. See
  `src/consensus_cabinet.erl:140` (`party_names/1` maps
  `consensus_party:name/1`), which proves elements are records. The type name was
  overloaded.
- `src/borda.erl:71,82` specs said `non_neg_integer()`, but the `nauru`/`dowdell`
  values return floats.
- `src/consensus_gallagher.erl:43,49` specs said `pos_integer()`, but the
  functions return floats.
- `src/consensus_party.erl:49,53` `sum_for`/`sum_seat_share_squares` specs said
  `pos_integer()`, but `seat_share()` is `number()`.

Resolution: added `coalition() :: [party_result()]` to `include/parties.hrl` and
kept `cabinet() :: [party_name()]` as the party-name output element type. All
internal `consensus_cabinet` specs that received record lists now use
`coalition()`. Also corrected `all_in/2`, `just_party_names/1` (return was
`[[atom()]]`), and `seat_share/1` (omitted its single-record clause). Specs only;
no runtime behavior changed. The borda line references above were off by one
after commit #41 added a spec line.

### 1.3 Magic unit inference in `consensus_party:make/3`

`src/consensus_party.erl:27-29` treats `VoteShare > 1.0` as a percentage and
silently divides by 100. The units are implicit. Recommendation: explicit
`make_percent/3`, or documented units with a separate constructor.

Fixed

Resolution: removed the `VoteShare > 1.0` inference. `make/3` now stores its
`VoteShare` argument unchanged and documents it as a fraction in `[0.0, 1.0]`;
added `make_percent/3`, which divides its `VoteSharePercent` argument by 100 and
documents it as a percentage in `[0.0, 100.0]`. Both constructors produce a
`vote_share` stored as a fraction. Within the repo only
`test/gallagher_index_test.erl` passed percentages to `make/3`; those call sites
were migrated to `make_percent/3`, while `nz2023/0` keeps `make/3` for its
fractions. Added `test/consensus_party_test.erl` to pin the contract, including
that `make/3` no longer infers a percent. Behavior change: external callers
passing `> 1.0` to `make/3` must switch to `make_percent/3`.

Risk: Tier 1 items change observable behavior or type contracts and should be
reviewed before merging. There is no single correct answer for the `borda`
return-shape decision; it is a judgment call.

---

## Tier 2 - Duplication and Structure

### 2.1 Label-alias macros scattered across modules

Six near-identical `-define(IS_*, ...)` guard macros in
`src/consensus_cabinet.erl:17-42`, plus `src/borda.erl:15-18`. Centralize in a
single `label:normalize/1` (or one lookup table) so synonyms are defined in one
place.

Fixed

Resolution: added `src/label.erl`, exporting `normalize/1` backed by a single
`CANONICAL_LABEL_OF` lookup table that maps every synonym to its canonical atom
(including `dowdell` -> `nauru`). `consensus_cabinet:compose/2,3` and
`borda:rankings/2` now normalize first and dispatch on canonical atoms, so all
`?IS_*` guard macros are gone. Behavior preserved: unknown labels still yield
`function_clause` from `consensus_cabinet` and `{error, badarg}` from `borda`;
unknown labels pass through `normalize/1` unchanged. Added `test/label_test.erl`.

### 2.2 `party_list_election:tabulate/6`

`src/party_list_election.erl:111-131` re-sorts `CountersWithQ` and
`EligibleWithQ` on every seat allocated. A single `lists:max`-style pass would
remove the double sort. `share/3` at `:100` truncates via `trunc(...*100)/100`
with an unclear name; rename to something like `seat_share_percent`.

Fixed

Resolution: `tabulate/6` now sorts `CountersWithQ` once per seat and filters
`EligibleWithQ` from that sorted list, so the winner is its head and the
increment reuses the same ordering; the second `lists:sort/2` is gone. The
originally proposed O(n) max pass was implemented first and abandoned: it broke
`webster_sainte_lague_test:webster_sainte_lague_basic_case/0`, because
`lists:sort/2` with the strict `>` comparator is not stable and the removed
ordering sort fed the final seat-tie order. Filtering the single sorted list
preserves that order exactly. Renamed `share/3` to `exact_seats/3` because it
returns the party's exact seat entitlement truncated to two decimals, not a
percentage; the plan's suggested `seat_share_percent` was a misnomer. Verified
with `./full_check.sh` (122 tests, Dialyzer, Elvis all pass).

### 2.3 `candidate` minor cleanups

- `src/candidate.erl:35` uses `length(maps:to_list(Prefs))`; use `maps:size/1`.
- The embedded Floyd-Warshall (`src/candidate.erl:67-92`) is a generic algorithm
  in a domain module and could be extracted.

Fixed

Resolution: `rank/1` now uses `maps:size/1` for the single-candidate fast path,
which returns `maps:keys/1` directly. Both Floyd-Warshall stages were extracted
verbatim into a new `src/floyd_warshall.erl` with a single `widest_paths/1`
entry point that builds the one-hop and triple lists internally;
`candidate:schulze_rank/1` now calls `widest_paths/1` and keeps only listing and
sorting. The recursion structure and `maps:put` order are unchanged, so the
strength map is byte-identical and tie ordering is preserved (`schulze_test.erl`
still passes). The misleading original `IOverJ`/`JOverI` variable names (which
swapped directions) became `Forward`/`Backward`. Added
`test/floyd_warshall_test.erl` pinning widest paths on a three-vertex graph and
empty/single-vertex cases, plus a single-candidate `rank/1` case in
`test/candidate_test.erl`. Verification: `widest_paths/1` was checked equal to a
verbatim copy of the old stages on random matrices (2-6 vertices) and
`./full_check.sh` passes.

### 2.4 Other minor cleanups

- `src/consensus_utils.erl:25` `uniqueify/1` has no `-spec`.
- `src/condorcet_election.erl:57-66` `with_zero_counts/1` has oddly placed
  `foldl` arguments.
- `src/droop_election.erl` internal `droop_winners/3` shares its name with the
  public function.
- `src/borda.erl:39-43` threads an unused `Label` argument through recursion.

---

## Tier 3 - Tests

### 3.1 No-op setup/teardown in every test file

All 15 test modules contain an identical no-op skeleton:

```erlang
X_setup()     -> ok.
X_teardown(_) -> ok.
X_test_() ->
    {setup, fun X_setup/0, fun X_teardown/1,
     fun(_) -> [ ... ] end}.
```

Since both callbacks are no-ops, the `{setup, ...}` wrapper is pointless. Return
a plain list instead. Saves ~100 lines and removes 30 dead functions.

### 3.2 Copy-pasted fixtures with drift risk

- NZ 2023 party data: `test/effective_number_of_parties_test.erl:86-105` vs
  `test/gallagher_index_test.erl:117-136`.
- EP 2024 group seats: `test/effective_number_of_parties_test.erl:55-65` vs
  `test/gallagher_index_test.erl:213-224`.

Extract a shared fixture module so the two index tests cannot drift.

### 3.3 `jefferson_dhondt_test.erl` (507 lines)

- 23 Argentine district cases (`:221-369`) and 11 Japanese PR-block cases
  (`:385-481`) are 3-6 line forwarders; tabularize into one parameterized case.
  Estimated savings: 150-200 lines.
- Near-identical helpers `assert_district_seats/3` (`:485-493`) and
  `assert_pr_block_seats/3,4` (`:495-506`) differ only in threshold and one
  extra argument; merge.
- Repeated seats-by-name and default-threshold idioms appear 7 times across this
  file and `test/webster_sainte_lague_test.erl`; extract helpers.

### 3.4 Dead test

`droop_five_winners_six_candidates_stv_case/0` (`test/droop_stv_test.erl:29-42`)
is never called (reference commented out at `:14`). Delete or re-enable.

### 3.5 Duplicate scenarios

The "single ranking, unanimous voters" scenario is tested three times:
`test/one_candidate_test.erl:18-26`, `test/two_candidates_test.erl:31-34`,
`test/three_candidates_test.erl:41-47`. Consolidate.

---

## Tier 4 - Tooling

### 4.1 Tests are effectively unlinted

`elvis.config:13-22` uses `ruleset => test_files`, which is empty in the
vendored elvis_core 1.1.2. Line length, whitespace, nesting, naming, and similar
rules never run on tests. The test `dont_repeat_yourself` threshold is also
relaxed to `min_complexity => 34` (vs 10 for `src`), which is why the
duplication in Tier 3 passes CI.

Recommendation: apply the `erl_files` ruleset to tests and lower the DRY
threshold. Note this changes the CI gate outcome and will require the Tier 3
cleanups first.

---

## Proposed Sequence

1. Tier 3 (tests first): safe, high line savings, no behavior change.
2. Tier 4.1: tighten elvis after Tier 3 lands, so the gate does not block on
   pre-existing duplication.
3. Tier 2: internal cleanup, behavior-preserving.
4. Tier 1: type specs and the `borda` API fix last, since these change contracts
   and need review.

## Known Constraints and Assumptions

- Dialyzer is silent on the wrong specs in Tier 1.2 because the specs are
  internal and do not conflict with call sites; correctness relies on manual
  review, not the tool.
- The `borda:rankings/2` return-shape decision (Tier 1.1) is a judgment call
  with no single correct answer.
- Elvis is a vendored escript at the repo root (`./elvis`, elvis_core 1.1.2);
  there is no rebar elvis dependency.
