rebar3 eunit &&
  rebar3 dialyzer &&
  ./elvis rock &&
  echo 'passed' || echo 'failed'
