-module(label).

%% API exports
-export([
    normalize/1
]).

-include("include/common.hrl").

%%====================================================================
%% API functions
%%====================================================================

% Maps every recognized label alias to its canonical atom. Unknown labels
% pass through unchanged so callers can decide how to reject them.
-define(CANONICAL_LABEL_OF,
        % smallest of the MWCs
        #{ bargaining_proposition    => bp
         % favors "moderate" parties near the ideological center
         , policy_viable_coalition   => pvc
         % ideologically similar parties more likely to form a coalition
         , minimal_range             => mr
         , minimum_range             => mr
         % favors a small # of seats to form a seat majority
         , minimal_size              => ms
         , minimum_size              => ms
         % favors a small # of parties to form a seat majority
         , minimal_winning_coalition    => mwc
         , minimum_winning_coalition    => mwc
         % MRs that don't skip, possibly adding superfluous middle parties
         , minimal_connected_winning    => mcw
         , minimum_connected_winning    => mcw
         % Nauru is the Dowdall system
         , dowdell                      => nauru
         }).

-spec normalize(label()) -> label().
normalize(Label) -> maps:get(Label, ?CANONICAL_LABEL_OF, Label).
