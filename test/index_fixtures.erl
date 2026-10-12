-module(index_fixtures).
-author('Kevin C. Baird').

-export([nz2023_party_results/0, ep2024_group_results/0]).

% https://en.wikipedia.org/wiki/2023_New_Zealand_general_election
% All 17 registered parties that contested the party vote, whose counts sum
% exactly to the 2,851,211 valid party votes; the article's separate 34,277
% "Independent" row is an electorate-vote figure and is excluded. Seats are
% the 122 of the 54th Parliament (120 proportional seats plus two Te Pati
% Maori overhang seats).
% {party_name, party vote share (fraction), seats of 122}
nz2023_party_results() -> [
    {national,         0.3808, 48},
    {labour,           0.2692, 34},
    {green,            0.1161, 15},
    {act,              0.0864, 11},
    {nz_first,         0.0609,  8},
    {te_pati_maori,    0.0308,  6},
    {top,              0.0222,  0},
    {nz_loyal,         0.0121,  0},
    {newzeal,          0.0057,  0},
    {legalise_cannabis,0.0046,  0},
    {freedoms_nz,      0.0034,  0},
    {democracy_nz,     0.0024,  0},
    {animal_justice,   0.0018,  0},
    {new_conservative, 0.0016,  0},
    {womens_rights,    0.0009,  0},
    {leighton_baker,   0.0007,  0},
    {new_nation,       0.0005,  0}
].

% https://en.wikipedia.org/wiki/2024_European_Parliament_election
% Group composition at the constitutive session (16-19 July 2024), 720 seats.
% The article folds the Non-Inscrits voters into its "Others" vote row, so
% their vote percentage is paired here.
% {group, seats of 720, group vote percentage}
ep2024_group_results() -> [
    {epp,         188, 19.65},
    {sd,          136, 15.63},
    {patriots,     84, 10.54},
    {ecr,          78,  8.93},
    {renew,        77,  9.15},
    {greens_efa,   53,  6.59},
    {the_left,     46,  6.77},
    {esn,          25,  4.94},
    {non_inscrits, 33,  8.64}
].
