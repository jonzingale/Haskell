module FairTournament where
-- import HavelHakimi
-- import SpringRank
-- import Elo

{--
This module aims to model fair tournaments based on Elo ratings, Havel-Hakimi
graphs and SpringRank. Some ideas to be explored include:
1. Balancing interesting matches against maximizing for possible winners
2. Generating adaptive matchings based on tournament record
3. Allowing non-NM players to affect NM winner outcome.
4. Assessing winners in different ways: Best player, largest elo jump, etc...

The strategy is to build reasonable spring rank (and havel-hakimi) graphs given
elo ratings for tournament contenders.

1'. Organize graphs for least potential elo difference on the network.
--}

