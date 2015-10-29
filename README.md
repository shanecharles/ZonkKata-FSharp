# ZonkKata-FSharp
Zonk Kata done in F#

Zonk is a game consisting of 6 dice. The official rules and details can be found at [http://www.zonkthegame.com](http://www.zonkthegame.com).

For this kata the scoring is defined as:
- An individual 1 or 5 scores 100 or 50 points respectively
- A straight 1 through 6 scores 1000 points
- Three pairs of any numbers score 750 points
- Three of a kind: 
  * 1 - 1000 points
  * 2 -  200 points
  * 3 -  300 points
  * 4 -  400 points
  * 5 -  500 points
  * 6 -  600 points
- Four of a kind scores 2x the Three of a kind points
- Five of a kind scores 3x the Three of a kind points
- Six of a kind scores 4x the Three of a kind points

eg.
-  1, 1, 4, 5, 6, 1 - 1050 points - Three of a kind of 1s and one 5
-  5, 4, 6, 2, 1, 2 -  150 points - Count of one 1 and one 5
-  1, 2, 6, 2, 1, 6 -  750 points - Three pairs
-  2, 3, 6, 4, 2, 3 -    0 points - Zonk!

The object is to calculate the maximum score for a roll. A die can only be used for one score.

## Build

    > build.cmd    // on Windows
    $ ./build.sh   // on unix
