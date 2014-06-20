o2048
=====

An OCaml implementation of 2048, inspired by "Implementing 2048 in 90 lines of Haskell" (https://news.ycombinator.com/item?id=7896187) and https://github.com/tfausak/hs2048 

Admittedly, clones for this game have been more than overdone, and a lot of them are really well done. This imlpementation was written purely for fun, to start getting some more experience writing and understanding OCaml code! 

It's worth noting that, even with my relatively little experience with OCaml, this implementation only took 87 lines of pure source code (i.e. without comments, empty lines, and `;;` on new lines). This includes re-writing a few basic list operations that are available in the Haskell standard list library, but not in OCaml (repeat, span, group, and transpose). Without those, it's only 68 lines of pure source. Although, to be fair, my command-line interface is completely bare bones and not all that pretty. 

***

Run
---

`ocaml o2048.ml`

Play
----

- Use the WASD keys to select a direction, then press enter! 
- Use Ctrl+C to end the game at any time


Rules
-----

You know the rules...
