# Advent of Code in Haskell

> In which Neill learns Haskell via Advent of Code.

## Setup

For the Advent of Code API to work, a session key needs to be stored in `/aoc-conf.yaml`.

Use the following format for the file:

```yaml
session: YOUR_KEY_HERE
```

## Inspiration

Although the solutions to the puzzles themselves are all purely my own, the scaffolding for
interacting with the AoC API and organizing solutions is heavily inspired by Justin Le's
[advent-of-code](https://github.com/mstksg/advent-of-code) repository. I am grateful for their
generous sharing of their Haskell knowledge!

## Notes & Learnings

### Template Haskell

To view and debug splice output outside the terminal on a per-file basis:

1. Add the following GHC options to the top of the file:

    ```haskell
    {-# OPTIONS_GHC -ddump-splices #-}
    {-# OPTIONS_GHC -ddump-to-file #-}
    ```

2. Trigger a build. If using `stack ghci`, the dump file will appear alongside the source. If using
   `stack build`, continue to the next step.

3. Locate the `.dump-splices` file deep within the `.stack-work` directory. For example, the dump
   for the `Sandbox.Splices` module appears in `.stack-work/dist/{{env
   info}}/build/advent-haskell/advent-haskell-tmp/src/Sandbox/Splices.dump-splices`
