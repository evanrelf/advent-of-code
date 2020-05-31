# Haskell

## Developing

I like to use GHCi to test my solutions incrementally in interactive fashion. If
you're a Nix user, I've provided a `shell.nix` file to set up the environment
with GHC and ghcid. Otherwise, just make sure you have GHC and (optionally)
ghcid installed.

Loading a solution into the REPL is as easy as:

```text
ghci 2018/1-1.hs
```

And loading a solution into ghcid is as easy as:

```text
ghcid 2018/1-1.hs
```

## Running

I wrote each solution to take input on `stdin`, and output the answer on
`stdout`.

With Nix installed, just run the file like so:

```text
./2018/1-1.hs < input.txt
```

Or if you don't have Nix, but you do have GHC, you can use `runghc` like so:

```text
runghc 2018/1-1.hs < input.txt
```

Ideally I write my solutions as efficiently as possible, however considering I'm
limiting myself to only the `base` library, I don't always have the most
performant abstractions available.

In cases where `runghc` is too slow (because it interprets your program rather
than compiling it), compiling with GHC is an option to eek out some more
performance:

```text
ghc -O3 2018/1-1.hs -o 2018/1-1
./2018/1-1 < input.txt
```

## Disclaimer

This is not a good place to look if you want examples of good, idiomatic,
well-thought-through Haskell code! I'm not always using the best tool for the
job in each exercise, in an effort to learn new things. Also there's a lot of
puzzles to get through so I'm usually sticking with the first working solution I
come up with, even when it's ugly...
