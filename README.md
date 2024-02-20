# Flazzy

A pure, lazy, functional, toy programming language.

```hs
map not (Some False) # evaluates to Some True
. not = (
  | True = False
  | False = True)
. map f = (
  | Some x = Some (f x)
  | None = None)
. Bool =
  | False
  | True
. Option a =
  | Some a
  | None
```

This repo is home to the Pyrotelekinetic Flazzy Compiler. It is not yet in a working state (hopefully it will be soon!). To build and run:

```sh
ghc main -o pyrofc && ./pyrofc example.flazzy
```