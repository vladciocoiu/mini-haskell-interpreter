# mini-haskell-interpreter

An interpreter for a smaller version of Haskell, written in Haskell :)

### How to run the project

```
stack build
stack exec interpreter-exe
```

### Usage

```
// Load a miniHaskell file
miniHaskell> :l file.mhs

// Evaluate a lambda-expression
miniHaskell> (/x -> x y) a
a y

// Quit the miniHaskell interpreter
miniHaskell> :q
```
