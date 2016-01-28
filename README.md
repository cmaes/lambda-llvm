# Prerequisites

## Install OCAML

On Linux

```
sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update
sudo apt-get install curl build-essential m4 ocaml opam
```

## Setup OPAM

```
opam init
eval `opam config env`
```

## Install Menhir

```
opam install menhir
```

## Install LLVM


```
sudo apt-get install llvm-3.7 llvm-3.7-dev llvm-3.7-runtime
```

```
opam install llvm
```



# Lambda LLVM

The goal is to implement a compiler for the lambda calculus with LLVM
