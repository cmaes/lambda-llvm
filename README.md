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

# Lambda LLVM

The goal is to implement a compiler for the lambda calculus with LLVM
