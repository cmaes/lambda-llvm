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

I tried

```
opam install llvm
```

But this failed because llmv wasn't install. It told me to issue the following command.

```
opam depext llvm.3.7
```

This tried to install llvm-3.7-dev but using this
[script](https://gist.githubusercontent.com/jpdeplaix/f930a62bb00b573e19f4/raw/9515dd708c65d567cf8e366dac61e0933d4b896b/gistfile1.txt).
But the script failed with an unmet libjsoncpp0 dependency. This may
have had to do with the fact that the script is for trusty. I'm using
Ubuntu wily so I had to do the following:

```
sudo add-apt-repository 'deb http://llvm.org/apt/wily/ llvm-toolchain-wily-3.7 main'
wget -O - http://llvm.org/apt/llvm-snapshot.gpg.key | sudo apt-key add -

sudo apt-get update

sudo apt-get install llvm-3.7-dev
```

And then install llvm ocaml bindings through opam

```
opam install llvm
```


# Lambda LLVM

The goal is to implement a compiler for the lambda calculus with LLVM
