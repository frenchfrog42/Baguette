# Baguette

A Lisp->BitcoinScript compiler, written in Racket (hopefully soon I'll switch to CommonLisp)  
Homepage of the project: https://frenchfrog42.github.io  
Documentation: https://docs-for-baguette.frenchfrog42.repl.co

## Example of contract

This contract requires you to give two equal values. It compiles to `OP_EQUAL`.

```racket
'(public (a b)
    (= (destroy a) (destroy b)))
```

This contract requires you to give a value that have a given hash. It compiles to `OP_SHA256 abcd OP_EQUAL`

```racket
'(public (a)
    (= (call sha256 ((destroy a))) "abcd")))
```

A contract that requires you to find two values where the sum is 8 and the product is 15.  
It compiles to `OP_2DUP OP_MUL OP_15 OP_EQUALVERIFY OP_ADD OP_8 OP_EQUAL`.  
Probably hard to be more efficient on this one.

```racket
(define c '(public (a b)
  (verify (= (* a b) 15))
  (= (+ (destroy a) (destroy b)) 8)))
```

This example is a bit pathological, because if you ask `(= 15 (* a b)))` the result is longer.  
It's on a todo list to rewrite the "search algorithm" that knows = + and * are commutative , but not done yet.  


---

You can find more examples in the `examples` directory.  
For instance, the [counter contract](https://github.com/frenchfrog42/Baguette/blob/main/examples/counter.rkt) and the [SuperAssetNFT contract](https://github.com/frenchfrog42/Baguette/blob/main/examples/SuperAssetNFT.rkt).


## Installation to use in vscode

I'd somewhat recommend to install locally, but you can try in your browser too, [for example at the bottom of my homepage](https://frenchfrog42.github.io).  

To install locally, you first need racket: `sudo apt-get install racket`  
Then, the language server: `raco pkg install racket-langserver`  
Then, the vscode extension named **Magic Racket**, you'll find it in the vscode

Now, please clone this repo: `git clone https://github.com/frenchfrog42/Baguette` and open whatever file. For instance `counter.rkt`.

To load the whole file in the repl, you can for example select the whole code and press `Alt+Enter`.  
Now you can execute any expression you want, for select `(profile-function-simple compteur)` at the top of the file, and press `Alt+Enter` again. The result should be printed at the bottom.

The result should more or less look like this:

![](https://i.imgur.com/hJAH3tV.png)

Somehow I can't find on this extension how to send a s-expr to the repl, hence the selection and `Alt+Enter` :/  
I personally used DrRacket when I wrote the code, but people usually prefer vscode.
