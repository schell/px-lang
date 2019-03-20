# px-lang
`px` aims to be a starting point for domain specific, typed lambda calculi.
It is inspired by David Himmelstrup's [fvg](https://github.com/Lemmih/fvg),
Sean Lee's [hylogen](https://github.com/sleexyz/hylogen) and
Stephen Diehl's amazing "Write You a Haskell" [series](http://dev.stephendiehl.com/fun/).
It uses an interesting cofree approach to type inference inspired by
[Brian McKenna](https://brianmckenna.org/blog/type_annotation_cofree) and is largely
based on the papers "Generalizing Hindley-Milner type Inference Algorithms" by
[Heeren, Hage and Swierstra](http://soft.vub.ac.be/~cfscholl/Capita-Selecta-2015/papers/2002%20Heeren.pdf)

I aim to make the language extensible with domain specific parsers, primitives and
code generators. The goal is to make `px` be the main ingredient in a bunch of
little functional languages.

# notes

This project started as a way for me to learn about TAPL and to address my
gripes with shader languages. It's been sitting on my hardrive untouched for
over a year now and I figured I should get it out in public before it gets lost.
It is not in a usable state, though it may compile and it may pass tests. The goals
are lofty and unatainable with my current availability for OSS. It could be a fun
study though!

# target examples
These aren't functional yet, but there an example of what I'd like the language
to look like in different domains (it's basically Haskell).

Coloring a canvas in WASM:
```haskell
munchingSquares :: Vector Word4
munchingSquares = do
  x <- range 0 255
  y <- range 0 255
  return $ xor x y


main :: WASM ()
main = do
  -- Make the canvas and set the pixels
  c <- newCanvas 256 256
  setPixels c munchingSquares
  -- Add it to the DOM
  b <- body =<< document
  appendChild b c
```

Coloring a canvas in WASM:


## features
* [x] ML syntax
  * [x] let expressions
  * [ ] indentation-sensitive parsing
  * [ ] pattern matching
  * [ ] case expressions
* [ ] custom data types
* [ ] type classes
* [ ] user defined primitives
* [ ] user defined mixfix operators
* [ ] user defined sugar
* [x] repl for evaluating expressions at the command line
* [x] hendley-milner type inference
* [ ] wasm codegen
* [ ] spir-v codegen

## links to implementation helpers

These are bookmarked links from my browser that are on topic:

* [Demystifying Type Classes](http://okmij.org/ftp/Computation/typeclass.html)
* [Hindley-Milner type inference with constraints](https://kseo.github.io/posts/2017-01-02-hindley-milner-inference-with-constraints.html)
* [Renaissance: a functional shading language](https://chadaustin.me/hci_portfolio/thesis.pdf)
* [Parsing Mixfix Operators](http://www.cse.chalmers.se/~nad/publications/danielsson-norell-mixfix.pdf)
