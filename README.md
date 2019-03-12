# px-lang
`px` is a simply typed lambda calculus for graphics. It is
inspired by David Himmelstrup's [fvg](https://github.com/Lemmih/fvg),
Sean Lee's [hylogen](https://github.com/sleexyz/hylogen) and
Stephen Diehl's amazing "Write You a Haskell" [series](http://dev.stephendiehl.com/fun/).
It uses an interesting cofree approach to type inference inspired by
[Brian McKenna](https://brianmckenna.org/blog/type_annotation_cofree) and is largely
based on the papers "Generalizing Hindley-Milner type Inference Algorithms" by
[Heeren, Hage and Swierstra](http://soft.vub.ac.be/~cfscholl/Capita-Selecta-2015/papers/2002%20Heeren.pdf).

# notes

This project started as a way for me to learn about TAPL and to address my
gripes with shader languages. It's been sitting on my hardrive untouched for
over a year now and I figured I should get it out in public before it gets lost.
It is not in a usable state, though it may compile and it may pass tests. The goals
are lofty and unatainable with my current availability for OSS. It could be a fun
study though!

# example? - may not be accurate or up to date

I don't think this is accurate any longer, see the tests for examples.
I think I was using this example as a target for the EDSL version of px.

Here is an example of a simple pass through shader that
varies its red channel over time.

```haskell
vertexShader
  :: Mat4 Float
  -> Mat4 Float
  -> Vec2 Float
  -> Vec4 Float
  -> (Vec4 Float, Vec4 Float)
vertexShader projection modelview position color =
  let Vec2 x y = position
  in (projection * modelview * Vec4 x y 0 1, color)


fragmentShader
  :: Float
  -> Vec4 Float
  -> Vec4 Float
fragmentShader time color =
  let Vec4 r g b a = color
  in Vec4 (r * sin time) g b a


program
  :: Mat4 Float
  -> Mat4 Float
  -> Float
  -> Buffer (Vec2 Float)
  -> Buffer (Vec4 Float)
  -> Image
program projection modelview time positionBuffer colorBuffer =
  let buffers  = zipBuffers positionBuffer colorBuffer
      vertices = map (uncurry (vertexShader projection modelview)) buffer
      pixels   = bimap id (fragmentShader time) vertices
  in Image pixels
```


## features
* [x] ML syntax
  * [x] let expressions
  * [ ] pattern matching
  * [ ] case expressions
* [x] repl for evaluating expressions at the command line
* [x] hendley-milner type inference
* [ ] spir-v codegen
  * [ ] and then cc to msl
  * [ ] and then cc to hsl
  * [ ] and then cc to glsl
  * [x] and then ...
  * [x] no and then!
* [ ] user defined mixfix operators
* [ ] vector and matrix primitives
* [ ] custom data types
* [ ] type classes
* [ ] quasiquoter for haskell edsl, compiling and checking at haskell compile
      time
* [ ] auto generate linkage functions for host platforms/langs


## links

These are bookmarked links from my browser that are on topic:

* [Demystifying Type Classes](http://okmij.org/ftp/Computation/typeclass.html)
* [Hindley-Milner type inference with constraints](https://kseo.github.io/posts/2017-01-02-hindley-milner-inference-with-constraints.html)
* [Renaissance: a functional shading language](https://chadaustin.me/hci_portfolio/thesis.pdf)
