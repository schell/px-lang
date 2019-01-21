# px-lang
`px` is a simply typed lambda calculus for graphics.

# example

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
