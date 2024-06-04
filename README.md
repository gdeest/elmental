# Elmental: A Flexible Bridge between Elm and Haskell

Elmental is a code generator that takes in Haskell type definitions and generates Elm datatypes, along with Aeson-compatible encoders and decoders.

It thus shares the same broad goals as several libraries such as [haskell-to-elm](https://github.com/haskell-to-elm/haskell-to-elm#readme), [elminator](https://github.com/sras/elminator), [elm-bridge](https://hackage.haskell.org/package/elm-bridge) and [elm-street](https://github.com/Holmusk/elm-street). However, Elmental is the only one supporting this combined set of features:

- Smooth integration with legacy and custom Elm code: Elmental does not assume that you are starting from scratch, nor that you'll never want to use some third party type or decoder.
- Total control over the organization of the generated code.
- Native and natural support for most (if not all) Elm-representable Haskell types:
  - Recursive, polymorphic ADTs.
  - Records (either as Elm record aliases or as real datatypes).
  - Partially applied type constructors, in order to "erase" type arguments such as phantom parameters. It makes it possible to use the [higher-kinded data](https://reasonablypolymorphic.com/blog/higher-kinded-data/) pattern on the Haskell-side, and map to several Elm types depending on the value of the type parameter.
- Excellent performance / low compile-time overhead.

