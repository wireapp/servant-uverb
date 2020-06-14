# servant-uverb

[![Hackage](https://img.shields.io/hackage/v/servant-uverb.svg?logo=haskell)](https://hackage.haskell.org/package/servant-uverb)
[![AGPL-3 license](https://img.shields.io/badge/license-AGPL--3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/servant-uverb/badge/lts)](http://stackage.org/lts/package/servant-uverb)
[![Stackage Nightly](http://stackage.org/package/servant-uverb/badge/nightly)](http://stackage.org/nightly/package/servant-uverb)
[![Build status](https://img.shields.io/travis/fisx/servant-uverb.svg?logo=travis)](https://travis-ci.org/fisx/servant-uverb)


more docs coming up!


TODO:

- [ ] `UStream` (like `Stream`)
- [ ] `NoContentUVerb` (like `NoContentVerb`)
- [ ] Tests


This set of packages allows you to write servant code for end-points
whose response types are open unions of types (`UVerb` is short for
`UnionVerb`)


### Related work

There is an [issue from
2017](https://github.com/haskell-servant/servant/issues/841)
discussing handlers that return different types under different
circumstances.

[servant-checked-exceptions](https://hackage.haskell.org/package/servant-checked-exceptions)
is a good solution to the problem, but it restricts the user to JSON
and a very specific envelop encoding for the union type, which is
often not acceptable.  (One good reason for this design choice is that
it makes writing clients easier, where you need to get to the union
type from one representative, and you don't want to run several
parsers in the hope that the ones that should will always error out so
you can try until the right one returns a value.)

[servnat-exceptions](https://github.com/ch1bo/servant-exceptions) is
another shot at at the problem.  It is inspired by
servant-checked-exceptions, so it may be worth taking a closer look.
The README also claims that
[cardano-sl](https://github.com/input-output-hk/cardano-sl) also has
some code for generalized error handling.

We have copied some code from
[world-peace](https://hackage.haskell.org/package/world-peace).  The
package itself wasn't flexible enough, and we had to use
[sop-core](https://hackage.haskell.org/package/sop-core) to implement
servant-uverb-server.

Also:
https://lukwagoallan.com/posts/unifying-servant-server-error-responses
