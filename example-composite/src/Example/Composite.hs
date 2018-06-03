module Example.Composite where

import ClassyPrelude
import Composite
import Composite.TH

-- base types

type X = '[ "foo" :-> Char, "bar" :-> Int ]

x :: Record X
x = 'a' :*: 1 :*: RNil :: Record X

$(withLensesAndProxies [d| type FFoo = "foo" :-> Char |])
