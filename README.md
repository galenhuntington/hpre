`hpre` is a simple Haskell code preprocessor that adds a few minor
syntactic features.  They are meant to be minimal, and increase
productivity without harming readability.

I have been using Haskell since 2005 and am largely self-taught.
As such, I developed my own habits and conventions, and found a few
aspects of the language unsatisfactory.  I developed what is now `hpre`
(formerly `gghc`) over the years to address these.

Here are the features, along with their rationales:


##  Ticked numbers

_An extension `NumericUnderscores` landed in GHC 8.6 that meets
this need; as such, this feature is deprecated and will eventually
be removed._

Large integer literals can be hard to read off.  Many languages
provide native syntax for spacing them out.  For example, Perl allows
one to write one billion as `1_000_000_000` rather than the hypnotic
`1000000000`.  Haskell does not, so I’ve added it.

My first try at a spacing symbol is a tick mark (apostrophe), `'`, but
I’m also experimenting with `_`.  So you can write `1'000'000'000` or
`1_000_000_000`, and `hpre` will take out the non-digits.  This made
a number of my mathematical projects more readable.

Since GHC 7.8, the language option `NumDecimals` allows writing one
billion (of an `Integral` type) as `1e9`.  However, you might not
want this extension, and anyway it won’t help if your number is not
mostly a row of zeroes, such as `1'073'741'824`.

No existing Haskell syntax (including GHC extensions) appears to
conflict with either notation.


##  Trailing commas and the like

_An extension `ExtraCommas` has been proposed that would replace some
of these features, but it has been stalled for a while._

That a comma is not allowed after the last item in a comma-separated
list is a recurring nuisance.  It is easy to fail to attend to
this exception when adding to the list, or moving entries around.
Furthermore, code diffs can triple in size:

```diff
-    itemSeven
+    itemSeven,
+    itemEight
```

This can make it harder to see at a glance that the only change was
adding an item.

The Haskell community’s answer to this has been to adopt
“Utrecht-style” commas, at the start of the line:

```haskell
   mapM_ print
      [ itemOne
      , itemTwo
      , itemThree
      ]
```

The idea being that you can add `, itemFour` easily.

However, this is not really satisfactory.  For one, I personally
find it unsightly; commas were meant to sit comfortably after a word.
Nevertheless, there is an argument that code is easier to scan with
delimiters at the start of lines, and the community has largely
settled on this style, so I have been slowly adopting it.

But the more important point is that it doesn’t fix the problem.
Instead of the last item being exceptional, now the first is.  Perhaps
it is more common to add to the end of a list than the beginning, but
one might want to move `itemOne` down, or add before it, or remove it.

Many other languages sensibly allow a _trailing comma_ in such lists,
after the last item.  Java, Python, Perl, Ruby, and modern Javascript
(ES5) all do.  In Rust it is the recommended style.  Haskell even
allows it in import and export lists.  It may indeed be better design
for the comma to be a (perhaps optionally omitted) terminator rather
than a separator.

`hpre` thus supports this for items split across lines, which is the
primary use case.  You can write

```haskell
   mapM_ print [
      itemOne,
      itemTwo,
      itemThree,
      ]
```

or

```haskell
   data Person = {
      Name :: Text,
      Age  :: Int,
      } deriving (Eq, Show)
```

and `hpre` will remove the final comma before sending to GHC.
Specifically, it looks for a comma at the end of one line (not
counting a `--` comment), followed by a closing delimiter at the
start of the next.

This could in principle conflict with the tuple section extension,
where `(True,)` is a function `a -> (Bool, a)`, but it would be
madness to insert a newline before the closing parenthesis.

_Leading_ commas are similarly supported, so these can be written:

```haskell
   mapM_ print [
      , itemOne
      , itemTwo
      ]

   data Person = {
      , Name :: Text
      , Age  :: Int
      } deriving (Eq, Show)
```

In addition, `hpre` supports leading _bars_ in `data` declarations:

```haskell
   data Foobar =
      | Foo
      | Bar
```

Trailing bars are not supported, and I currently have no plans to
add them.

There are other places where extra delimiters might be handy, such
as pattern guards and language extension lists, but these are much
less common and thus less troublesome, so as of now I have no plans
to support those either.


##  Tab expansion

The tab versus spaces debate is an old one, and still unresolved (e.g.,
Python has settled on spaces, Go on tabs).  The Haskell community
today endorses spaces, but I remain ambivalent.  GHC now warns on tabs
(although `-fno-warn-tabs` can disable that), but interprets them as
eight-space jumps.

Eight spaces is a lot, and most prefer an indentation of at most four.
So, code with tabs will be mis-aligned by the Haskell parser, which
is relevant with significant whitespace.

`hpre` has a (hardcoded) setting `tabWidth`, and it expands all tabs in
the source to use that as a tabstop before passing it along to GHC.
Thus, you can continue to use your favorite tabstop, and, if you change
your mind about it (an advantage of tabs), you can change it in `hpre`.

If you don’t use tabs, this feature has no effect.


##  Empty guards

Haskell uses guards to break down cases inside a `case` statement or
function definition.  The standard idiom for the “default” case
is to use `otherwise`.

```haskell
   fac n | n < 1     = 1
         | otherwise = n * fac (n-1)

   ... case a `divMod` b of
          (q, r) | odd r     -> r
                 | otherwise -> q + r
```

`otherwise` is not part of the language, but is defined in the standard
Prelude as the value `True`.

The first thing that bugs me about `otherwise` is that it’s _bulky_.
In many cases, as in the above two, it visually dwarfs the other
parts of the code, including the important condition.

It is also easily made unnecessary.  Indeed, Concurrent Clean has a
nicer solution, where the `otherwise` is optional.  Imagine this:

```haskell
   fac n | n < 1 = 1
         |       = n * fac (n-1)
```

To me this is much more pleasant and natural.  (In Clean the second
`|` is actually omitted, but I don’t follow that.)

And the idea fits nicely with existing syntax.  There is a “pattern
guards” extension, standard in Haskell2010, which allows multiple
conditions in a guard:

```haskell
   bar x | not (null x), Just y <- lookup 1 x, y /= 3 = ...
```

An empty guard is a small generalization of this, where the number
of conditions can be zero.

A practical application is that Haskell compilers should and do check
for incomplete definitions and cases.  To this end, `otherwise` is
special-cased into GHC as something known to always be true, which is
itself hacky.  Indeed, `elsewise = True; x | elsewise = ()` will issue
a warning with `-Wincomplete-patterns`.  If the compiler accepted zero
conditions, it could know _a priori_ that the guards are exhaustive.

More philosophically, `otherwise` feels like a hack.  The “else”
case should be part of the _syntax_, but instead the namespace
is needed.  If, say, for some reason you didn’t bring all of the
Prelude into scope, you might not have the syntactic ability to write
a default case.

`hpre` adds support for empty guards.  It checks for a `|` followed
by whitespace and an `=` or `->` (or `→`) on the same line, and
writes in a literal `True`.  This doesn’t give the full power that
a compiler-supported empty guard would, but it works well in practice.


##  Ditto marks

Haskell function definitions with multiple equations require repeating
the function name:

```haskell
   bigBulkyFunctionName :: Maybe [Int] -> (Int, Int)

   bigBulkyFunctionName Nothing        = ...
   bigBulkyFunctionName (Just [])      = ...
   bigBulkyFunctionName (Just [_])     = ...
   bigBulkyFunctionName (Just (a:b:_)) = ...
```

In programming, there is the principle of DRY (“Don’t Repeat
Yourself”), that it is best for some fact to appear in only one place.
This can be taken too far, but I feel it is applicable here, for the
usual reasons:

First, this is a lot of repetitive typing.  One can of course copy
and paste in any reasonable editor, but that’s analogous to saying
that a language is quick to code in, as long as you have an IDE to
generate all the tedious boilerplate.

Relatedly, we introduce a source of error:  A typo in one of those
copies can cause a failure, perhaps at runtime.  Suppose the last
line is mistyped as

```haskell
   bigBulkyFunctoinName (Just (a:b:_)) = ...
```

This module will still compile (although perhaps with more warnings),
but will error out at runtime if this case arises.  If it’s an uncommon
case, this might suddenly occur in production weeks later and bring
down your app.

This syntax appears to be motivated by mathematical notation.
But mathematicians tend to use short identifiers (such as _f_ or
**fib**), which are less troublesome, and of course write for humans,
who can correct for errors.

Next, a major motivation for DRY is the ease of maintenance and making
changes.  An example is developing an alternative implementation of
some function.  These co-exist during testing:

```haskell
   isPrime :: Integer -> Bool
   isPrime 0 = False
   isPrime 1 = False
   isPrime 2 = True
   isPrime n | n < 0  = False
             |        = all (\x -> n `mod` x /= 0) [2 .. n-1]

   isPrime' :: Integer -> Bool
   isPrime' n = ... some more efficient algorithm ...

   propItWorks x = isPrime x == isPrime' x
```

Once it’s time to swap in the new version, in most languages I
could just switch the names, but for Haskell I have to change every
definition.  This comes up frequently for me.  More generally, it
arises anytime I want to rename a function.

My solution is a “ditto mark”, `''`, which abbreviates the
identifier above it if it starts the line.  So, in

```haskell
   myfunc 0 = ...
   ''     1 = ...
```

the `''` expands to `myfunc`.

I find this dovetails nicely with type annotations:

```haskell
   bigBulkyFunctionName :: Maybe [Int] -> (Int, Int)

   ''  Nothing        = ...
   ''  (Just [])      = ...
   ''  (Just [_])     = ...
   ''  (Just (a:b:_)) = ...
```

Dittos don’t need to be on the top level; the only requirement is
that the function name be the first word on the line:

```haskell
   fromDigits base = loop where
      loop (x:l) = x * base + loop l
      ''   []    = 0
```

`hpre` also accepts the Unicode ditto mark (`”`) and the CJK ditto
mark (`〃`).

Use is entirely optional, and in many cases I prefer to spell out
the names:

```haskell
   get1, get2, get3 :: (a, a, a) -> a

   get1 (x, _, _) = x
   get2 (_, x, _) = x
   get3 (_, _, x) = x
```

Some may find ditto marks unpalatable, but I’m quite pleased with
how they have worked out in practice.


##  Multiplex imports

_This is an experimental feature I am still in the process of trying
out._

Haskell code is littered with pairs of lines such as this:

```haskell
   import qualified Data.Map.Strict as Map
   import Data.Map.Strict (Map)
```

This is obnoxious both to write out and to read.

In fact, the syntax of import statements is suboptimal in several ways.
Even the word `qualified` is a bit ugly, being long and messing up
alignment, so that some codebases conventionally use spaces to line
up names:

```haskell
   import           Control.Monad
   import qualified Data.Set as S
   import           Data.Set (Set)
```

(A language extension `ImportQualifiedPost` is available as of GHC
8.10 that moves `qualified` after the module name, which somewhat
obviates this.)

Meanwhile, qualification seems an unnecessary feature.  It is rare
that one wants to use `as` _un_&#xfeff;qualified.  No other language I
know of, including those Haskell-inspired, has any such keyword.
For instance, in PureScript `as` in an import always means qualified.

Haskell singularly optimizes its syntax for the least common case.

`hpre` extends and alters the import syntax with the following
two rules:

1.  A module may be imported multiple times in one statement, by
separating specifiers with commas.  For instance `import Foo as A,
as B` will import the module into both namespaces.

2.  An import with `as` is always qualified.

Thus, using `hpre`, the above two lines can be written as simply

```haskell
   import Data.Map.Strict (Map), as Map
```

Each comma-separated specifier is an optional `as` clause followed by
any of the usual: a list of symbols, a `hiding` clause, or nothing.
Examples:

```haskell
   import Data.List hiding (group), as List
   import Data.List.NonEmpty (group), as NE
   import Data.ByteString as B hiding (pack)
   import Data.ByteString.Char8 as B (pack)
   import Data.ByteString.Lazy (fromChunks), as B (toStrict), as BL
```

`hpre` expands each of these into multiple `import` statements.

The “unqualified as” import can be emulated as follows:

```haskell
import Control.Exception, as Exc
```

That is, this will import all symbols from the module both unqualified
and with `Exc.`.  (A _trailing_ comma, however, is ignored.)

If an import is explicitly `qualified`, it is left unchanged, although
eventually I may make this an error.

In Haskell, modules can be re-exported by adding `module X` to the
export list.  This exports symbols only if they are in scope _both_
unqualified and qualified as _X_.  A common idiom is to use one module
name for all exported symbols, e.g.,

```haskell
   module Foo (Foo(..), module Export) where
   import Control.Monad as Export
   import qualified Data.Text as T
   import Data.Text as Export (Text, pack, unpack)
```

With multiplex imports, any import can be marked for re-export simply
and consistently by adding (in this example) `, as Export`:

```haskell
   module Foo (Foo(..), module Export) where
   import Control.Monad, as Export
   import Data.Text (Text, pack, unpack), as T, as Export
```

An extension I’m considering is having `(..)` represent the
whole module, as in `import Foo (..)`, to avoid the odd notation of
separating off an empty specifier with a comma.

Unlike all other `hpre` features, multiplex imports can change the
meaning of valid Haskell programs, since `as` imports all become
qualified.  For this reason, it is off by default, and is activated
by putting `--+` on a line by itself, which causes import statements
to be processed from then on.

In the future, I may drop this condition, and simply have `hpre`
transform all imports, accepting that it is not fully compatible.
This will necessitate a 2.0 release.


##  Limitations and future work

These extensions can in principle interfere with alignment.  I try
to minimize this effect—for example, the `True` in empty guards
replaces an equal number of spaces when possible, and `COLUMN` pragmas
are used with ditto expansion—and I almost never encounter problems,
but it can happen.  As an example,

```haskell
     bad a b | b = 1
             |   = f a where f 0 = 5
                             f x = x + 1
```

will fail to parse because the `f`s will not align when `True`
is inserted.  However, I personally almost never use alignment in
this way, and anyway this is a marginal case; if the condition were
one character wider there would be no problem.

Until the ticked numbers feature is removed, similar alignment problems
could occur when `'` and `_` are removed from lines.

The parsing is somewhat primitive; I don’t try to handle Haskell’s
entire syntax.  As such, it’s possible to confuse it.  However,
I have used `hpre` in several large and complex projects without
problems.

All features I seriously considered for `hpre` have now been
implemented, so no additions are planned any time soon.


##  Installation and use

`hpre` can be installed by running `cabal install` or `stack install`
from its directory.  Alternatively, you can build the binary directly:

```bash
ghc --make -O hpre.hs
```

To use, add the command line options `-F -pgmF hpre` to `ghc` (or
`runghc`, `ghci`, `runhaskell`, `ghc-options` in `cabal`, etc.),
or add this line to individual Haskell files:

```haskell
{-# OPTIONS_GHC -F -pgmF hpre #-}
```

Make sure that wherever `hpre` is installed (e.g., `~/.cabal/bin/`),
your build system can find it, or put a full path after `-pgmF`.

