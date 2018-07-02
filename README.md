`hpre` is a simple Haskell code pre-processor that adds a few minor
syntactic features.  They are meant to be minimal, and increase
productivity without harming readability.

I have been using Haskell since 2005 and am largely self-taught.
As such, I developed my own habits and conventions, and found a few
points that bothered me.  I have slowly developed `hpre` (formerly
`gghc`) over the years to address these.

To use, add the command line options `-F -pgmF hpre` to `ghc` (or
`runghc`, `ghci`, `runhaskell`, etc.).  `hpre` needs to be compiled
and put in your `$PATH`, or you can provide an absolute path.
An individual file can also just include this line:

```haskell
{-# OPTIONS_GHC -F -pgmF hpre #-}
```

Here are the features, along with their rationales:


##  Ticked numbers

_An extension `NumericUnderscores` will land in GHC 8.6 that provides this feature._

Large integer literals can be hard to read off.  Many languages
provide native syntax for spacing them out.  For example, Perl allows
one to write one billion as `1_000_000_000` rather than the hypnotic
`1000000000`.  Haskell does not, so I've added it.

My first try at a spacing symbol is a tick mark (apostrophe), `'`, but
I'm also experimenting with `_`.  So you can write `1'000'000'000` or
`1_000_000_000`, and `hpre` will take out the non-digits.  This made
a number of my mathematical projects more readable.

Since GHC 7.8, the language option `NumDecimals` allows writing one
billion (of an `Integral` type) as `1e9`.  However, you might not
want this extension, and anyway it won't help if your number is not
mostly a row of zeroes, such as `1'073'741'824`.

No existing Haskell syntax (including GHC extensions) appears to
conflict with either notation.


##  Trailing commas

That the last item in a comma-separated list does not take a comma is
a recurring nuisance.  It is easy when adding to the list, or moving
entries around, to fail to attend to this exception.  Furthermore,
code diffs look like this:

```diff
-    itemSeven
+    itemSeven,
+    itemEight
```

This can make it harder to see at a glance that the only change was
adding an item.

The Haskell community's answer to this has been to adopt
“Utrecht-style” commas, at the start of the line:

```haskell
   mapM_ print
      [ itemOne
      , itemTwo
      , itemThree
      ]
```

The idea being that you can add `, itemFour` easily.

However, this has two drawbacks.  First, and this is subjective,
it is ugly.  Commas were not meant to be dangled like this, but to
sit comfortably after a word.

Second, and more importantly, it doesn't even fix the problem.
Instead of the last item being exceptional, now the first is.
The motivation appears to be that it is more common to add to the end
of a list than the beginning.  But one might want to move `itemOne`
down, or add before it, or remove it.

Many other languages sensibly allow a _trailing comma_ in such lists,
after the last item.  Java, Python, Perl, Ruby, and modern Javascript
(ES5) all do.  Haskell even allows it in import and export lists.
In Rust it is the recommended style.  It may indeed be better design
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

I also have experimental support for initial commas:

```haskell
   mapM_ print [
      , itemOne
      , itemTwo
      ]
```


##  Tab expansion

The tab versus spaces debate is an old one, and still unresolved
(e.g., Go defies the general trend by enforcing tabs).  The Haskell
community today firmly endorses spaces, but I remain ambivalent.
GHC now warns on tabs (although `-fno-warn-tabs` can disable that),
but interprets them as eight-space jumps.

Eight spaces is a lot, and most prefer an indentation of at most four.
So, code with tabs will be mis-aligned by the Haskell parser, which
is relevant with significant whitespace.

`hpre` has a (hardcoded) setting `tabWidth`, and it expands all tabs in
the source to use that as a tabstop before passing it along to GHC.
Thus, you can continue to use your favorite tabstop, and, if you change
your mind about it (an advantage of tabs), you can change it in `hpre`.

If you don't use tabs, this feature has no effect.


##  Empty guards

Haskell uses guards to break down cases inside a `case` statement or
function definition.  The standard idiom for the “default” case
is to use `otherwise`.

```haskell
   fac n | n < 1     = 1
         | otherwise = n * fac (n-1)

   ... case a `divMod` b of
          (q, r) | flag      -> r
                 | otherwise -> q + r
```

`otherwise` is not part of the language, but defined in the standard
Prelude as the value `True`.

The first thing that bugs me about `otherwise` is that it's _bulky_.
In many cases, as in the above two, it visually dwarfs the other
parts of the code, including the important condition.

It is also easily made unnecessary.  Indeed, Concurrent Clean has a
nicer solution, where the `otherwise` is optional.  Imagine this:

```haskell
   fac n | n < 1 = 1
         |       = n * fac (n-1)
```

To me this is much more pleasant and cleaner.  (NB:  In Clean the
second `|` is actually omitted, but I don't copy that.)

And the idea fits nicely.  Consider the “pattern guards” extension,
standard in Haskell 2010, which allows multiple conditions in a guard:

```haskell
   bar x | not (null x), Just y <- lookup 1 x, y /= 3 = ...
```

An empty guard is a natural generalization of this, where the number
of conditions can be zero.

A practical application is that Haskell compilers should and do check
for incomplete definitions and cases.  To this end, `otherwise`
is special-cased into GHC as something known to always be true,
which is ugly itself.  If the compiler accepted zero conditions,
it could know _a priori_ that the guards are exhaustive.

More philosophically, `otherwise` seems like a hack.  The “else”
case should be part of the _syntax_, but instead one needs the
namespace.  If, say, for some reason you didn't bring all of the
Prelude into scope, you might not have the syntactic ability to write
a default case.

`hpre` adds support for empty guards.  It checks for a `|` followed by
whitespace and an `=` or `->` on the same line, and writes in a literal
`True`.  This doesn't give the full power that a compiler-supported
empty guard would, but it works well in practice, and allows the last
`fac` function to be written.


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

In programming, there is the principle of DRY (“Don't Repeat
Yourself”), that it is best for some fact to appear in only one place.
This can be taken too far, but I feel it is applicable here, for the
usual reasons:

First, this is a lot of repetitive typing.  One can of course copy
and paste in any reasonable editor, but that's analogous to saying
that a language is quick to code in, as long as you have an IDE to
do all the tedious stuff.

Relatedly, we introduce a source of error:  A typo in one of those
copies can cause a failure, perhaps at runtime.  Suppose the last
line is mistyped as

```haskell
   bigBulkyFunctoinName (Just (a:b:_)) = ...
```

Note the `i`/`o` transposition.  This module will still compile
(although perhaps with more warnings), but will error out at runtime
if this case arises.  If it's an uncommon case, this might suddenly
occur in production weeks later and bring down your app.

This syntax appears to be motivated by mathematical notation, but
mathematicians tend to use short identifiers (such as _f_ or **fib**),
which are less troublesome, and of course write for humans, who can
correct for errors.

Next, a major motivation for DRY is the ease of maintenance and making
changes.  An example is developing an alternative implementation of
some function.  These co-exist during testing:

```haskell
   isPrime :: Integer -> Bool
   isPrime n = ... even worse algorithm than below ...

   isPrime' :: Integer -> Bool
   isPrime' 0 = False
   isPrime' 1 = False
   isPrime' 2 = True
   isPrime' n | n < 0  = False
              |        = all (\x -> n `mod` x /= 0) [2 .. n-1]

   propItWorks x = isPrime x == isPrime' x
```

Once it's time to swap in the new version, in most languages I
could just switch the names, but for Haskell I have to change every
definition.  This comes up a lot for me.  More generally, I might
rename a function for any reason.

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

Some may find ditto marks unpalatable, but I've been quite pleased
with how they have worked out in practice.


##  Limitations and future work

These extensions can in principle interfere with alignment.  I try to
minimize this effect—for example, the `True` in empty guards replaces
an equal number of spaces when possible—and I almost never encounter
problems, but one should be aware of the possibility.  As an example,

```haskell
area b x = 3.141_593 * sq b where sq True  = x*x
                                  sq False = x
```

will fail because the `sq`s will not align when the `_` is taken out.
However, I never write code like that.

The parsing is somewhat primitive; I don't try to handle Haskell's
entire syntax.  As such, it's surely possible to confuse it.  However,
I have used `hpre` in several large and complex projects without
problems.

Other ideas I am considering:

###  Import lists

I get annoyed having to repeatedly type out the likes of this:

```haskell
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
```

There is also the general redundancy of writing `import` over and over.
I have some ideas for “import blocks”.

###  Data definitions

Possibly, the trailing comma idea could extend
to `data` definitions, something like

```haskell
data Foobar =
   | Foo
   | Bar
```

###  More adjustments

The issues with alignment such as those above could probably be
addressed with more effort.  Also, use of COLUMN pragmas is a work
in progress.


##  Installation

Knowledge of Haskell is assumed, so building should be straightforward.

```bash
ghc --make -O hpre.hs
```

Note that Haskell pre-processors take three arguments: the module name,
the input file, and the output file.  This differs from typical command
line functions, which for instance can usually take stdin to stdout.

