Test HERMIT dictionary construction.
The module `DictTest` (in src/) defines a transformation `succ`, which applies the successor function if an `Enum` dictionary can be build, using `buildDictionaryT` from `HERMIT.Dictionary.GHC`.
See HERMIT issue [Find/construct dictionary from class and type](https://github.com/ku-fpg/hermit/issues/88).

My expected result:
```haskell
test1 :: Bool
test1 = succ Bool $fEnumBool False
test2 :: Int
test2 = succ Int $fEnumInt (I# 3)
test3 :: [Char]
test3 = (unpackCString# "oops!"#)        -- failure
```

Instead, all three `succ` applications succeed, and but `test3` refers to a bogus dictionary variable:
```
test3 :: [Char]
test3 = succ [Char] $dGHC.Enum.Enum[GHC.Types.Char] (unpackCString# "oops!"#)
```

Moreover, *all three* definitions succeed with this bogus form when `Enum` does not appear in the pre-transformed module.
I've worked around by adding the following:
```haskell
foo :: ()
foo = pred ()
```
Thanks to Andrew Farmer for this clue.

Install and run as follows:

```
bash-3.2$ cabal install
...
Installed dict-test-0.0
bash-3.2$ cd test
test3 :: [Char]
test3 =
  succ [Char] $dGHC.Enum.Enum[GHC.Types.Char]
    (unpackCString# "oops!"#)
test2 :: Int
test2 = succ Int $fEnumInt (I# 3)
test1 :: Bool
test1 = succ Bool $fEnumBool False
hermit<10> resume
*** Core Lint errors : in result of Core plugin:  HERMIT0 ***
<no location info>: Warning:
    In the expression: GHC.Enum.succ
                         @ [GHC.Types.Char]
                         $dGHC.Enum.Enum[GHC.Types.Char]_s18M
                         (GHC.CString.unpackCString# "oops!"#)
    $dGHC.Enum.Enum[GHC.Types.Char]_s18M
      :: GHC.Enum.Enum [GHC.Types.Char]
    [LclId, Str=DmdType] is out of scope
*** Offending Program ***
Test.test3 :: GHC.Base.String
[LclIdX, Str=DmdType]
Test.test3 =
  GHC.Enum.succ
    @ [GHC.Types.Char]
    $dGHC.Enum.Enum[GHC.Types.Char]_s18M
    (GHC.CString.unpackCString# "oops!"#)

Test.test2 :: GHC.Types.Int
[LclIdX, Str=DmdType]
Test.test2 =
  GHC.Enum.succ @ GHC.Types.Int GHC.Enum.$fEnumInt (GHC.Types.I# 3)

Test.test1 :: GHC.Types.Bool
[LclIdX, Str=DmdType]
Test.test1 =
  GHC.Enum.succ @ GHC.Types.Bool GHC.Enum.$fEnumBool GHC.Types.False

Test.foo :: ()
[LclIdX, Str=DmdType]
Test.foo = GHC.Enum.succ @ () GHC.Enum.$fEnum() GHC.Tuple.()

*** End of Offense ***
```


For now, I'm working around the problem with a tweak to `buildDictionaryT` that checks for empty bindings (`null bnds`):

```haskell
buildDictionaryT :: Transform c HermitM Type CoreExpr
buildDictionaryT = contextfreeT $ \ ty -> do
    dflags <- getDynFlags
    binder <- newIdH ("$d" ++ filter (not . isSpace) (showPpr dflags ty)) ty
    guts <- getModGuts
    (i,bnds) <- liftCoreM $ buildDictionary guts binder
    if (null bnds)
      fail "couldn't build dictionary"
     else return $ case bnds of
                     [NonRec v e] | i == v -> e -- the common case that we would have gotten a single non-recursive let
                     _ -> mkCoreLets bnds (varToCoreExpr i)
```

 </blockquote>

Now I use this definition if the CPP symbol `MyBuildDict` is defined.
