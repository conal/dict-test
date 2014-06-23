Test HERMIT dictionary construction.
The module `DictTest` (in src/) defines a transformation `succ`, which applies the successor function if an `Enum` dictionary can be build, using `buildDictionaryT` from `HERMIT.Dictionary.GHC`.
See HERMIT issue [Find/construct dictionary from class and type](https://github.com/ku-fpg/hermit/issues/88).

My expected result:
```haskell
test3 :: [Char]
test3 = (unpackCString# "oops!"#)        -- failure
test2 :: Int
test2 = succ Int $fEnumInt (I# 3)
test1 :: Bool
test1 = succ Bool $fEnumBool False
foo :: ()
foo = succ () $fEnum() ()
```

Instead, all three `succ` applications succeed, and but `test3` refers to a bogus dictionary variable:
```
test3 :: [Char]
test3 =
  succ [Char] $dGHC.Enum.Enum[GHC.Types.Char]
    (unpackCString# "oops!"#)
```

Moreover, *all three* definitions succeed with this bogus form when the name `succ` does not appear in the pre-transformed module.
I've worked around by adding
```haskell
foo :: ()
foo = succ ()
```
Thanks to Andrew Farmer for this clue.

Install and run as follows:

```
bash-3.2$ cabal install
...
Installed dict-test-0.0
bash-3.2$ cd test
bash-3.2$ hermit Test.hs -v0 -opt=DictTest DoTest.hss
[starting HERMIT v0.5.0.0 on Test.hs]
% ghc Test.hs -fforce-recomp -O2 -dcore-lint -fsimple-list-literals -fexpose-all-unfoldings -fplugin=DictTest -fplugin-opt=DictTest:-v0 -fplugin-opt=DictTest:DoTest.hss -fplugin-opt=DictTest:*: -v0
test1 :: Bool
test1 = succ Bool $dGHC.Enum.EnumGHC.Types.Bool False
test2 :: Int
test2 = succ Int $dGHC.Enum.EnumGHC.Types.Int (I# 3)
hermit<7> resume
*** Core Lint errors : in result of Core plugin:  HERMIT0 ***
<no location info>: Warning:
    In the expression: GHC.Enum.succ
                         @ GHC.Types.Bool $dGHC.Enum.EnumGHC.Types.Bool_s18j GHC.Types.False
    $dGHC.Enum.EnumGHC.Types.Bool_s18j :: GHC.Enum.Enum GHC.Types.Bool
    [LclId, Str=DmdType] is out of scope
<no location info>: Warning:
    In the expression: GHC.Enum.succ
                         @ GHC.Types.Int $dGHC.Enum.EnumGHC.Types.Int_s18m (GHC.Types.I# 3)
    $dGHC.Enum.EnumGHC.Types.Int_s18m :: GHC.Enum.Enum GHC.Types.Int
    [LclId, Str=DmdType] is out of scope
*** Offending Program ***
Test.test1 :: GHC.Types.Bool
[LclIdX, Str=DmdType]
Test.test1 =
  GHC.Enum.succ
    @ GHC.Types.Bool $dGHC.Enum.EnumGHC.Types.Bool_s18j GHC.Types.False

Test.test2 :: GHC.Types.Int
[LclIdX, Str=DmdType]
Test.test2 =
  GHC.Enum.succ
    @ GHC.Types.Int $dGHC.Enum.EnumGHC.Types.Int_s18m (GHC.Types.I# 3)

*** End of Offense ***
```
