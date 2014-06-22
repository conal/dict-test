Test HERMIT dictionary construction.
Currently appears to be broken.

To build, install and run as follows:

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


<no location info>: 
Compilation had errors
```
