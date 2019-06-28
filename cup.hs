module Cup where

cup f10z = \message -> message f10z

drink aCup ozDrank = cup amount
  where
    amount = max (f10z - ozDrank) 0
    f10z = getOz aCup

getOz aCup = aCup (\f10z -> f10z)

haveSomeSips =
  let coffeeCup = cup 12
      afterASip = drink coffeeCup 1
      afterTwoSips = drink afterASip 1
      afterGulp = drink afterTwoSips 4
      afterHugeGulp = drink afterGulp 10
   in [ getOz afterASip
      , getOz afterTwoSips
      , getOz afterGulp
      , getOz afterHugeGulp
      ]

isEmpty aCup = getOz aCup == 0

haveFoldedSips =
  let coffeeCup = cup 12
      result = foldl drink coffeeCup [1, 2, 3, 4]
   in getOz result
