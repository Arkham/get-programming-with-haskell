# Functional OOP with Robots

- Use functional programming to create objects
- Create example objects that interact with each other
- Represent state in a functional way

## A cup of coffee

```
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
```

## Fighting robots

Each robot has:
- a name
- an attack strength
- a number of hit points

We can represent that with a three elements tuple:

`("Bob", 10, 100)` represents a robot named Bob that has an attack of 10
and has 100 hit points.

```
module Robot where

robot (name, attack, hp) = \message -> message (name, attack, hp)

name (n, _, _) = n

attack (_, a, _) = a

hp (_, _, hp) = hp

getName aRobot = aRobot name

getAttack aRobot = aRobot attack

getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))

setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))

setHP aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))

printRobot aRobot =
  aRobot (\(n, a, h) -> n ++ " attack:" ++ (show a) ++ " hp:" ++ (show h))

killerRobot = robot ("Kill3r", 25, 200)

printStuff =
  let nicerRobot = setName killerRobot "kitty"
      gentlerRobot = setAttack killerRobot 5
      softerRobot = setHP killerRobot 50
   in map printRobot [killerRobot, nicerRobot, gentlerRobot, softerRobot]

damage aRobot attackDamage =
  aRobot
    (\(n, a, h) ->
       let newHP = max (h - attackDamage) 0
        in robot (n, a, newHP))

fight aRobot defender = damage defender attack_
  where
    attack_ =
      if getHP aRobot > 10
        then getAttack aRobot
        else 0

gentleGiant = robot ("Mr. Friendly", 10, 300)

fightClub =
  let gentleGiantRound1 = fight killerRobot gentleGiant
      killerRobotRound1 = fight gentleGiant killerRobot
      gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
      killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
      gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
      killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2
   in map printRobot [gentleGiantRound3, killerRobotRound3]

deathmatch =
  let fastRobot = robot ("speedy", 15, 40)
      slowRobot = robot ("slowpoke", 20, 30)
      fastRobotRound1 = fight slowRobot fastRobot
      slowRobotRound1 = fight fastRobot slowRobot
      fastRobotRound2 = fight slowRobotRound1 fastRobotRound1
      slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
      fastRobotRound3 = fight slowRobotRound2 fastRobotRound2
      slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
   in map printRobot [fastRobotRound3, slowRobotRound3]
```
