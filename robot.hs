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

threeRoundFight = fightWithRounds killerRobot gentleGiant 3

fightWithRounds aRobot bRobot 0 = map printRobot [aRobot, bRobot]
fightWithRounds aRobot bRobot rounds =
  let newBRobot = fight aRobot bRobot
      newARobot = fight bRobot aRobot
   in fightWithRounds newARobot newBRobot (round - 1)
