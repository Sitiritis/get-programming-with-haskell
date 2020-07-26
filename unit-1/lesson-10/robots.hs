robot (name, attack, hp) = \message -> message (name, attack, hp)

killerRobot = robot ("Kill3r", 25, 200)
gentleGiant = robot ("Mr. Friendly", 10, 300)
fastRobot = robot ("speedy", 15, 40)
slowRobot = robot ("slowpoke", 20, 30)

team1 = [killerRobot, fastRobot, slowRobot]

name (n, _, _) = n
attack (_, a, _) = a
hp (_, _, hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHp aRobot = aRobot hp

setName aRobot newName = aRobot (\(_, a, h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, _, h) -> robot (n, newAttack, h))
setHp aRobot newHp = aRobot (\(n, a, _) -> robot (n, a, newHp))

printRobot aRobot = aRobot (\(n, a, h) ->
  n ++
  " attack: " ++ show a ++
  " hp: " ++ show h)

damage aRobot attackDamage = aRobot (\(n, a, h) ->
  robot (n, a, h - attackDamage))

fight aRobot defender = damage defender attack
  where attack = if getHp aRobot > 0
                 then getAttack aRobot
                 else 0

gentleAttack = fight gentleGiant
fightAll = map gentleAttack
