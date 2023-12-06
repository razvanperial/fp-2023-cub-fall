module Lib where 

data Player = Player
    { playerHealth    :: Int
    , playerArmor     :: Int
    , playerAttack    :: Int
    , playerDexterity :: Int
    , playerStrength  :: Int
    }

calculatePlayerDamage :: Int -> Int -> Int
calculatePlayerDamage attack strength = attack + strength

calculatePlayerDefense :: Int -> Int -> Int
calculatePlayerDefense armor dexterity = armor * dexterity

calculateHit :: Int -> Int -> Int -> Int
calculateHit damage defense health = health + defense - damage

-- The second player hits first player and the new first player is returned
hitPlayer :: Player -> Player -> Player
hitPlayer player1 player2 =
    let damage = calculatePlayerDamage
            (playerAttack player2)
            (playerStrength player2)
        defense = calculatePlayerDefense
            (playerArmor player1)
            (playerDexterity player1)
        newHealth = calculateHit
            damage
            defense
            (playerHealth player1)
    in player1 { playerHealth = newHealth }