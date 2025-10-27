import Control.Monad.State
import System.Random


-----------------------------------------------------
-- A hero in peril is fighting a dragon. At each turn, the hero picks
-- a weapon to strike the dragon. The more powerful weapons which have
-- a lot of hit points, also drain the stamina of the hero. If stamina
-- becomes 0 or negative, the hero can fight no longer, and awaits a
-- certain death while the dragon keeps attacking. Choose your weapon
-- wisely.
--
-- the dragon only has a fixed attack, which does only a
-- moderate amount of damage, but has a lot of health.
------------------------------------------------------



-----------------------------------------
-- Data types
-----------------------------------------
data Character = Hero {h_name::[Char],
                       hero_health::Integer,
                       hero_stamina::Integer} deriving Show
data Dragon = Dragon {d_name::[Char],
                      dragon_health::Integer} deriving Show
data Weapon = W { w_name::[Char],
                  hit_points:: Integer,
                  stamina_penalty::Integer} deriving Show
type GameState = (Character,Dragon,Weapon)

---------------------------------------------
-- The main protagonists
-- Aragorn did not kill Smaug, but this is just an illustration
---------------------------------------------
smaug = Dragon{d_name="Smaug",dragon_health=10000}
aragorn = Hero{h_name="Aragorn",
                hero_health=2000,
                hero_stamina=200}
sword = W {w_name="sword",hit_points=200,stamina_penalty=10}
battle_axe = W {w_name="battle axe", hit_points=400,stamina_penalty=20}
halberd = W {w_name="halberd",hit_points=4000,stamina_penalty=40}

----------------------------------------------------------
-- Utility Functions
----------------------------------------------------------
get_dragon_health Dragon{dragon_health=h} = h
get_dragon_name Dragon{d_name=n} = n
get_hero_health Hero{hero_health=h} = h
get_hero_name   Hero{h_name=n} = n
get_hero_stamina Hero{hero_stamina=s} = s
get_weapon_name W{w_name=w} = w
get_stamina_penalty W{stamina_penalty=p} = p
get_hit_points W{hit_points=h} = h

wound_hero :: Integer -> StateT GameState IO ()
wound_hero hit = do
  (c,d,w) <- get
  put (c{hero_health=(get_hero_health c)-hit},d,w)
  return ()

wound_dragon :: Integer -> StateT GameState IO ()
wound_dragon hit = do
  (c,d,w) <- get
  put (c{hero_stamina = (get_hero_stamina c)-(get_stamina_penalty w)},
        d{dragon_health=(get_dragon_health d)-hit},w)
  return ()

--------------------------------------------------------
-- Game state transformations. These encode the game logic
---------------------------------------------------------
gameStep :: StateT GameState IO ()
gameStep = do
  (lift.print) "Which weapon do you want to want to yield? [0. Sword  1. Battle-axe 2. Halberd]"
  weapon_choice_string <- lift getLine
  let weapon = [sword, battle_axe, halberd] !! (read weapon_choice_string::Int)
  (c,d,_) <- get
  put (c,d,weapon) -- update the state by changing the weapon

  -- =================================
  -- update the health 
  -- uses the top level State monad
  -- =================================
  randomHit <- lift randomIO
  wound_hero (200-(randomHit `mod` 200)) -- dragon has a fixed max hit point
                                         -- actual hit point may be lower due to randomness
  randomHit2 <- lift randomIO
  (c,d,w) <- get
  -- nop is written as (wound_dragon 0) to satisfy the type system
  if (get_hero_stamina c)>0 then wound_dragon (randomHit2 `mod` 10000) else wound_dragon 0
  show_game_status

-- ===================================
-- show : uses the underlying IO monad
-- ===================================
show_game_status = do
  (c,d,w) <- get
  let
    h_health = (max (get_hero_health c) 0)
    
    msg = if h_health > 0
      then (get_hero_name c)++
           " strikes with his " ++ (get_weapon_name w) ++  
           " and is grievously wounded, with " ++ (show h_health) ++ " points"
      else (get_hero_name c)++" lies dead."
  lift (print msg)
  let
    d_health = (max (get_dragon_health d) 0)
    msg = if d_health > 0
      then (get_dragon_name d)++
           " is grievously wounded, with " ++ (show d_health) ++ " points"
      else (get_dragon_name d)++" lies dead."
  lift (print msg)  -- since print is from underlying IO
  return ()

gameLoop :: StateT GameState IO ()
gameLoop = do
  gameStep
  (c,d,w) <- get
  if (get_hero_health c) <= 0 || (get_dragon_health d) <= 0
    then return ()
    else gameLoop

main = do
  runStateT gameLoop (aragorn,smaug,sword)
  return ()
  
