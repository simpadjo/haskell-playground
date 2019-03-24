module Einstein where

{--
   https://en.wikipedia.org/wiki/Zebra_Puzzle
--}
import Data.List

--Describing features
data Feature = House | Nation | Color | Animal | Drink | Cig
  deriving (Eq, Show)

newtype Value = Value {idx :: Int}
  deriving (Eq, Show)

eng = (Nation, Value 1)
esp = (Nation, Value 2)
ukr = (Nation, Value 3)
nor = (Nation, Value 4)
jap = (Nation, Value 5)

red = (Color, Value 1)
green = (Color, Value 2)
blue = (Color, Value 3)
yellow = (Color, Value 4)
white = (Color, Value 5)

dog = (Animal, Value 1)
snail = (Animal, Value 2)
fox = (Animal, Value 3)
horse = (Animal, Value 4)
zebra = (Animal, Value 5)

coffee = (Drink, Value 1)
tea = (Drink, Value 2)
milk = (Drink, Value 3)
juice = (Drink, Value 4)
water = (Drink, Value 5)

sigKool = (Cig, Value 1)
sigOG = (Cig, Value 2)
sigChest = (Cig, Value 3)
sigLS = (Cig, Value 4)
sigParl = (Cig, Value 5)

--Describing restrictions
type Condition = Persons -> Maybe Bool

conj :: Condition -> Condition -> Condition
conj c1 c2 p =
  case c1 p of
    Just True -> c2 p
    Just False -> Just False
    Nothing -> case c2 p of
                 Just False -> Just False
                 _ ->  Nothing

lookupFeature :: (Feature, Value)-> Feature -> Persons -> Maybe Value
lookupFeature f f2 (Persons ps _) =
  do
    person <- find (\p -> elem f (attrs p)) ps
    attrValue <- find (\e -> (fst e) == f2) (attrs person)
    return $ (snd attrValue)

neighbors :: (Feature, Value) -> (Feature, Value) -> Condition
neighbors f1 f2 ps =
      do
        p1 <- lookupFeature f1 House ps
        p2 <- lookupFeature f2 House ps
        let id1 = idx p1 in
          let id2 = idx p2 in
            return $ (id1 == id2 + 1) || (id1 == id2 -1)

simpleCondition :: (Feature, Value) -> (Feature, Value) -> Condition
simpleCondition e (f2, v2) ps =
  do
    v <- lookupFeature e f2 ps
    return  $ v == v2

zeroCondition :: Condition
zeroCondition _ = Just True

cond6 :: Condition
cond6 ps =
  do
    idWhite <- lookupFeature white House ps
    idGreen <- lookupFeature green House ps
    return $ (idx $ idGreen) == (idx idWhite) + 1

allConditions = [
                     simpleCondition eng red,
                     simpleCondition esp dog,
                     simpleCondition green coffee,
                     simpleCondition ukr tea,
                     cond6,
                     simpleCondition snail sigOG,
                     simpleCondition yellow sigKool,
                     simpleCondition (House, Value 3) milk,
                     simpleCondition nor (House, Value 1),
                     neighbors sigChest fox,
                     neighbors horse sigKool,
                     simpleCondition sigLS juice,
                     simpleCondition jap sigParl,
                     neighbors nor blue
                   ] :: [Condition]

joinedCondition = foldr conj zeroCondition allConditions

--Describing search state

--Could use Map instead but don't want to add a dependency
newtype Person = Person {attrs :: [(Feature, Value)]}
  deriving (Eq, Show)

data Persons = Persons {persons :: [Person], freeFeatures :: [Feature]}
  deriving (Eq, Show)

initialState :: Persons
initialState = Persons (fmap mkPerson [1..5]) [Nation, Color, Animal, Drink, Cig] where
  mkPerson i = Person $ [(House, Value i)]

--Traversal of the search tree
setValues :: [Person] -> Feature -> [Value] -> [Person]
setValues ps f v =
    fmap app (zip ps v)
    where
      app :: (Person, Value) -> Person
      app ((Person attrs), w) = Person ((f, w) : attrs)


--TODO: use coerce?
perms :: [[Value]]
perms = permutations lst where
  lst = fmap Value [1..5]

expand :: Persons -> [Persons]
expand (Persons ps freeFeatures)  =
  case freeFeatures of
    f : fs -> fmap (\perm -> Persons (setValues ps f perm) fs) perms
    [] -> []

solve :: Condition -> Persons -> [Persons]
solve c p = case c p of
              Just True -> [p]
              Just False -> []
              Nothing -> (expand p) >>= (solve c)

run = solve joinedCondition initialState
