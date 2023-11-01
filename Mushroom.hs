module Mushroom where
import Data.Ratio ((%), Ratio)
import Data.Tuple (swap)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Data.Function


--
-- --                                      Pre-defined functions.
-- 
-- These functions are used to convert between counts, rationals, and string percentages. The counts
-- are similar to those used in the summaries for Project 1. You may not need these functions at all,
-- depending on your implementation choices.

outOf :: Int -> Int -> Rational
outOf a b =  (fromIntegral a) % (fromIntegral b)

ratioOfCount :: (Int, Int) -> Rational
ratioOfCount (a,b) = (fromIntegral a) % (fromIntegral $ a+b)

percentOfRatio :: Rational -> String
percentOfRatio r = (show $ truncate $ 100 * r) ++ "%"

percentOfCount :: (Int, Int) -> String
percentOfCount c = percentOfRatio $ ratioOfCount c


-- All undefined values and functions should be completed. Your code will compile 
-- even if some functions are left undefined.

--
-- --                                       Milestone Part One
--
observationsStr =
  unlines [  "poison,white,brown,brown,knobbed,missing,narrow,musty"
            ,"poison,white,brown,brown,knobbed,missing,narrow,musty"
            ,"edible,white,brown,brown,knobbed,missing,broad,musty"
            ,"edible,white,brown,brown,knobbed,missing,broad,musty"
            ,"edible,white,brown,brown,knobbed,missing,broad,musty"
            ,"edible,white,brown,white,knobbed,missing,narrow,musty"
            ,"edible,white,brown,white,knobbed,missing,narrow,musty"
            ,"poison,white,brown,white,knobbed,missing,broad,musty"
            ,"edible,white,brown,brown,bell,missing,narrow,musty"
            ,"edible,white,brown,brown,bell,missing,narrow,musty"
            ,"poison,white,brown,brown,bell,missing,broad,musty"
            ,"poison,white,brown,brown,bell,missing,broad,musty"
            ,"poison,white,brown,brown,bell,missing,broad,musty"
            ,"poison,white,brown,white,bell,missing,narrow,musty"
            ,"poison,white,brown,white,bell,missing,narrow,musty"
            ,"edible,white,brown,white,bell,missing,broad,musty"
          ]

observations = fromJust $ readObservationFile observationsStr

testobsStr = unlines [  "poison,white,brown,brown,knobbed,missing,narrow,musty"
                       ,"poison,white,brown,brown,knobbed,missing,broad,musty"
                       ,"poison,white,brown,brown,bell,missing,broad,musty"
                       ,"poison,white,brown,brown,bell,missing,broad,none"
                       ,"edible,white,brown,brown,knobbed,missing,broad,none"
                       ,"edible,white,brown,brown,bell,missing,narrow,none"]

tobsStr = unlines [ "poison,white,brown,brown,knobbed,missing,narrow,musty"
                   ,"poison,white,brown,brown,knobbed,missing,narrow,musty"
                   ,"poison,white,brown,brown,knobbed,missing,narrow,musty"
                   ,"edible,white,green,brown,knobbed,missing,narrow,musty"
                   ,"edible,white,green,brown,knobbed,missing,narrow,none"
                   ,"edible,white,green,brown,knobbed,missing,narrow,none"
                   ,"edible,white,green,brown,knobbed,missing,broad,none"
                   ,"edible,white,green,brown,knobbed,missing,broad,musty"]
tobs = fromJust $ readObservationFile observationsStr
type Guide = [(Int, String)]

-- Mushroom edibility is either Nom or NoNom. We should only eat mushrooms we are pretty certain are
-- Noms. Please do not actually eat any mushrooms based on advice from this project.
data Edible = Nom | NoNom deriving (Show, Eq, Ord)

--Define an algebraic data type for all the different possible attributes.
--You may decompose this by having sub-types (like Operators and Tokens for the class project),
--but every possible attribute should be representable as an Attribute.
--You may not use Chars, Strings, or Integers in this type.  You should derive Eq and Show (at least to
--start). You may also derive Ord, but do not rely on the ordering. 
data Attribute = --Edibility Edibility
                 StalkColor Color
                | CapColor Color
                | SporeColor Color
                | CapShape CapShape
                | StalkShape StalkShape
                | GillSize GillSize
                | Odor Odor
                deriving (Show, Eq)
--constructors for types
--data Edibility = SafeToEat | Poison deriving (Eq, Show)
data Color = Brown | Green | Purple | White | Yellow deriving (Eq, Show)
data CapShape = Bell | Conical | Knobbed deriving (Eq, Show)
data StalkShape = Bulbous | Club | Missing deriving (Eq, Show)
data GillSize = Broad | Narrow deriving (Eq, Show)
data Odor = Almond | Anise | Foul | None | Musty deriving (Eq, Show)

-- Make a list of all possible attributes. There should be 28 different attributes.
allAttributes :: [Attribute]
allAttributes = [ StalkColor color | color <- [Brown, Green, Purple, White, Yellow] ]
             ++ [ CapColor color | color <- [Brown, Green, Purple, White, Yellow] ]
             ++ [ SporeColor color | color <- [Brown, Green, Purple, White, Yellow] ]
             ++ [ CapShape shape | shape <- [Bell, Conical, Knobbed] ]
             ++ [ StalkShape shape | shape <- [Bulbous, Club, Missing] ]
             ++ [ GillSize size | size <- [Broad, Narrow] ]
             ++ [ Odor smell | smell <- [Almond, Anise, Foul, None, Musty] ]
            -- ++ [ Edibility edibility | edibility <- [SafeToEat, Poison] ]
--code review is gonna murder me lol

--A mushroom is a list of attributes.
type Mushroom = [Attribute]

--An observation is a mushroom that is known to be nomable, or not nomable.  Thus, it is a tuple of
--a mushroom and an edibility.
type Observation = (Mushroom, Edible)

-- readObservation takes a single line of the input file, in the format described on the
-- project page, and return the corresponding observation.  You may find the splitOn function
-- helpful. splitOn takes a string delimiter, a string to be split, and splits the string based on
-- the delimiter.  For instance (words str) is the same as (splitOn " " str)
-- I suggest you make helper functions for reading different columns.
-- The sequence function may be helpful.


idxHelper :: [String] -> Int -> String
idxHelper attributes n =
    let tmp = zip attributes [0..n-1]
    in fst (last tmp)
--lets me spit out the specific string based on the column its in. indexing starts at 1

readObservation :: String -> Maybe Observation
readObservation line = do
    let attributes = splitOn "," line
        --edibilityVal = parseEdibility (idxHelper attributes 1)
        stalkColorVal = parseColor (idxHelper attributes 2)
        capColorVal = parseColor (idxHelper attributes 3)
        sporeColorVal = parseColor (idxHelper attributes 4)
        capShapeVal = parseCapShape (idxHelper attributes 5)
        stalkShapeVal = parseStalkShape (idxHelper attributes 6)
        gillSizeVal = parseGillSize (idxHelper attributes 7)
        odorVal = parseOdor (idxHelper attributes 8)
        edibleVal = parseEdible (idxHelper attributes 1)
    return ( [StalkColor stalkColorVal, CapColor capColorVal, SporeColor sporeColorVal,
              CapShape capShapeVal, StalkShape stalkShapeVal, GillSize gillSizeVal,
              Odor odorVal]
           , edibleVal
           )



--helper function to assign attribute to the right parser

parseEdible :: String -> Edible
parseEdible "edible" = Nom
parseEdible "poison" = NoNom
parseEdible str = error "Invalid Edible Attribute"
{- parseEdibility :: String -> Edibility
parseEdibility "edible" = SafeToEat
parseEdibility "poison" = Poison
parseEdibility str =  error "Invalid Edibility Attribute"-}

parseColor :: String -> Color
parseColor "brown" = Brown
parseColor "green" = Green
parseColor "purple" = Purple
parseColor "white" = White
parseColor "yellow" = Yellow
parseColor str = error "Invalid Color Attribute" -- for incorrect inputs

parseCapShape :: String -> CapShape
parseCapShape "bell" = Bell
parseCapShape "conical" = Conical
parseCapShape "knobbed" = Knobbed
parseCapShape str = error "Invalid Cap Shape"

parseStalkShape :: String -> StalkShape
parseStalkShape "bulbous" = Bulbous
parseStalkShape "club" = Club
parseStalkShape "missing" = Missing
parseStalkShape str = error "Invalid Stalk Shape"

parseGillSize :: String -> GillSize
parseGillSize "broad" = Broad
parseGillSize "narrow" = Narrow
parseGillSize str = error "Invalid Gill Size"

parseOdor :: String -> Odor
parseOdor "almond" = Almond
parseOdor "anise" = Anise
parseOdor "foul" = Foul
parseOdor "none" = None
parseOdor "musty" = Musty
parseOdor str = error "Invalid Odor Attribute"


-- readObservationFile takes the entire contents of an entire file and return the list of
-- observations. Note the first line has header information and is not a valid observation. 
-- The lines function may be helpful. 
-- The sequence function may be helpful.
readObservationFile :: String ->  Maybe [Observation]
readObservationFile fileContent =
    let allLines = lines fileContent
        obseravationLines  = tail allLines --remove wrong header
        observations = map readObservation obseravationLines
        in sequence observations

--
-- --                                       Milestone Part Two
--

--numCorrect computes how much information about edibility can be gained by checking a specific
--attribute. It takes a single attribute and a list of observations, and answers the question: 
--"If all we had to go on was this attribute, how many mushrooms would we label correctly?"
--1) Split the observations into the those with the attribute, and those without the attribute. 
--2) One of these sets will have a higher percentage of edible mushrooms. Call that set A, and the
--   other set B. Note that A may be the set with the attribute, or the set without the attribute.
--3) If mushrooms in set A are assumed to be edible, and those in the other set B are assumed to be
--   inedible, return the number of correct guesses.
--4) Important: if either set is empty, no information is gained by looking at the attribute. Return 0.
--
--You may find the built-in partition function useful.
--
-- This was done in the in-class activity! Refer to that for assistance.
-- restore if necessary 
    {-numCorrect :: Attribute -> [Observation] -> Int
numCorrect attrib obsList = 
    let (withAttrib, withoutAttrib) = partition (\(mushroom, _) -> attrib `elem` mushroom) obsList
        numWithAttrib = length withAttrib
        numWithoutAttrib = length withoutAttrib
        numEdibleWithAttrib = length(filter(\(_,edible)-> edible == Nom) withAttrib)
        numEdibleWithoutAttrib = length(filter(\(_,edible)-> edible == NoNom) withoutAttrib)
        in
            if null withAttrib || null withoutAttrib
                then 0
                else
                    numEdibleWithAttrib + numEdibleWithoutAttrib-}
                    --max instead of added gave me 30, added gave 38

{-numCorrect :: Attribute -> [Observation] -> Int
numCorrect attribType obsList =
    let (withAttrib, withoutAttrib) = partition (\(mushroom, _) -> attribType `elem` mushroom) obsList
        numWithAttrib = length withAttrib
        numWithoutAttrib = length withoutAttrib
        numEdibleWithAttrib = length (filter (\(_, edible) -> edible == Nom) withAttrib)
        numEdibleWithoutAttrib = length (filter (\(_, edible) -> edible == Nom) withoutAttrib)
    in
        if null withAttrib || null withoutAttrib
            then 0
            else
                max (numEdibleWithAttrib + numWithoutAttrib - numEdibleWithoutAttrib)
                    (numEdibleWithoutAttrib + numWithAttrib - numEdibleWithAttrib)
-}
--this gave the 62 

{- numCorrect :: Attribute -> [Observation] -> Int
numCorrect attrib obsList =
    let (withAttrib, withoutAttrib) = partition (\(mushroom, _) -> attrib `elem` mushroom) obsList
        numWithAttrib = length withAttrib
        numWithoutAttrib = length withoutAttrib
        numActuallyEdible = length (filter (\(_, edible) -> edible == Nom) withAttrib)
        numActuallyInedible = length (filter (\(_, edible) -> edible == NoNom) withoutAttrib)
        percentEdible = fromIntegral numActuallyEdible / fromIntegral numWithAttrib
        percentInedible = fromIntegral numActuallyInedible / fromIntegral numWithoutAttrib
    in
        if null withAttrib || null withoutAttrib
            then 0
            else
                 numActuallyEdible + numActuallyInedible
-}
numCorrect :: Attribute -> [Observation] -> Int
numCorrect attrib obsList =
    let (withAttrib,withoutAttrib) = partition (\(mushroom, _) -> attrib `elem` mushroom) obsList
        numWithAttrib = length withAttrib
        numWithoutAttrib = length withoutAttrib
        numActuallyEdibleWith = length (filter (\(_, edible) -> edible == Nom) withAttrib)
        numActuallyInedibleWith = length (filter (\(_, edible) -> edible == NoNom) withAttrib)
        numActuallyEdibleWithout = length (filter (\(_, edible) -> edible == Nom) withoutAttrib)
        numActuallyInedibleWithout = length (filter (\(_, edible) -> edible == NoNom) withoutAttrib)
        percentEdibleWith = fromIntegral numActuallyEdibleWith / fromIntegral numWithAttrib
        percentEdibleWithout = fromIntegral numActuallyEdibleWithout / fromIntegral numWithoutAttrib
        percentInedibleWith = fromIntegral numActuallyInedibleWith / fromIntegral numWithAttrib
        percentInedibleWithout = fromIntegral numActuallyInedibleWithout / fromIntegral numWithoutAttrib

        a = if percentEdibleWith >= percentEdibleWithout then numActuallyEdibleWith else numActuallyEdibleWithout
        b = if percentInedibleWith >= percentInedibleWithout then numActuallyInedibleWith else numActuallyInedibleWithout
        in 
            if null withAttrib || null withoutAttrib
            then 0
            else a+b

    -- A decision tree is a binary tree that stores the likelihood of a mushroom being edible based on
-- its attributes.  Decision nodes are labeled with an attribute and have two children, with the
-- left child applying to mushrooms with that attribute, and the right child applying to mushrooms
-- without that attribute.  End nodes are leaves, and  should store enough information to compute
-- the percent chance of a mushroom being edible.  Do not store lists of observations or mushrooms.
-- Doubles are likely not precise enough, but Rationals or tuples (or even triples) of Integers will
-- be sufficient.

-- Define an algebraic data type for decision trees.
data DTree = DecisionNode Attribute DTree DTree
            | Leaf Rational
            deriving Show

-- Given a list of attributes and a list of observations, build a decision tree.
--  * If all the observations have the same edibility, you can safely make an end node: there is no
--    need to further analyze a mushroom.  
--  * If all the observations have the same attributes, you must make an end node : there is no way
--    to futher analyze a mushroom.
--  * Otherwise, go through the list of attributes and find the one that gives you the most
--    information about edibility, as measured by the number of correct guesses that can be obtained if
--    this was the only attribute used.  Then create a decision node using that attribute as a pivot.
--  * For efficiency, you can delete the pivot from the list of attributes, but it doesn't really
--    matter.
--  * You should create helper functions for this problem. 

--Restore if necessary
{- buildTree :: [Attribute] -> [Observation] -> DTree
buildTree attribs [] = error "Empty Observations List"
--buildTree attribs [(mushroom,edible)] =  Leaf (if edible == Nom then 1 else 0)
buildTree attribs obsList 
    |allSameEdibility obsList = Leaf (probabilityCalculator obsList)
     -- allSameAttributes obsList = Leaf (probabilityCalculator obsList)
    --otherwise = 
    |otherwise =    let bestAttribute = findBestAttribute obsList
                        (withAttrib, withoutAttrib) = partition (\(mushroom, _) -> bestAttribute `elem` mushroom) obsList
                        leftTree = buildTree attribs withAttrib
                        rightTree = buildTree attribs withoutAttrib
                     in DecisionNode bestAttribute leftTree rightTree
-}
-- Works ok 

buildTree :: [Attribute] -> [Observation] ->DTree
--buildTree attribs [] = error "AAAAAHH"
buildTree attribs [(mushroom, edible)] = Leaf (if edible == Nom then 1 else 0)
buildTree attribs obsList
    | allSameEdibility obsList = Leaf (probabilityCalculator obsList)
    | allSameAttributes obsList = Leaf (probabilityCalculator obsList)
    | otherwise = let   attrsWithZeroFilteredOut = [ attr | attr <- attribs, numCorrect attr obsList /= 0 ]
                        bestAttr = bestAttribute attrsWithZeroFilteredOut obsList
                        (withAttrib, withoutAttrib) = partition (\(mushroom, _) -> bestAttr `elem` mushroom) obsList
                 in DecisionNode bestAttr (buildTree attrsWithZeroFilteredOut withAttrib) (buildTree attrsWithZeroFilteredOut withoutAttrib)
{-
buildTree ::[Attribute] -> [Observation] ->DTree
buildTree attrs obs = buildTreefr attrs obs (denom obs)

buildTreefr :: [Attribute] -> [Observation] -> Rational -> DTree
buildTreefr _ [] total = error "AAH"  -- No observations
buildTreefr _ obs total | all ((==) (snd (head obs)) . snd) obs = 
    let numEdible = fromIntegral (length (filter (\(_, label) -> label == Nom) obs))
        in Leaf (numEdible / denom obs) -- All observations have the same edibility -- All observations have the same edibility
--Total needs to be (fromIntegral (length obs))
buildTreefr [] obs total = Leaf (fromIntegral (length (filter (\(_, ed) -> ed == Nom) obs)) / fromIntegral (length obs)) -- No more attributes to split on
buildTreefr attrs obs total =
    --let bestAttribute =  maximumBy(compare `on` (`numCorrect` obs)) attrs
    let bestAttr = bestAttribute attrs obs
        (withAttrib, withoutAttrib) = partition (\(mushroom, _) -> bestAttr `elem` mushroom) obs
        numWithAttrib = fromIntegral (length withAttrib)
        numWithoutAttrib = fromIntegral (length withoutAttrib)
        in DecisionNode bestAttr (buildTreefr attrs withAttrib (numWithAttrib)) (buildTreefr attrs withoutAttrib (numWithoutAttrib))
-}
bestAttribute :: [Attribute] -> [Observation] -> Attribute
bestAttribute attrs obsList =
    fst $ maximumBy (compare `on` snd) [ (attr, numCorrect attr obsList) | attr <- attrs ]
denom :: [Observation] -> Rational
denom obs = fromIntegral (length obs)
--RESTORE
{- 
buildTree :: [Attribute] -> [Observation] -> DTree
buildTree _ [] = error "Empty Observation list"
buildTree attributes observations
    | allSameEdibility observations = Leaf (computeEdibilityPercentage observations)
    | allSameAttributes observations = Leaf (computeEdibilityPercentage observations)
    | otherwise =
        let bestAttribute = findBestAttribute attributes observations
            (withBestAttribute, withoutBestAttribute) = partition (\(mushroom, _) -> bestAttribute `elem` mushroom) observations
            leftTree = buildTree (filter (/= bestAttribute) attributes) withBestAttribute
            rightTree = buildTree (filter (/= bestAttribute) attributes) withoutBestAttribute
        in DecisionNode bestAttribute leftTree rightTree
-}

-- Helper function to check if all observations have the same edibility
--RESTORE
allSameEdibility :: [Observation] -> Bool
allSameEdibility [] = True
allSameEdibility ((_, edibility):obsList) = all (\(_, ed) -> ed == edibility) obsList

-- Helper function to check if all observations have the same attributes
allSameAttributes :: [Observation] -> Bool
allSameAttributes [] = True
allSameAttributes ((attrs, _):obsList) = all (\(otherAttrs, _) -> attrs == otherAttrs) obsList

-- Helper function to compute the percentage of edible mushrooms in observations
{-computeEdibilityPercentage :: [Observation] -> Rational
computeEdibilityPercentage observations =
    let numEdible = length (filter (\(_, edibility) -> edibility == Nom) observations)
        totalMushrooms = length observations
    in fromIntegral numEdible / fromIntegral totalMushrooms
-}
--Restore if necessary
{- findBestAttribute :: [Observation] -> Attribute
findBestAttribute obsList = 
    let bestAttribute = foldr (\attr acc -> if numCorrect attr obsList > numCorrect acc obsList then attr else acc) (head allAttributes) (tail allAttributes)
    in bestAttribute
-}
--currently unused
{- findBestAttribute :: [Attribute] -> [Observation] -> Attribute
findBestAttribute attributes obsList =
    let (bestAttrib, _) = foldl (\(currentBest, currentMax) attrib ->
            let correctGuesses = numCorrect attrib obsList
            in if correctGuesses > currentMax then (attrib, correctGuesses) else (currentBest, currentMax)
            ) (head attributes, 0) attributes
    in bestAttrib
-}
--CHAT GPT aided helper functions
-- Helper function to check if all observations have the same edibility
{-allSameEdibility :: [Observation] -> Bool
allSameEdibility [] = True -- Empty list, all observations have the same edibility by default
allSameEdibility [(mushroom, _)] = True -- Single observation, same edibility
allSameEdibility ((_, edibility):rest) = all (\(_, ed) -> ed == edibility) rest
-}
-- Helper function to check if all observations have the same attributes
{- allSameAttributes :: [Observation] -> Bool
allSameAttributes [] = True -- Empty list, all observations have the same attributes by default
allSameAttributes [(mushroom, _)] = True -- Single observation, same attributes
allSameAttributes ((mushroom, _):rest) = all (\(otherMushroom, _) -> nub mushroom == nub otherMushroom) rest
-}


probabilityCalculator :: [Observation] -> Rational
probabilityCalculator obsList =
    let numEdible = length (filter (\(_,edible)-> edible == Nom) obsList)
        totalObs = length obsList
        in fromIntegral numEdible `outOf` fromIntegral totalObs

--
----                                     Core Project
--

-- rateMushroom takes a mushroom, a decision tree, and a safety limit, and returns a string
-- describing if we can eat the mushroom.  Follow the decision tree for this mushroom, and check if
-- the corresponding end node estimates the chance of edibility to be higher than the safety limit.
-- If it is greater than the safety limit, return the string "Eat the mushroom" 
-- If it is less than or equal to the safety limit, return the string "Do not eat the mushroom"
-- For full credit, append the estimate to the string similar to the below:
--   "Eat the mushroom: estimate a 95% chance of being edible."
--   "Do not eat the mushroom: estimate a 40% chance of being poisonous."
-- The ``precentOfRatio`` and ``percentOfCount`` functions may be helful.

-- Helper function to calculate the percentage from a ratio

formatResult :: Bool -> Rational -> String
formatResult isEdible estimate =
    let percentage = percentFromRatio estimate
        poisonpercentage = (100 - percentFromRatio estimate)
        action = (if isEdible then "Eat the mushroom" else "Do not eat the mushroom") ++ (if isEdible then ": estimate a " ++ show percentage ++" % change of being edible" else ": estimate a " ++ show poisonpercentage ++"% change of being poisonous")
        in action

percentFromRatio :: Rational -> Double
percentFromRatio ratio = fromRational ratio * 100


--was my original 

rateMushroom :: Mushroom -> DTree -> Rational -> String
rateMushroom [] (Leaf estimate) safetyLimit =
    let isEdible = estimate > safetyLimit
        in formatResult isEdible estimate

rateMushroom [] (DecisionNode _ _ rightTree) safetyLimit =
    rateMushroom [] rightTree safetyLimit

rateMushroom (attr:restAttrs) (DecisionNode decisionAttr leftTree rightTree) safetyLimit =
    if attr == decisionAttr
        then rateMushroom restAttrs leftTree safetyLimit
        else rateMushroom restAttrs rightTree safetyLimit
        
rateMushroom _ _ _ = "Invalid mushroom or decision tree"


{-rateMushroom :: Mushroom -> DTree -> Rational -> String
rateMushroom [] (DecisionNode _ _ rightTree) safetyLimit =
    rateMushroom [] rightTree safetyLimit

rateMushroom (attr:restAttrs) (DecisionNode decisionAttr leftTree rightTree) safetyLimit =
    if attr == decisionAttr
        then rateMushroom restAttrs leftTree safetyLimit
        else rateMushroom restAttrs rightTree safetyLimit

rateMushroom [] (Leaf estimate) safetyLimit =
    formatResult (estimate > safetyLimit) estimate

--rateMushroom _ _ _ = "Invalid mushroom or decision tree"
-}
-- buildGuide takes a decision tree, a safety limit, and return an itemized guide. 
-- Each line is numbered separately and should have one of two forms.
--  "n: Eat the mushroom." / "n: Do not eat the mushroom."
--  "n: If the mushroom has (attribute) go to step x, otherwise go to step y."
-- For this implementation, every node in the tree will have a separate line.
-- You will need helper functions.
-- This results in a mess ......
{-
buildSubGuide :: DTree -> Int -> Rational -> (Int, [String])
buildSubGuide (Leaf estimate) step safetyLimit =
    (step + 1, [show step ++ ": " ++ formatResult (estimate > safetyLimit) estimate])

buildSubGuide (DecisionNode attribute leftTree rightTree) step safetyLimit =
    let (leftStep, leftGuide) = buildSubGuide leftTree (step + 1) safetyLimit
        (rightStep, rightGuide) = buildSubGuide rightTree (leftStep + 1 + length leftGuide) safetyLimit
    in
        (rightStep, [show step ++ ": If the mushroom has " ++ showAttribute attribute ++ " go to step " ++ show (step + 1) ++ ", otherwise go to step " ++ show (leftStep + 1)]
        ++ leftGuide
        ++ rightGuide)


buildGuide :: DTree -> Rational -> [String]
buildGuide tree safetyLimit =
    snd $ buildSubGuide tree 1 safetyLimit
-}
--CLOSEST ONE YET

buildSubGuide :: DTree -> Int -> Rational -> (Int, [String])
buildSubGuide (Leaf estimate) step safetyLimit =
    (step, [show step ++ ": " ++ formatResult (estimate > safetyLimit) estimate])

buildSubGuide (DecisionNode attribute leftTree rightTree) step safetyLimit =
    let (leftStep, leftGuide) = buildSubGuide leftTree (step + 1) safetyLimit
        (rightStep, rightGuide) = buildSubGuide rightTree (leftStep + 1) safetyLimit
    in
        (rightStep, [show step ++ ": If the mushroom has " ++ showAttribute attribute ++ " go to step " ++ show (step + 1) ++ ", otherwise go to step " ++ show (leftStep + 1)]
        ++ leftGuide
        ++ rightGuide)

buildGuide :: DTree -> Rational -> [String]
buildGuide tree safetyLimit =
    snd $ buildSubGuide tree 1 safetyLimit

--Returns identical results
{-
buildSubGuide :: DTree -> Int -> Rational -> (Int, [String])
buildSubGuide (Leaf estimate) step safetyLimit =
    (step, [show step ++ ": " ++ formatResult (estimate > safetyLimit) estimate])

buildSubGuide (DecisionNode attribute leftTree rightTree) step safetyLimit =
    let (leftStep, leftGuide) = buildSubGuide leftTree (step + 1) safetyLimit
        (rightStep, rightGuide) = buildSubGuide rightTree (leftStep + 1) safetyLimit
        currentGuideLine = show step ++ ": If the mushroom has " ++ showAttribute attribute ++ " go to step " ++ show (step + 1) ++ ", otherwise go to step " ++ show (leftStep + 1)
    in
        (rightStep, currentGuideLine : leftGuide ++ rightGuide)
buildGuide :: DTree -> Rational -> [String]
buildGuide tree safetyLimit =
    snd $ buildSubGuide tree 1 safetyLimit
-}

{-
buildGuide :: DTree -> Rational -> [String]
buildGuide tree safetyLimit = buildSubGuide tree 1 safetyLimit

buildSubGuide :: DTree -> Int -> Rational -> [String]
buildSubGuide (Leaf estimate) step safetyLimit = 
    [show step ++ ": " ++ formatResult (estimate > safetyLimit) estimate]

buildSubGuide (DecisionNode attribute leftTree rightTree) step safetyLimit =
    [show step ++ ": If the mushroom has " ++ showAttribute attribute ++ " go to step " ++ show (step + 1) ++ ", otherwise go to step " ++ show (step + 2)]
    ++ buildSubGuide leftTree (step + 1) safetyLimit
    ++ buildSubGuide rightTree (step + 2) safetyLimit
-}
--                                     Full Credit
--

-- For the first full credit, improve on the derived Show instance for attributes.  Make a custom
-- instance of Show that returns a proper English clause describing the attribute. For instance, "a
-- club stalk", "narrow gills", or "an almond odor." 
showAttribute :: Attribute -> String
--showAttribute (Edible ed) = if ed == Nom then "edible" else "inedible"
showAttribute (StalkColor color) = "a " ++ show color ++ " stalk color "
showAttribute (CapColor color) = "a " ++ show color ++  " cap color "
showAttribute (SporeColor color) = "a " ++ show color ++ " spore color "
showAttribute (CapShape shape) = "a " ++ show shape ++ " cap shape "
showAttribute (StalkShape shape) = if shape == Missing then "a " ++ show shape ++ " stalk" else "a " ++ show shape ++ " stalk shape"
showAttribute (GillSize size) = "a " ++ show size ++ " gill size "
showAttribute (Odor odor) =
                            let a = "a "
                                b = "an "
                                e = ""
                                c = "no odor"
                                d = show odor ++ " odor"
                            in (if odor == None then e else (if odor == Anise || odor == Almond then b else a)) ++ (if odor == None then c else d)


-- For the second full credit portion, you will eliminate redundancies in the guide. This will be
-- done using common subexpression elimination. We will keep an index mapping strings to integer
-- locations. Since indexes are useful for other types, we will write generic functions.

-- makeEntry adds an element to the index. It returns the location of the element in 
-- the index and, if necessary, an updated index.
-- If the element is already in the index, you should not add it again. 
-- If it does not occur in the index, find the next largest location and associate that element with
-- that location.
-- Index locations should start at 1.
type Index a = [(a, Int)]

makeEntry :: Eq a => a -> Index a -> (Int, Index a)
makeEntry element index =
    case lookup element index of
        Just location -> (location, index)
        Nothing -> let newLocation = 1 + maximum (0 : map snd index)
                   in (newLocation, (element, newLocation) : index)



-- For instance: makeEntry 'x' [('a',1),('x',2),('b',3)] = (2, [('a',1),('x',2),('b',3)])
-- For instance: makeEntry 'y' [('a',1),('x',2),('b',3)] = (4, [('a',1),('x',2),('b',3),('y',4)])
-- The order of the entries in the index does not matter, and will quite likely be reversed.

-- Once makeEntry is working, make a version of buildGuide (buildGuideCSE) that passes around an index of
-- strings.  When you want to add a string to the index, use makeEntry to avoid creating duplicates.
-- As a natural consequence of this, you will return an "upside-down" guide: the entry point will
-- have the largest location.

buildGuideCSE :: DTree -> Rational -> [String]
buildGuideCSE = undefined

-- For extra credit, change indexes from association lists to association binary search trees.
