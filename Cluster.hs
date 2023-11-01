module Cluster where
import Data.List
import Data.Maybe
import Debug.Trace
import Data.Function
import Data.Functor.Contravariant.Divisible (Divisible(divide))
import Test.Grader.Timing (BlowupReport(n))

--Points are an x coordinate, a y coordinate, and an integer label.
type Point = (Double, Double, Int)
type Center = Point
--A cluster is a center and a list of points.
type Cluster = (Center, [Point])

-- All undefined values and functions should be completed. Your code will compile and test 
-- -- (with the -- test flag) even if some functions are left undefined.
--
-- --                                       Milestone
--
--tenPoints = [(7,1,100), (7,3,200), (8,1,300), (8,2,400), (7.5,3.5,500), (2,7,600), (1,7,700), (3,7,800), (2,8,900), (2,6,1000)]
--testClusters = assignPoints trafficDist [(1,1,-1),(2,2,-1)] tenPoints 
--Given a list of elements and an integer k, return k evenly spaced elements of the list.
--As a first attempt, simply take the first k elements. Fix this after you finish the rest of the
--project.
getKElems :: Int -> [a] -> [a]
getKElems k lst  
    |k <= 1 = take k lst
    |otherwise = let n = length lst
                     indices  = [i * (n - 1) `div` (k - 1) | i <- [0 .. k - 1]]
                     tmp = zip lst [0..]
                in [ x | (x,i) <-  tmp, i `elem` indices]





{- getKElems k lst =
        let n = length lst
            a = max 1 n `div` (k-1) + 1
            b = max 1 n `div` (k-1)
            spacing = if k*a >= n && n `mod` a < n `mod` b then a else b
            indices = take (k-1) ([0,spacing..n-1]) ++ [n-1]
            lst2 = zip lst [0..]
            lst3 = [ e | (e, i) <- lst2, i `elem` indices] in
            lst3
         --My solution, missing final reweight -}

        {-
         ni = length indices
        v = last indices - head (tail (reverse indices))
        q = abs (head indices - head (tail indices))
        w = v-q
        rwFactor = fromIntegral w / fromIntegral ni

        addedRWFactorSpacing = floor(1/rwFactor)
        
        lst4 = zip lst3 [0..]
        in
            if w <= 0
                then lst3
                else [e | (e,i) <- lst4, i `mod` addedRWFactorSpacing /= 0]
        --[ e + (fromIntegral (rwFactor*i) / fromIntegral i) | (e,i)<-lst4]
-}

--WORK IN PROGRESS, RETURNING LST3 FOR NOW
        --check if getKElems 5 [0..18] should be [0,5,10,15,18] or [0,4,8,12,18]
            --all should be within one of each other


--VERY CLOSE fix spacing

 {-   
    let n = length lst
        numElems =  n `div` k + 1
        kSpacedIndicess = [ x| x <- (map (*k) [0..(numElems)])]
        indexedList = [(x,i)|x <- lst, i <- kSpacedIndices] in
        [x | (x,i)<- indexedList]
--Example: getKElems 3 [1..6] = [1,3,6]
-}

--Return the Euclidean distance between two points. You may work in squared distances if you
--prefer.
eucDist :: Point -> Point -> Double
eucDist (x1,y1,l1) (x2,y2,l2) = sqrt ((y2-y1)^2+(x2-x1)^2)
--Example: eucDist (0,0,10) (1,1,10) < eucDist (0,0,10) (0,2,10)

--Return the Manhattan distance between two points: the distance between the points based on
--strictly horizontal and vertical movement, as if on a city grid.
manhatDist :: Point -> Point -> Double
manhatDist (x1,y1,l1) (x2,y2,l2) = abs (y2-y1) + abs (x2-x1)
--Example: manhatDist (0,0,10) (1,1,10) == manhatDist (0,0,10) (0,2, 10)

--Return the Chebyshev distance between two points: the maximum between the x-distance and the
--y-distance, as if diagonal movement was free, like for a King in chess.
chebyDist :: Point -> Point -> Double
chebyDist (x1,y1,l1) (x2,y2,l2) =
    let ydist = abs (y2-y1)
        xdist = abs (x2-x1) in
        max ydist xdist
--Example: chebyDist (0,0,10) (0,5,10) == chebyDist (0,0,10) (5,5,10)

--Return the traffic-aware Manhattan distance: count horizontal distances twice as much as vertical.
trafficDist :: Point -> Point -> Double
trafficDist (x1,y1,l1) (x2,y2,l2) =
    let xdistadjusted = 2 * abs (x2-x1)
        ydist = abs (y2-y1) in
        xdistadjusted + ydist
--Example: trafficDist (0,0,10) (0,10,10) == trafficDist (0,0,10) (5,0,10)

--Return the township-aware Manhattan distance. The label of the point is taken to be the township
--the point is in.  A penalty factor of 2 is applied to the distance between points in different
--townships.
townshipDist :: Point -> Point -> Double
townshipDist (x1,y1,l1) (x2,y2,l2) =
    let  p1 = (x1,y1,l1)
         p2 = (x2,y2,l2) in
            if l1 == l2 then manhatDist p1 p2 else 2 * manhatDist p1 p2
--Example: townshipDist (0,0,10) (1,1,20) == 2*townshipDist (0,0,10) (1,1,10) 

--Given a list of doubles, compute their average. You may need fromIntegral.
average :: [Double] -> Double
average lst =
    let total = sum lst
        numberElem = length lst in
        total / fromIntegral numberElem

--Example:  average [0,5,10] = 5.0

--Given a ranking function and a list of elements, return the element with the minimum rank.
minimize :: (a -> Double) -> [a] -> a
minimize f lst = foldr1 (\x y -> if f x < f y then x else y) lst
--Example: minimize (fromIntegral . length) ["aaaa", "b"] = "b"

--Given a bucketing function, a list of buckets, and a list of items to be placed in buckets, 
--and returns the list of bucket, items-in-buckets pairs.
--Go take your old buildCorpus function, copy it, and turn it into a HOF (higher-order function).
-- Bucket is a HOF, because it takes a function as input. You can write it using recursion, other
-- HOFs, or list comprehensions as you choose.
bucket :: Eq b => (a -> b) -> [b] -> [a] -> [(b,[a])]
bucket f [] lst = []
bucket f buckets [] = [(bucket, []) |bucket <- buckets]
bucket f (bucketHead:bucketTail) lst =
    (bucketHead,filter (\x ->  f x == bucketHead) lst) : bucket f bucketTail lst
--Example:  bucket length [1..3] ["Hi","my","job","is","fun","!"]
--[(1,["!"]),(2,["Hi","my","is"]),(3,["job","fun"])]
--
--buildCorpus :: [(PixelImage, Digit)] -> Corpus
--buildCorpus imgLbls = [(d, digitIdent d imgLbls ) |d <- allDigits, digitIdent d imgLbls /= []]

--Full project!

--Given a metric, a list of centers, and a point, return the center closest to the point.
--Hint: you've already written a higher-order function to do this.
assignPoint :: (Point -> Center -> Double) -> [Center] -> Point -> Center
assignPoint metric lstCenters pt1 =
    minimize (\center -> metric pt1 center) lstCenters
--assignPoint metric lstcent pt = minimize (metric
--Examples: assignPoint eucDist  [(0,0,-1),(5,7,-1)] (5,0,100) = (0.0,0.0,-1)
--          assignPoint trafficDist  [(0,0,-1),(5,7,-1)] (5,0,100) = (5.0,7.0,-1)

--Given a metric, a list of centers, and a list of point, return the clusters, where each point is
--assigned to the nearest center.
--Hint: you've already written a higher-order function to do this.
assignPoints :: (Point -> Center -> Double) -> [Center] -> [Point] -> [Cluster]
assignPoints metric lstCenters lstPoints  =
        bucket (\p -> assignPoint metric lstCenters p) lstCenters lstPoints
        --[(center, filter(\pt -> assignPoint metric lstCenters pt == center) lstPoints)| center <- lstCenters]


        
--Examples:
--let testClusters = assignPoints trafficDist [(1,1,-1),(2,2,-1)] tenPoints 
--
--[(c, length ps) | (c,ps) <- testClusters]
--[((1.0,1.0,-1),1),((2.0,2.0,-1),9)]
--
--testClusters
--[((1.0,1.0,-1),[(1.0,7.0,700)]),
-- ((2.0,2.0,-1),[(7.0,1.0,100),(7.0,3.0,200),(8.0,1.0,300),(8.0,2.0,400),(7.5,3.5,500),
--                (2.0,7.0,600),(3.0,7.0,800),(2.0,8.0,900),(2.0,6.0,1000)])]


--Given a metric and a cluster, return the mean of the points in the cluster.
--The existing center is NOT a point, and should be ignored.
--The label should be the label of the closest point to the new center. 
--Since you can't take the mean of an empty list of points, return Nothing in that case.
findMean :: (Point -> Center -> Double) -> Cluster -> Maybe Center
findMean metric (cent,lstpts) 
        | null lstpts = Nothing
        | otherwise =   let numPoints = fromIntegral (length lstpts)
                            maxLabel = maximum [l | (x,y,l)<- lstpts]
                            xMean = sum( [ x| (x,y,l) <-lstpts])/numPoints
                            yMean = sum( [ y| (x,y,l) <-lstpts])/numPoints
                            closestPoint = assignPoint metric lstpts (xMean,yMean, maxLabel + 1)
                            closestLabel = getLabel closestPoint  in
                            Just (xMean,yMean, closestLabel)
        where 
            getLabel::Point -> Int
            getLabel (x1,y1,label) = label



--Example: findMean eucDist ((3,3,0), [(0,0,0), (10,10,0), (2,2,1)]) = Just (4.0,4.0,1)

--Given a metric and a list of clusters, relocate all the centers to the mean of their clusters. Be
--sure to return only the valid centers. If any cluster is empty, simply remove it.
moveCenters :: (Point -> Center -> Double) -> [Cluster] -> [Center]
moveCenters metric clusters = catMaybes [findMean metric (cent,lstpts) | (cent,lstpts) <- clusters] -- catMaybes removes invalid centers (the nothing case)

--Example:  moveCenters trafficDist testClusters  = [(1.0,7.0,700),(5.166666666666667,4.277777777777778,200)]

--Given a metric, k, and a list of clusters, first move the centers, and then reassign the points
--to the new centers.
--Note that clusters can become empty and disappear. For full credit, replace missing clusters as
--described on the website.

{-reassignPoints :: (Point -> Center -> Double) -> [Point] -> [Center] -> [(Center,Point)]
reassignPoints metric points centers =
    [(closestCenter, pt) | pt <- points, let closestCenter = assignPoint metric centers pt]
-}
improveClusters :: (Point -> Center -> Double) -> Int -> [Cluster] -> [Cluster]
improveClusters metric k clusters =
    let movedCenters = moveCenters metric clusters
        pointsList = concat [snd x| x <- clusters]
        adjustedCenters = if length movedCenters == k then movedCenters else splitClusters k movedCenters clusters
        --newCenters = if then movedCenters else 
        --reassignedClusters = reassignPoints metric (concat [pts | (_, pts) <- clusters]) movedCenters
    in assignPoints metric adjustedCenters pointsList
    {- let movedCenters = moveCenters metric clusters
        newClusters = [(center, []) | center <- movedCenters]
        reassignClusters = foldr (\pt acc -> (assignPoint metric movedCenters pt, pt):acc) newClusters
    in reassignClusters (concat [points | (_, points) <- clusters])-}
{-reassignPoints :: (Point -> Center -> Double) -> [Point] -> [Center] -> [Point]
reassignPoints metric points centers =
    [pt | pt <- points, let closestCenter = assignPoint metric centers pt in closestCenter `elem` centers]
-}
    {-
    let movedCenters = moveCenters metric clusters
        newClusters = [(center,[])| center <- movedCenters]
        reassignClusters = foldr (\pt acc ->(assignPoint metric movedCenters pt, pt):acc) newClusters
        in reassignClusters (concat [points | (_, points) <- clusters])  -- Reassign points to the new clusters
-}
--Discussed with Caleb, Chat GPT aided helper function
splitClusters :: Int -> [Center] -> [Cluster] -> [Center]
splitClusters k center clusters = 
    let extractedPoints = [snd x | x <- clusters]
        newClustPt = maximumBy(compare `on` length) extractedPoints
        newCenterPt = fst ( head (filter(\(x,y) -> y == newClustPt) clusters))
        tmp = [ x | x <- newClustPt, x /= newCenterPt]
    in  if length tmp > 1 then center ++ [head tmp] else center

    

--Example: let newClusters = improveClusters trafficDist 2 testClusters 
--[(c, length ps) | (c,ps) <- newClusters]
--[((1.0,7.0,700),5),((5.166666666666667,4.277777777777778,200),5)]

--iterationLimit should be used by kMeans to limit how many times improveClusters can be called.
--Remember variables are not mutable.
iterationLimit = 100
--Given a metric, k, and a list of points, create the initial clusters and repeatedly 
--improve them, until they stop changing.
kMeans :: (Point -> Center -> Double) -> Int -> [Point] -> [Cluster]
kMeans metric k points = 
    let initial = assignPoints metric (getKElems k points) points
        n = iterationLimit
        in improver metric k initial n
    where improver:: (Point -> Center -> Double) -> Int -> [Cluster] -> Int -> [Cluster]
          improver metric k clusters 0 = clusters
          improver metric k clusters n = improver metric k (improveClusters metric k clusters) (n-1)
