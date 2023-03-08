type Cell = (Int,Int)

getX :: Cell -> Int
getX (x , y) = x

getY :: Cell -> Int
getY (x , y) = y

data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)


up :: MyState -> MyState
up Null = Null
up (S (x,y) cell string myState) =if x == 0 then Null else S (x - 1 , y) cell "up" (S (x,y) cell string myState)


down :: MyState -> MyState
down Null = Null
down (S (x,y) cell string myState) = if x == 3 then Null else S (x+1,y) cell "down" (S (x,y) cell string myState)


left :: MyState -> MyState
left Null = Null
left (S (x,y) cell string myState) =if y == 0 then Null else S (x,y-1) cell "left" (S (x,y) cell string myState)


right :: MyState -> MyState
right Null = Null
right (S (x,y) cell string myState) = if y == 3 then Null else S (x,y+1) cell "right" (S (x,y) cell string myState)


collect :: MyState -> MyState

collect (S (x, y) (h : t) string myState)
  | x == getX h && y == getY h = S (x , y) t "collect" (S (x,y) (h : t) string myState)
  | t /= [] = collect (S (x,y) t string myState)
  | otherwise = Null

nextMyStates :: MyState -> [MyState]
nextMyStates (S (x,y) cell string myState) = helpercollect (S (x,y) cell string myState) ++ helperRight (S (x,y) cell string myState) ++ helperLeft (S (x,y) cell string myState) ++ helperup (S (x,y) cell string myState) ++ helperdown (S (x,y) cell string myState)

helperRight (S (x,y) cell string myState) = [right (S (x,y) cell string myState) | right (S (x,y) cell string myState) /= Null]
helperLeft (S (x,y) cell string myState) = [left (S (x,y) cell string myState) | left (S (x,y) cell string myState) /= Null]
helperup (S (x,y) cell string myState) = [up (S (x,y) cell string myState) | up (S (x,y) cell string myState) /= Null]
helperdown (S (x,y) cell string myState) = [down (S (x,y) cell string myState) | down (S (x,y) cell string myState) /= Null]
helpercollect (S (x,y) cell string myState) = [collect (S (x,y) cell string myState) | collect (S (x,y) cell string myState) /= Null]


isGoal :: MyState -> Bool
isGoal (S (x,y) cell string myState) | null cell = True
                                     | otherwise  = False


search :: [MyState] -> MyState
search [] = Null
search (h:t) = if isGoal h then h else search (t ++ nextMyStates h)


constructSolution:: MyState ->[String]

constructSolution (S (x,y) cell string myState) = if myState == Null then [] else constructSolution myState ++ [string]


solve :: Cell -> [Cell] -> [String]

solve (curx,cury) (h:t) = constructSolution (search (nextMyStates(S (curx,cury) (h:t) "" Null)))


