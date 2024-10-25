module LSystems ( LSystem(LSystem), ColouredLine, Command(..)
                , angle, axiom, rules, lookupChar
                , expandOne, expand, move, parse,unbracket, trace1, trace2
                , expandLSystem, commandMap ) where

import IC.Colour
import GHC.RTS.Flags (MiscFlags(installSignalHandlers))

type Rules a = [(Char, [a])]
data LSystem = LSystem Float [Char] (Rules Char)
type Vertex = (Float, Float)
type TurtleState = (Vertex, Float)
data Command = F | L | R | B [Command] deriving Show
type ColouredLine = (Vertex, Vertex, Colour)

--rulesPeanoGosper :: Rules Char
--rulesPeanoGosper = [('M', "M+N++N-M--MM-N+"), ('N', "M-N++N+M--M-N")] --for debug

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (LSystem ang _ _) = ang

-- Returns the axiom string for the given system.
axiom :: LSystem -> [Char]
axiom (LSystem _ axi _) = axi

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules Char
rules (LSystem _ _ rul) = rul

--
-- Pre: the character has a binding in the Rules list
--
lookupChar :: Rules a -> Char -> [a]
lookupChar rule c = case lookup c rule of
                      Just rep -> rep
                      Nothing -> []

--
-- Expand command string s once using rule table r
--
expandOne :: Rules Char -> [Char] -> [Char]
expandOne rules = concatMap (lookupChar rules)

--
-- Expand command string s n times using rule table r
--
expand :: [Char] -> Int -> Rules Char -> [Char]
expand str 0 _ = str
expand str n rule = expand (expandOne rule str) (n - 1) rule

-- Move a turtle.
--
-- F moves distance 1 in the current direction.
-- L rotates left according to the given angle.
-- R rotates right according to the given angle.
move :: Command -> Float -> TurtleState -> TurtleState
move L ang ((x, y), facing) = ((x, y), facing + ang)
move R ang ((x, y), facing) = ((x, y), facing - ang)
move F _ ((x, y), facing) = ((x + 1 * cos rad, y + 1 * sin rad), facing) where rad = degreesToRadians facing

degreesToRadians :: Float -> Float
degreesToRadians x = (x / 180) * pi

parse :: Rules Command -> [Char] -> [Command]
parse _ [] = []
parse rules ('[' : str) = let cmd = parse rules inside
                              rest = parse rules outside
                          in B cmd : rest
                            where (inside, outside) = unbracket 1 str
parse rules (c : cs) = lookupChar rules c ++ parse rules cs

unbracket :: Integer -> [Char] -> ([Char], [Char]) -- return the stuff inside [] and what is left at the right side of ]
unbracket _ [] = ([], [])
unbracket 1 (']' : str) = ([], str)
unbracket n (']' : str) = 
  let (inside, outside) = unbracket (n - 1) str
  in (']' : inside, outside) 
unbracket n ('[' : str) = 
  let (inside, outside) = unbracket (n + 1) str
  in ('[' : inside, outside)
unbracket n (c : str) = 
  let (inside, outside) = unbracket n str
  in (c : inside, outside)

trace1 :: [Command] -> Float -> Colour -> [ColouredLine]
trace1 cmds = tracehelper1 cmds ((0, 0), 90) []

tracehelper1 :: [Command] -> TurtleState -> [ColouredLine] -> Float -> Colour -> [ColouredLine]
tracehelper1 [] _ lines _ _ = lines
tracehelper1 (F : cs) ((x, y), facing) lines ang colour = let ((newx, newy), facing) = move F ang ((x, y), facing)
                                                              line = ((x, y), (newx, newy), colour)
                                                          in tracehelper1 cs ((newx, newy), facing) (line : lines) ang colour 
tracehelper1 (L : cs) ((x, y), facing) lines ang colour = let (_, newfacing) = move L ang ((x, y), facing)
                                                          in tracehelper1 cs ((x, y), newfacing) lines ang colour
tracehelper1 (R : cs) ((x, y), facing) lines ang colour = let (_, newfacing) = move R ang ((x, y), facing)
                                                          in tracehelper1 cs ((x, y), newfacing) lines ang colour
tracehelper1 (B cmds : cs) state lines ang colour = let insidelines = tracehelper1 cmds state [] ang colour
                                                         in tracehelper1 cs state (lines ++ insidelines) ang colour


-- This version uses an explicit stack of residual commands and turtle states
trace2 :: [Command] -> Float -> Colour -> [ColouredLine]
trace2 cmds = tracehelper2 cmds [] ((0, 0), 90) []

tracehelper2 :: [Command] -> [(TurtleState, [Command])] -> TurtleState -> [ColouredLine] -> Float -> Colour -> [ColouredLine]
tracehelper2 [] [] _ lines _ _ = lines
tracehelper2 [] ((state, cmds) : stack) ((x, y), facing) lines ang colour = tracehelper2 cmds stack state lines ang colour
tracehelper2 (F : cs) stack ((x, y), facing) lines ang colour = let ((newx, newy), facing) = move F ang ((x, y), facing)
                                                                    line = ((x, y), (newx, newy), colour)
                                                                in tracehelper2 cs stack ((newx, newy), facing) (line : lines) ang colour
tracehelper2 (L : cs) stack ((x, y), facing) lines ang colour = let (_, newfacing) = move L ang ((x, y), facing)
                                                                in tracehelper2 cs stack ((x, y), newfacing) lines ang colour
tracehelper2 (R : cs) stack ((x, y), facing) lines ang colour = let (_, newfacing) = move R ang ((x, y), facing)
                                                                in tracehelper2 cs stack ((x, y), newfacing) lines ang colour
tracehelper2 (B cmds : cs) stack state lines ang colour = tracehelper2 cs ((state, cmds) : stack) state lines ang colour

--I do not think I did trace1 and trace2 right but somehow to run cabal test gives no output for the project so I couldn't really debug
--Also calling drawLSystem1 leads to *** Exception: user error (unknown GLUT entry glutInit) on my windows laptop and I have no idea how to solve that either
--so I couldn't do the extension stuff
--(and it might be reasonable not to do them since I couldn't even run a test on the basic project)


-- Provided Functions
------------------------------------------------------------------------------

expandLSystem :: LSystem -> Int -> [Command]
expandLSystem (LSystem _ axiom rs) n = parse commandMap (expand axiom n rs)

commandMap :: Rules Command
commandMap = [ ('M', [F])
             , ('N', [F])
             , ('X', [])
             , ('Y', [])
             , ('A', [])
             , ('+', [L])
             , ('-', [R])
             ]
