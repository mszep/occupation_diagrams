{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

arrowLength = 1.0
barLength   = 1.0

hSep = barLength * 2.2
vSep = arrowLength * 1.2

downArrow = arrowAt origin unit_Y #
              translate (r2 (barLength / 6, arrowLength / 2))
upArrow = downArrow # rotateBy 0.5
levelBar = hrule barLength

occEmpty = levelBar
occSingleUp = levelBar <> upArrow
occSingleDown = levelBar <> downArrow
occDouble = levelBar <> upArrow <> downArrow

fromChar :: Char -> Diagram B R2
fromChar '|' = occEmpty
fromChar '<' = occSingleDown
fromChar '>' = occSingleUp
fromChar 'Z' = occDouble
fromChar _   = undefined

myVcat = vcat' (with & sep .~ vSep & catMethod .~ Distrib)
myHcat = hcat' (with & sep .~ hSep & catMethod .~ Distrib)

--occDiagram :: [Char] -> Diagram B R2
occDiagram cs = myVcat $ reverse $ map fromChar cs #
                  lineCap LineCapRound #
                  lw 0.03

occDiagrams ss = (myHcat $ map occDiagram ss) # centerXY # pad 1.1

occDiagramsFromFile :: FilePath -> IO (Diagram SVG R2)
occDiagramsFromFile file = do
  f <- readFile file
  return $ occDiagrams $ lines f

main = mainWith (occDiagramsFromFile)
