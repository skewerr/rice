{-# LANGUAGE NoMonomorphismRestriction #-}

module Config.Dimensions where

import Data.Ratio

(screenWidth, screenHeight) = (1366, 768)
(characterWidth, characterHeight) =  (6, 12)
(horizontalPadding, verticalPadding) = (2, 2)

panelHeight = 20
borderWidth =  2
terminalPadding = 3
windowGap = 1

availableHorizontalSpan = screenWidth - 2 * horizontalPadding
availableVerticalSpan = screenHeight - 2 * verticalPadding - panelHeight
commonGap = windowGap + borderWidth + terminalPadding

resizeRatio = characterWidth % availableHorizontalSpan
topWindowRatio = terminalLRatio 22
rightColumnsRatio = terminalCRatio 81
masterRatio = 1 - rightColumnsRatio

-- | Calculates the horizontal ratio of a terminal window given the number of
-- columns it should have.
terminalCRatio :: Integral a => a -> Ratio a
terminalCRatio columns = horizontalSpan % availableHorizontalSpan
  where horizontalSpan = columns * characterWidth + 2 * commonGap

-- | Calculates the vertical ratio of a terminal window given the number of
-- lines it should have.
terminalLRatio :: Integral a => a -> Ratio a
terminalLRatio lines = verticalSpan % availableVerticalSpan
  where verticalSpan = lines * characterHeight + 2 * commonGap

-- | Calculates the horizontal ratio of a window given its width in pixels.
horizontalRatio :: Integral a => a -> Ratio a
horizontalRatio width = width % availableHorizontalSpan
