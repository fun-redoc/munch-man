module Step where

-- | holds the step for the game loop
data Step a
  = StepChange a a -- ^ Previos State, Next State
  | StepHold     a -- ^ Current State
  deriving (Show, Eq)

-- | extract current State from step
current::Step a -> a
current (StepChange _ a) = a
current (StepHold     a) = a
