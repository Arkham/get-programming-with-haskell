module Shape where

data Shape
  = CircleShape Circle
  | SquareShape Square
  | RectangleShape Rectangle
  deriving (Eq, Show)

newtype Circle = Circle
  { radius :: Double
  } deriving (Eq, Show)

newtype Square = Square
  { side :: Double
  } deriving (Eq, Show)

data Rectangle = Rectangle
  { rectangleWidth :: Double
  , rectangleLength :: Double
  } deriving (Eq, Show)

perimeter :: Shape -> Double
perimeter (CircleShape circle) = radius circle * 2 * pi
perimeter (SquareShape square) = side square * 4
perimeter (RectangleShape rectangle) =
  (rectangleWidth rectangle * 2) + (rectangleLength rectangle * 2)

example :: [Double]
example =
  let circle = CircleShape Circle {radius = 2}
      square = SquareShape Square {side = 3}
      rectangle =
        RectangleShape Rectangle {rectangleWidth = 10, rectangleLength = 3}
   in [perimeter circle, perimeter square, perimeter rectangle]
