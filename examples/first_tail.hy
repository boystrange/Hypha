-- EXAMPLE: first sends a message on the first channel, tail sends
-- a message on any subsequent channel

  { *first?(x,y).x!1 }
|
  { *tail?(x,y).all!y }
|
  { *all?(x,y).{ x!3 | all!y } }
|
  first!l | tail!l
