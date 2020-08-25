module Ex10_3

import DataStore
import Shape

-- getValues : DataStore (SString .+. schema) -> List (SchemaType schema)
-- getValues = map snd . listItems

getValues : DataStore (SString .+. schema) -> List (SchemaType schema)
getValues store with(storeView store)
    getValues store | SNil = []
    getValues (addToStore value store) | (SAdd rec) = snd value :: getValues store | rec

area : Shape -> Double
area shape with(shapeView shape)
    area (rectangle length width) | Rectangle = length * width
    area (triangle length width) | Triangle = 0.5*length*width
    area (circle radius) | Circle = pi*radius*radius
    area (compose shape1 shape2) | (Composed rec1 rec2) =
        (area shape1 | rec1)
        + area shape2 | rec2

{-
area (triangle 3 4)
area (circle 10)
area (compose (triangle 3 4) (circle 10))
-}
