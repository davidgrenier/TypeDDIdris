module Utils.Math

import Data.List

export
average : List Int -> Double
average nums = cast (sum nums) / cast (length nums)
