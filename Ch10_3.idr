module Ch10_3

import Utils.Prelude
import DataStore

testStore : DataStore (SString .+. SString .+. SInt)
testStore =
    addToStore ("Pluto", "New Horizons", 2015) empty
    |> addToStore ("Uranus", "Voyager 2", 1986)
    |> addToStore ("Venus", "Venera", 1961)
    |> addToStore ("Mercury", "Mariner 10", 1974)

listItems : DataStore schema -> List (SchemaType schema)
listItems store with (storeView store)
    listItems store | SNil = []
    listItems (addToStore value store) | (SAdd rec) = value :: listItems store | rec

filterKeys : (test : SchemaType schema -> Bool) -> DataStore (SString .+. schema) -> List String
filterKeys test store with (storeView store)
    filterKeys test store | SNil = []
    filterKeys test (addToStore value store) | (SAdd rec) =
        let rest = (filterKeys test store | rec) in
        if test (snd value)
            then fst value :: rest
            else rest
