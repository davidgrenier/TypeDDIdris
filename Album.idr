module Album

import Data.List

record Album where
    constructor Create
    artist : String
    title : String
    years : Integer

help : Album
help = Create "The Beatles" "Help" 1965

rubbersoul : Album
rubbersoul = Create "The Beatles" "Rubber Soul" 1965

clouds : Album
clouds = Create "Joni Mitchell" "Clouds" 1969

hunkydory : Album
hunkydory = Create "David Bowie" "Hunky Dory" 1971

heroes : Album
heroes = Create "David Bowie" "Heroes" 1977

collection : List Album
collection = [help, rubbersoul, clouds, hunkydory, heroes]

Eq Album where
    (==) (Create artist title years) (Create artist' title' years') =
        artist == artist' && title == title' && years == years'

Ord Album where
    compare (Create artist title years) (Create artist' title' years') =
        case compare artist artist' of
        EQ =>
            case compare years years' of
            EQ => compare title title'
            order => order
        order => order
