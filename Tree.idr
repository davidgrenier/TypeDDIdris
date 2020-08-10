module Tree

public export
data Tree ty
    = Empty
    | Node (Tree ty) ty (Tree ty)
