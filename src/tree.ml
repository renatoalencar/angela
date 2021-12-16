type digest = Cstruct.t

type t = | Empty
         | Leaf of digest
         | Node of (digest * t * t)

type path = | Left of (digest * path)
            | Right of (digest * path)
            | Bottom of digest

let empty = Empty

let leaf x = Leaf x

let node digest left right = Node (digest, left, right)

let root = function
  | Leaf digest | Node (digest, _, _) -> Some digest
  | Empty -> None
