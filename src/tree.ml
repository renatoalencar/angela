type digest = Cstruct.t

type t = | Empty
         | Leaf of digest
         | Node of { digest: digest
                   ; height: int
                   ; length: int
                   ; left: t
                   ; right: t }
         | DuplicateNode of { digest: digest
                            ; height: int
                            ; length: int
                            ; node: t }

type path = | Left of (digest * path)
            | Right of (digest * path)
            | Bottom of digest

let empty = Empty

let leaf x = Leaf x

let length t =
  match t with
  | Empty -> 0
  | Leaf _ -> 1
  | DuplicateNode { length ; _ }
  | Node { length ; _ } -> length

let height t =
  match t with
  | Empty -> 0
  | Leaf _ -> 1
  | DuplicateNode { height ; _ }
  | Node { height ; _ } -> height

let node digest left right =
  Node { digest
       ; left
       ; right
       ; height = height left + 1
       ; length = length left + length right }

let duplicate_node digest node =
  DuplicateNode { digest
                ; node
                ; height = height node + 1
                ; length = length node }

let root = function
  | Leaf digest
  | DuplicateNode { digest ; _ }
  | Node { digest; _ } -> Some digest
  | Empty -> None
