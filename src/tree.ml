type digest = Cstruct.t

type 'a node = { digest: digest
               ; height: int
               ; length: int
               ; left: 'a
               ; right: 'a }

type 'a duplicate_node =  { digest: digest
                          ; height: int
                          ; length: int
                          ; node: 'a  }

type t = | Empty
         | Leaf of digest
         | Node of t node
         | DuplicateNode of t duplicate_node

type path = | Left of (digest * path)
            | Right of (digest * path)
            | Bottom of digest

let empty = Empty

let is_empty = function Empty -> true | _ -> false

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

let digest = function
  | Node { digest ; _ }
  | DuplicateNode { digest ; _ }
  | Leaf digest -> Some digest
  | Empty -> None

let digest_exn node =
  match digest node with
  | Some digest -> digest
  | None -> raise (Invalid_argument "Merkle.digest")

let rec fold f a t =
    match t with
    | Node { left; right; _ } ->
       let acc = f a t in
       fold f (fold f acc left) right
    | DuplicateNode { node ; _ } -> 
       fold f (f a t) node
    | el -> f a el

let exists f t =
  fold (fun acc node -> acc || f node) false t

let for_all f t =
  fold (fun acc node -> acc && f node) true t

let mem digest t =
  exists
    (function Leaf leaf -> digest = leaf | _ -> false)
    t
