
module Tree = Tree
module Print = Print

module Make (H: Mirage_crypto.Hash.S) = struct
  open Tree

  type t = Tree.t

  let root = root

  let hash_pair a b =
    H.get (H.feed (H.feed H.empty a) b)

  let either ~left ~right fleft fright =
    match left, right with
    | (Some a, None) -> fleft a
    | (None, Some b) -> fright b
    | (None, None)   -> None
    | (Some _, Some _) -> raise (Invalid_argument "either")

  let rec find_proof t digest =
    match t with
    | Leaf leaf -> Some (Bottom leaf)

    | Node (_, Leaf left, Leaf right) when left = digest ->
      Some (Right (right, Bottom left))
    | Node (_, Leaf left, Leaf right) when right = digest ->
      Some (Left (left, Bottom right))

    | Node (_, (Node (left, _, _) as node), Node (right, _, _)) when left = right -> 
      Option.map (fun path -> Left (left, path)) (find_proof node digest)

    | Node (_, (Node (left, _, _) as node_left), (Node (right, _, _) as node_right)) ->
      either ~left:(find_proof node_left digest) ~right:(find_proof node_right digest)
        (fun path -> Some (Right (right, path)))
        (fun path -> Some (Left (left, path)))

    | Empty
    | Node (_, Leaf _, Leaf _) -> None

    | _ -> assert false

  let rec hash_path path =
    match path with
    | Right (right, Bottom left)
    | Left (left, Bottom right) -> hash_pair left right

    | Right (right, path) -> hash_pair (hash_path path) right
    | Left  (left, path)  -> hash_pair left (hash_path path)

    | Bottom leaf -> leaf

  let verify_path root path =
    root = (hash_path path)

  let rec step nodes =
    match nodes with
    | [] -> []

    | Empty :: nodes -> step nodes
    | node :: Empty :: nodes -> step (node :: nodes)

    | (Leaf a as node_a) :: (Leaf b as node_b) :: nodes
    | (Node (a, _, _) as node_a) :: (Node (b, _, _) as node_b) :: nodes ->
       node (hash_pair a b) node_a node_b :: step nodes

    | (Leaf digest as repeated) :: []
    | (Node (digest, _, _) as repeated) :: [] ->
      [ node (hash_pair digest digest) repeated repeated ]

    (* Invariant: Nodes of different types shouldn't be on the same level *)
    | Leaf _ :: Node _ :: _
    | Node _ :: Leaf _ :: _ -> assert false

  let compute leafs =
    let rec loop nodes =
      match nodes with
      | [] -> empty
      | [root] -> root
      | nodes -> loop (step nodes)
    in
    loop (List.map leaf leafs)

end

