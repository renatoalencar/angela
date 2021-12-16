
module Tree = Tree
module Print = Print

module Make (H: Mirage_crypto.Hash.S) = struct
  open Tree

  type t = Tree.t

  let root = root

  let hash_pair a b =
    H.get (H.feed (H.feed H.empty a) b)

  let either ~left ~right f_left f_right =
    match left, right with
    | (Some a, None)   -> f_left a
    | (None, Some b)   -> f_right b
    | (None, None)     -> None
    | (Some _, Some _) -> raise (Invalid_argument "either")

  let node_digest = function
    | Node (digest, _, _) | Leaf digest -> digest
    | Empty -> raise (Invalid_argument "node_digest")

  let same_node a b =
    match a, b with
    | Node (a, _, _), Node (b, _, _) -> a = b
    | a, b -> a = b

  let find_proof t digest =
    let rec find_proof_aux t =
      match t with
      | Leaf leaf when leaf = digest -> Some (Bottom leaf)

      | Node (_, left, right) ->
         if same_node left right then
           Option.map
             (fun path -> Left (node_digest left, path))
             (find_proof_aux left)
         else
           either ~left:(find_proof_aux left) ~right:(find_proof_aux right)
             (fun path -> Some (Right (node_digest right, path)))
             (fun path -> Some (Left (node_digest left, path))) 

      | Leaf _ | Empty -> None
    in
    find_proof_aux t

  let verify_path root path =
    let rec hash_path path =
      match path with
      | Bottom leaf -> leaf
      | Right (right, path) -> hash_pair (hash_path path) right
      | Left  (left, path)  -> hash_pair left (hash_path path)
    in
    root = (hash_path path)

  let compute leafs =
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
    in
    let rec loop nodes =
      match nodes with
      | [] -> empty
      | [root] -> root
      | nodes -> loop (step nodes)
    in
    loop (List.map leaf leafs)

end

