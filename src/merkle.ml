
module Make (H: Mirage_crypto.Hash.S) = struct
  module Print = Print

  include Tree

  let hash_pair a b =
    H.get (H.feed (H.feed H.empty a) b)

  let either ~left ~right f_left f_right =
    match left, right with
    | (Some a, None)   -> f_left a
    | (None, Some b)   -> f_right b
    | (None, None)     -> None
    | (Some _, Some _) -> assert false

  let find_proof t digest =
    let rec find_proof_aux t =
      match t with
      | Leaf leaf when leaf = digest ->
         Some (Bottom leaf)

      | DuplicateNode { node ; _ } ->
         Option.map
           (fun path -> Left (Tree.digest_exn node, path))
           (find_proof_aux node)

      | Node { left ; right ; _ } ->
        either ~left:(find_proof_aux left) ~right:(find_proof_aux right)
          (fun path -> Some (Right (Tree.digest_exn right, path)))
          (fun path -> Some (Left (Tree.digest_exn left, path))) 

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

  let compute leaves =
    let rec step nodes =
      match nodes with
      | [] -> []

      | Empty :: nodes -> step nodes
      | node :: Empty :: nodes -> step (node :: nodes)

      | (Leaf a as node_a) :: (Leaf b as node_b) :: nodes
      | (Node { digest = a; _ } as node_a) :: (Node { digest = b ; _ } as node_b) :: nodes
      | (Node { digest = a; _ } as node_a) :: (DuplicateNode { digest = b; _ } as node_b) :: nodes ->
        node (hash_pair a b) node_a node_b :: step nodes

      | (Leaf digest as repeated) :: []
      | (Node  { digest ; _ } as repeated) :: []
      | (DuplicateNode { digest ; _ } as repeated) :: [] ->
        [ duplicate_node (hash_pair digest digest) repeated ]

      (* Invariant: Nodes of different types shouldn't be on the same level *)
      | Leaf _ :: Node _ :: _
      | Node _ :: Leaf _ :: _
      | Leaf _ :: DuplicateNode _ :: _
      | DuplicateNode _ :: _ :: _  -> assert false
    in
    let rec loop nodes =
      match nodes with
      | [] -> empty
      | [root] -> root
      | nodes -> loop (step nodes)
    in
    loop (List.map leaf leaves)

  let is_full_node node =
    match node with
    | Node { left; right; _ } -> length left = length right
    | Empty | DuplicateNode _ -> false
    | Leaf _ -> true

  let rec make_duplicate_branch height leaf =
    if height > 1 then
      let node = make_duplicate_branch (height - 1) leaf in
      let digest = Tree.digest_exn node in
      Tree.duplicate_node (hash_pair digest digest) node
    else
      Tree.duplicate_node (hash_pair leaf leaf) (Tree.leaf leaf)

  let make_node t digest node =
    let digest' = Tree.digest_exn node in
    Tree.node
      (hash_pair digest digest')
      t node

  let rec add t leaf =
    match t with
    | Empty ->
       Tree.leaf leaf
    | Leaf digest ->
       Tree.node (hash_pair digest leaf) t (Tree.leaf leaf)

    | Node { digest; height ; _ } when is_full_node t ->
       make_node t digest
         (make_duplicate_branch (height - 1) leaf)

    | Node { left ; right ; _ } ->
       make_node left
         (Tree.digest_exn left)
         (add right leaf)

    | DuplicateNode { node; _ } when is_full_node node ->
       add node leaf

    | DuplicateNode { node; _ } ->
       let node = add node leaf in
       let digest = Tree.digest_exn node in

       Tree.duplicate_node (hash_pair digest digest) node

end

