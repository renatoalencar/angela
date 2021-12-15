
module Tree = struct
  type digest = Cstruct.t

  type t = Empty | Leaf of digest | Node of (digest * t * t)

  type path = | Left of (digest * path)
              | Right of (digest * path)
              | Bottom of digest
  
  let empty = Empty

  let leaf x = Leaf x

  let node digest left right = Node (digest, left, right)

  let root = function
    | Leaf digest | Node (digest, _, _) -> Some digest
    | Empty -> None
end

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

module Print = struct
  open Tree

  open Format

  let pp_digest fmt digest =
    let digest = Cstruct.to_string digest in
    String.iter
      (fun c -> fprintf fmt "%x" (Char.code c))
      digest

  let pp_path fmt path =
    let print_node label digest =
      pp_open_hbox fmt ();
      pp_print_string fmt label;
      pp_print_space fmt ();
      pp_digest fmt digest;
      pp_close_box fmt ();
      pp_print_space fmt ();
    in
    let rec print_path path =
      match path with
      | Left (digest, path) ->
         print_node "Left" digest;
         print_path path
      | Right (digest, path) ->
         print_node "Right" digest;
         print_path path
      | Bottom digest ->
         print_node "Bottom" digest
    in
    pp_open_vbox fmt 0;
    print_path path;
    pp_close_box fmt ()

  let rec pp_print_tree ?indent fmt node =
    let pp_node fmt label digest =
      pp_print_string fmt label ;
      pp_print_break fmt 2 0;
      pp_digest fmt digest;
      pp_print_break fmt 2 0
    in
    let pp_branch fmt indent label node =
      pp_print_string fmt label ;
      pp_print_break fmt 2 0;
      pp_print_tree ~indent fmt node;
    in

    let indent = Option.value ~default:0 indent in
    match node with
    | Node (digest, left, right) ->
       pp_node fmt "Node digest" digest;

       pp_open_vbox fmt (indent + 2);

       pp_print_break fmt 2 0;

       pp_branch fmt indent "Left node" left;
       pp_print_break fmt 2 0;

       pp_branch fmt indent "Right node" right;

       pp_close_box fmt ()
    | Leaf digest ->
       pp_node fmt "Leaf digest" digest;
    | Empty ->
       pp_print_string fmt "Empty node"

  let pp_tree fmt node =
    pp_open_vbox fmt 0;
    pp_print_tree fmt node;
    pp_print_break fmt 2 0;
    pp_close_box fmt ()

end
