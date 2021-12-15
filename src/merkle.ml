
module Make (H: Mirage_crypto.Hash.S) = struct
  type t = Empty | Leaf of Cstruct.t | Node of (Cstruct.t * t * t)

  type path = | Left of (Cstruct.t * path)
              | Right of (Cstruct.t * path)
              | Bottom of Cstruct.t
  
  let empty = Empty

  let leaf x = Leaf x

  let node digest left right = Node (digest, left, right)

  let root t =
    match t with
    | Leaf digest 
    | Node (digest, _, _) -> Some digest
    | Empty -> None

  let hash_pair a b =
    H.get (H.feed (H.feed H.empty a) b)

  let rec find_proof t digest =
    match t with
    | Node (_, Leaf left, Leaf right) when left = digest ->
      Some (Right (right, Bottom left))
    | Node (_, Leaf left, Leaf right) when right = digest ->
      Some (Left (left, Bottom right))

    | Node (_, (Node (left, _, _) as node_left), (Node (right, _, _) as node_right)) -> (
      match find_proof node_left digest, find_proof node_right digest with
      | (Some path, None) -> Some (Right (right, path))
      | (None, Some path) -> Some (Left (left, path))
      | (Some path, Some _) when left = right -> Some (Left (left, path)) (* Duplicated paths *)
      | (None, None) -> None
      | (Some _, Some _) -> assert false (* Hash collision?  *)
    )

    | Empty
    | Node (_, Leaf _, Leaf _) -> None

    | Leaf leaf -> Some (Bottom leaf)

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

  let digest_to_hex digest =
    digest
    |> Cstruct.to_string
    |> String.to_seq
    |> Seq.map (fun c -> Printf.sprintf "%x" (Char.code c))
    |> List.of_seq
    |> String.concat ""

  let rec print_proof proof =
    match proof with
    | Left (digest, path) ->
      Printf.sprintf "Left %s ->\n%s" (digest_to_hex digest) (print_proof path)
    | Right (digest, path) ->
      Printf.sprintf "Right %s ->\n%s" (digest_to_hex digest) (print_proof path)
    | Bottom digest ->
      Printf.sprintf "Bottom %s\n" (digest_to_hex digest)

  let rec step nodes =
    match nodes with
    | [] -> []
    | Empty :: nodes -> step nodes

    | (Leaf a as node_a) :: (Leaf b as node_b) :: nodes
    | (Node (a, _, _) as node_a) :: (Node (b, _, _) as node_b) :: nodes ->
       node (hash_pair a b) node_a node_b :: step nodes

    | (Leaf digest as repeated) :: []
    | (Node (digest, _, _) as repeated) :: [] ->
      [ node (hash_pair digest digest) repeated repeated ]

    (* TODO: This are mostly invariants, but being four, I'm a little bit
       uncomfortable with that. I'd like to have something encoded within
       the type system to avoid that. *)
    | _ -> assert false

  let compute leafs =
    let rec loop nodes =
      match nodes with
      | [] -> empty
      | [root] -> root
      | nodes -> loop (step nodes)
    in
    loop (List.map leaf leafs)

  let print_indented indent str =
      print_string (String.make indent ' ');
      print_endline str

  let print_node ~indent ~label digest =
    print_indented indent label;
    print_indented indent (digest_to_hex digest)

  let rec print_tree ?indent node =
    let indent = Option.value ~default:0 indent in
    match node with
    | Node (digest, left, right) ->
       print_node ~indent ~label:"Node digest" digest;
       
       print_string "\n";
       print_indented indent "Left node" ;
       print_tree ~indent:(indent + 4) left;

       print_string "\n";
       print_indented indent "Right node";
       print_tree ~indent:(indent + 4) right
    | Leaf digest ->
       print_node ~indent ~label:"Leaf digest" digest
    | Empty ->
       print_indented indent "Empty node\n"
end
