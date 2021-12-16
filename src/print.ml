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
