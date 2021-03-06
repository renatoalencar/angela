# Angela

A Merkle tree implementation in OCaml that works with mirage-crypto.

## How to

Create a module from the functor `Merkle.Make` and a `Mirage_crypto.Hash`
and compute the tree from a list of digests as `Cstruct.t`s.

```ocaml
# module MerkleTree = Merkle.Make(Mirage_crypto.Hash.SHA256) ;;
module MerkleTree :
  sig
    type digest = Cstruct.t
    type 'a node = 'a Merkle.Make(Mirage_crypto.Hash.SHA256).node
    type 'a duplicate_node =
        'a Merkle.Make(Mirage_crypto.Hash.SHA256).duplicate_node
    type t =
      Merkle.Make(Mirage_crypto.Hash.SHA256).t =
        Empty
      | Leaf of digest
      | Node of t node
      | DuplicateNode of t duplicate_node
    type path =
      Merkle.Make(Mirage_crypto.Hash.SHA256).path =
        Left of (digest * path)
      | Right of (digest * path)
      | Bottom of digest
    val empty : t
    val is_empty : t -> bool
    val leaf : digest -> t
    val node : digest -> t -> t -> t
    val root : t -> digest option
    val height : t -> int
    val length : t -> int
    val find_proof : t -> digest -> path option
    val verify_path : digest -> path -> bool
    val digest : t -> digest option
    val digest_exn : t -> digest
    val compute : digest list -> t
    val add : t -> digest -> t
    val mem : digest -> t -> bool
    val fold : ('a -> t -> 'a) -> 'a -> t -> 'a
    val exists : (t -> bool) -> t -> bool
    val for_all : (t -> bool) -> t -> bool
    module Print :
      sig
        val pp_path : ?fmt:Format.formatter -> path -> unit
        val pp_tree : ?fmt:Format.formatter -> t -> unit
      end
  end

# let tree = MerkleTree.compute digests ;;
- : MerkleTree.t =

```

## Examples

### Computing the tree

```ocaml
# let txs = List.map Cstruct.of_hex
    [ "9884e663d3024748c2930d1eb276598e1e05a6ce3dcf7c7a959e04bb5b937767"
    ; "7bf8c23d17f1f1c0fdfb1e797057acb4bde3d9096d9830be47647a708b0371f9"
    ; "8d0160c0d84236dda89711eb07586e3d95b186fed727103aabf3fa7cda07d65f"
    ; "2c0a56c6ca0d14ca8ad7968cc1774fb24637072ce48928b76e0e754833d636f5" ] ;;
val txs : Cstruct.t list =
  [{Cstruct.buffer = <abstr>; off = 0; len = 32};
   {Cstruct.buffer = <abstr>; off = 0; len = 32};
   {Cstruct.buffer = <abstr>; off = 0; len = 32};
   {Cstruct.buffer = <abstr>; off = 0; len = 32}]
                                    
# let tree = MerkleTree.compute txs ;;
val tree : MerkleTree.t = MerkleTree.Node <abstr>

# let root = MerkleTree.root tree ;;
val root : MerkleTree.digest option =
  Some {Cstruct.buffer = <abstr>; off = 0; len = 32}

```

### Computing from a fold

```ocaml
# let val tree : MerkleTree.t = MerkleTree.Node <abstr>
val tree = List.fold_left MerkleTree.add MerkleTree.empty txs ;;
```

### Finding a Merkle path for a particular hash

```ocaml
# let path = MerkleTree.find_proof tree
    (Cstruct.of_hex "8d0160c0d84236dda89711eb07586e3d95b186fed727103aabf3fa7cda07d65f") ;;
val path : MerkleTree.path option =
  Some
   (Merkle.Tree.Left
     ({Cstruct.buffer = <abstr>; off = 0; len = 20},
      Merkle.Tree.Right
       ({Cstruct.buffer = <abstr>; off = 0; len = 32},
        Merkle.Tree.Bottom {Cstruct.buffer = <abstr>; off = 0; len = 32})))
```

### Verifying a Merkle path

```ocaml
# let root = Option.value ~default:Cstruct.empty root in
    path
    |> Option.map (MerkleTree.verify_path root)
    |> Option.value ~default:false
  ;;
- : bool = true

```

### Pretty printing

A tree
```ocaml
# MerkleTree.Print.pp_tree tree ;;
Node digest
23f4662aba9c097357f1b8a22aa1ebf8d658b37

  Left node
  Node digest
  a4ee24c67d6aac10ac3a7734b45f7651ae8158
  
    Left node
    Leaf digest
    9884e663d324748c293d1eb276598e1e5a6ce3dcf7c7a959e4bb5b937767
    
    Right node
    Leaf digest
    7bf8c23d17f1f1c0fdfb1e797057acb4bde3d996d9830be47647a708b371f9
    
  Right node
  Node digest
  babdb8238b5515a7b8507655be7079bc299b090
  
    Left node
    Leaf digest
    8d160c0d84236dda89711eb7586e3d95b186fed727103aabf3fa7cda7d65f
    
    Right node
    Leaf digest
    2ca56c6cad14ca8ad7968cc1774fb2463772ce48928b76ee754833d636f5

- : unit = ()
```

A path
```ocaml
# Option.iter MerkleTree.Print.pp_path path ;;
Left a4ee24c67d6aac10ac3a7734b45f7651ae8158
Right 2ca56c6cad14ca8ad7968cc1774fb2463772ce48928b76ee754833d636f5
Bottom 8d160c0d84236dda89711eb7586e3d95b186fed727103aabf3fa7cda7d65f
- : unit = ()
```

## License

[MIT](./LICENSE)
