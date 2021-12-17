
module Tree : sig
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

  val empty : t

  val leaf : digest -> t

  val node : digest -> t -> t -> t

  val root : t -> digest option
end

module Print : sig
  val pp_path : ?fmt:Format.formatter -> Tree.path -> unit

  val pp_tree : ?fmt:Format.formatter -> Tree.t -> unit
end

module Make (H: Mirage_crypto.Hash.S) : sig
  type t = Tree.t

  val root : t -> Tree.digest option

  val height : t -> int

  val length : t -> int

  val find_proof : t -> Tree.digest -> Tree.path option

  val verify_path : Tree.digest -> Tree.path -> bool

  val compute : Tree.digest list -> t

  val add : t -> Tree.digest -> t
end
