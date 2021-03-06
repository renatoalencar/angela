
module Make (H: Mirage_crypto.Hash.S) : sig
  type digest = Cstruct.t

  type 'a node
  type 'a duplicate_node

  type t = | Empty
           | Leaf of digest
           | Node of t node
           | DuplicateNode of t duplicate_node

  type path = | Left of (digest * path)
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

  module Print : sig
    val pp_path : ?fmt:Format.formatter -> path -> unit

    val pp_tree : ?fmt:Format.formatter -> t -> unit
  end
end

