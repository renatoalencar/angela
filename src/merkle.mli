
module Make (H: Mirage_crypto.Hash.S) : sig
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

  val height : t -> int

  val length : t -> int

  val find_proof : t -> digest -> path option

  val verify_path : digest -> path -> bool

  val compute : digest list -> t

  val add : t -> digest -> t

  val mem : digest -> t -> bool

  module Print : sig
    val pp_path : ?fmt:Format.formatter -> path -> unit

    val pp_tree : ?fmt:Format.formatter -> t -> unit
  end
end

