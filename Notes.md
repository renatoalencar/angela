
```ocaml
module type Hash = sig
  type t

  val make : string -> t
end

module type MerklePath = sig
  type t
end

module type MerkleTree (H: Hash) = sig
  type t
  
  val empty n: t
  
  val add_with_proof : string -> t -> (MerkleTree.t, MerklePath.t)
  
  val add : string -> t -> MerkleTree.t
  
  val proof : Hash.t -> t -> MerklePath.t
  
  val proof_opt : Hash.t -> t -> MerklePath.t option
  
  val proof_of_leaf : string -> t -> MerklePath.t

  val proof_of_leaf_opt : string -> t -> MerklePath.t option

  val length : t -> int
end

module SHA256_MerkleTree = MerkleTree(SHA256)

let () =
    let data = [ .... ] in
    List.fold_left SHA256_MerkleTree.add SHA256_MerkleTree.empty data
  
```


```c
void* compute(void* hashes, size_t length) {
    while (length > 1) {
        for (size_t i = 0; i + 1 < length; i += 2) {
            hash_function(hashes[i], hashes[i + 1], hashes[i / 2]);
        }
        
        // not considering odd sized array
        
        length /= 2;
    }
    
    return hashes[0];
}
```

Merkle trees where originally designed as a signature scheme for one time
signature schemes, where instead of signing a single payload, it would sign
several, making the keys reusable for more than one payload.

The payload list size should be a power of 2 instead.

## Requirements

* Should hash together a bunch of digests until it forms a merkle tree,
  according to the spec.
* In case there's a not regular number of nodes (not 2^n) it should duplicate
  the last one.
* It should possible to withdrawn a merkle path from a digest, as a merkle path.
  Therefore, it should be possible to proof that a digest D is present on a complete
  Merkle tree M or not.
* From a Merkle Path P, and a Merkle Root R, it should be possible to determine that
  P belongs to the Merkle tree with root R.

## References

* Mastering Bitcoin, page 170, Merkle Trees section;
* Bitcoin Merkle Implementation: https://github.com/bitcoin/bitcoin/blob/master/src/consensus/merkle.cpp
* Wikipedia: Merkle Tree: https://en.wikipedia.org/wiki/Merkle_tree
* Georg Becker, 2008. Merkle Signature Schemes, Merkle Trees and Their Cryptanalysis:
  https://www.emsec.ruhr-uni-bochum.de/media/crypto/attachments/files/2011/04/becker_1.pdf
* Ralph C. Merkle, 1987. A Digital Signature Based on a Conventional Encryption Function:
  https://link.springer.com/chapter/10.1007%2F3-540-48184-2_32
* MERKLE RALPH C, 1979. Method of providing digital signatures:
  https://worldwide.espacenet.com/patent/search/family/022107098/publication/US4309569A?q=pn%3DUS4309569
* Ralph C. Merkle, 1979. SECRECY, AUTHENTICATION, AND PUBLIC KEY SYSTEMS:
  https://www.merkle.com/papers/Thesis1979.pdf
