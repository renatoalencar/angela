module SHA256 = Mirage_crypto.Hash.SHA256

module MerkleTree = Merkle.Make(SHA256)

let txs = [ "It's log forge"
          ; "Log for jay"
          ; "Log forge"
          ; "It stands for log for java"
          ; "It forges logs" ]

let digests = List.map (fun s -> SHA256.digest (Cstruct.of_string s)) txs

let () =
  let tree = MerkleTree.compute digests in
  let proof = MerkleTree.find_proof tree (List.hd digests) in
  Merkle.Print.pp_tree Format.std_formatter tree;
  Option.iter (Merkle.Print.pp_path Format.std_formatter) proof
