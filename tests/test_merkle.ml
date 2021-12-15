
module Hash = Mirage_crypto.Hash.SHA256
module MerkleTree = Merkle.Make(Hash)

let test_with_txs ~root txs =
  let correct_root = Cstruct.of_hex root in
  let txs = List.map Cstruct.of_hex txs in

  let tree = MerkleTree.compute txs in
  let proofs = List.filter_map (MerkleTree.find_proof tree) txs in
  let root = Option.get @@ MerkleTree.root tree in

  Alcotest.(check string) "Should correctly compute the merkle root" (Cstruct.to_string root) (Cstruct.to_string correct_root);
  Alcotest.(check int) "Should have all the proofs" (List.length proofs) (List.length txs);
  Alcotest.(check bool) "Should verify the merkle path" (List.for_all (MerkleTree.verify_path root) proofs) true

let test_tree_with_power_of_2_tx_count () =
  test_with_txs ~root:"48577decdc11118731ba68839f179378d9b5fb186dc2ae41bebea0f26e5283e3"
    [ "a33d9397f26c605ebe36e44571c0f90f10a2906219f7bfc2f3180a06020c7864"
    ; "d6c21784fd20c6b29e48c8fbac663a4422d117e54b5f3721606b4ef63ce17b7e"
    ; "3a8b8bbc27528c78971779f1065b2f12188871ae67724c1ac55319916a4248a5"
    ; "09438be5471f6b8fe1eb0bb79d597ccce4c6d6447b2b525e0361e05055276c53"
    ; "273ab832692e54bed7d1f368383fbce7ab6ed96d7483b1a043c4d129aea373e4"
    ; "41f192f977e250ac6c9a53947c56b36974771917f83c052e833e1ac387cdd6f7"
    ; "8f355d90eec8252f1ba06df7ca45a82553398efcac93692961e2109b4f57ace8"
    ; "24d51eb7e39a55e9884810b9aa74dbd6668a2efb1910412a41e969657fff9547" ]

let test_tree_with_even_tx_count () =
  test_with_txs ~root:"3162ad5e13341f87974663f1a624db83c5abc2e3c908601d72a99bab24ff77a4"
    [ "a33d9397f26c605ebe36e44571c0f90f10a2906219f7bfc2f3180a06020c7864"
    ; "d6c21784fd20c6b29e48c8fbac663a4422d117e54b5f3721606b4ef63ce17b7e"
    ; "09438be5471f6b8fe1eb0bb79d597ccce4c6d6447b2b525e0361e05055276c53"
    ; "273ab832692e54bed7d1f368383fbce7ab6ed96d7483b1a043c4d129aea373e4"
    ; "41f192f977e250ac6c9a53947c56b36974771917f83c052e833e1ac387cdd6f7"
    ; "24d51eb7e39a55e9884810b9aa74dbd6668a2efb1910412a41e969657fff9547" ]

let test_tree_with_odd_tx_count () =
  test_with_txs ~root:"becb7f9d02e80d014e527edc829e4b20b3f65176f0a0965662e1e31f8d675745"
    [ "0727c505cd8ef35bc0c7cb1533f30938a810ab09fd62d4acc3fe24633e3a2890"
    ; "01bf124f50eb419603c34c36dc272926d24f21b58a3226b7af79c72c2d9c20dc"
    ; "d3372e8c0073b6e5ad96f8e77d2ea34fa9106a353a7ee23335fc7ed1af9b18f3"
    ; "2533cf0a77a307b5dcb2b32ab582c8f93f56536a3c6678284c7a929d152d2ce3"
    ; "6ad757683565d17a0121151582dff584dae6fa472d01f767b38ce7019c9c7ab6"
    ; "cce45eb5db95ace6ff1adde485f3315dc6cb1a5fdb700045cd45eab5075bb542"
    ; "209c87148c8bc0348bd340df2783d0cf24a6b510d62d2fc1c9f25bb0c90c78b1"
    ; "05e0f55be08b902453727adba9f2dbc1403fdfdec7946e5418c74d4cadf3b18a"
    ; "48c801475ed00cff287ee1676253a50e12350536d917cbb8436de194b18787a8"
    ; "9161f59546dbe257a3096bff5e8895673ddc6ad04b740b983e4cc68b44697c10"
    ; "a4dc8c1c0d30556457ea74c26026a9412417ce48af80b1567cd46c607ef84b5e"
    ; "3d3418459b54b0a519ff9d3a7c7ea717811392fe4b01a386fa56ed9479b3fcc4"
    ; "d3a47dc98d4d17c2fdc51f793cd7495419cd616de40c47af4e622b651ca4fbf9"
    ; "fd72b0900f25e0465fc7ef705178db7e212bc61032fe9853533e8f0d05a9868e"
    ; "83725f89f03a57e8aec8387d48489b083b8c357bf0fffbb3c1f997bee151effa"
    ; "d69622ad699771f04b7f4626790bfcec555567f8d8e52f709094ab44235aa5bf"
    ; "ada57acbca80bcb7c13ca16419d3c11ed3b96391ed4c428dd6df0ff9ffadb8f0" ]

(* TODO: Use a better structure in order to make generative testing.
   QCheck seems promising. *)

let test_with_txs_count count =
  let generate_string () =
    let size = 10 in
    let arr = Array.make size 0 in
    for i = 0 to size - 1 do
      arr.(i) <- Random.bits () land 0xff
    done;
    arr
    |> Array.map Char.chr
    |> Array.to_seq
    |> String.of_seq
  in

  let txs = Array.make count "" in
  Array.iteri (fun i _ -> txs.(i) <- generate_string ()) txs;

  let txs = List.map (fun s -> Hash.digest (Cstruct.of_string s)) (Array.to_list txs) in

  let tree = MerkleTree.compute txs in
  let proofs = List.filter_map (MerkleTree.find_proof tree) txs in
  let root = Option.get @@ MerkleTree.root tree in

  Alcotest.(check int) "Should have all the proofs" (List.length proofs) (List.length txs);
  Alcotest.(check bool) "Should verify the merkle path" (List.for_all (MerkleTree.verify_path root) proofs) true
        
let test_generative iterations () =
  Random.self_init ();

  let iteration () =
    let count = Random.bits () mod 500 in
    if count > 0 then (
      try
        test_with_txs_count count
      with exn -> (
        Printf.printf "Failed with %d hashes\n" count;
        raise exn
      )
    )
  in

  for _ = 0 to iterations do
    iteration ()
  done

let () =
  let open Alcotest in
  run "Merkle" [
      "Tree", [ test_case "Test with 2^3 hashes"
                  `Quick test_tree_with_power_of_2_tx_count
              ; test_case "Test with 2^3 - 2 (should have 1 duplication)"
                  `Quick test_tree_with_even_tx_count
              ; test_case "Test with 2^4 + 1 (should have several duplications)"
                  `Quick test_tree_with_odd_tx_count
              ; test_case "Test with random amount of hashes"
                  `Slow (test_generative 200) ]
    ]
  
