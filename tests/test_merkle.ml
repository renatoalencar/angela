
module Hash = Mirage_crypto.Hash.SHA256
module MerkleTree = Merkle.Make(Hash)

let test_with_empty_tx () =
  let tree = MerkleTree.compute [] in
  let root = Option.map Cstruct.to_string @@ MerkleTree.root tree in

  Alcotest.(check (option string)) "Should be None" root None

let test_with_single_tx () =
  let tree = MerkleTree.compute [ Cstruct.of_hex "09438be5471f6b8fe1eb0bb79d597ccce4c6d6447b2b525e0361e05055276c53" ] in
  let root = Option.map Cstruct.to_string @@ MerkleTree.root tree in

  Alcotest.(check (option string)) "Should be the same"
    root (Some "\tC\139\229G\031k\143\225\235\011\183\157Y|\204\228\198\214D{+R^\003a\224PU'lS")

let log2 n =
  let open Float in
  n |> of_int |> log2 |> ceil |> to_int |> ((+) 1)

let test_with_txs ~root txs =
  let correct_root = Cstruct.of_hex root in
  let txs = List.map Cstruct.of_hex txs in

  let tree = MerkleTree.compute txs in
  let proofs = List.filter_map (MerkleTree.find_proof tree) txs in
  let root = Option.get @@ MerkleTree.root tree in

  Alcotest.(check string) "Should correctly compute the merkle root"
    (Cstruct.to_string root) (Cstruct.to_string correct_root);
  Alcotest.(check int) "Should have all the proofs"
    (List.length proofs) (List.length txs);
  Alcotest.(check bool) "Should verify the merkle path"
    (List.for_all (MerkleTree.verify_path root) proofs) true;
  Alcotest.(check int) "Should have the right length"
    (List.length txs) (MerkleTree.length tree);
  Alcotest.(check int) "Should have the right height"
    (log2 (List.length txs)) (MerkleTree.height tree)

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

let test_add_with_empty () =
  let digest = (Cstruct.of_hex "cce45eb5db95ace6ff1adde485f3315dc6cb1a5fdb700045cd45eab5075bb542") in
  let tree = MerkleTree.add Merkle.Tree.empty digest in
  let root = Option.get @@ MerkleTree.root tree in

  Alcotest.(check string) "Should have the same digest"
    (Cstruct.to_string digest) (Cstruct.to_string root)

let test_add_with_leaf () =
  let digest = (Cstruct.of_hex "cce45eb5db95ace6ff1adde485f3315dc6cb1a5fdb700045cd45eab5075bb542") in
  let leaf = (Cstruct.of_hex "0727c505cd8ef35bc0c7cb1533f30938a810ab09fd62d4acc3fe24633e3a2890") in
  let tree = MerkleTree.add (Merkle.Tree.leaf leaf) digest in
  let root = Option.get @@ MerkleTree.root tree in

  let expected_root = Cstruct.of_hex "996c1d54f4dc1384a4f76ffc5e1e5ad836a08b18e15491d2a18f533c96361643" in

  Alcotest.(check string) "Should have right root"
    (Cstruct.to_string expected_root) (Cstruct.to_string root)

let test_with_height_2_node () =
  let txs = [ Cstruct.of_hex "0727c505cd8ef35bc0c7cb1533f30938a810ab09fd62d4acc3fe24633e3a2890"
            ; Cstruct.of_hex "cce45eb5db95ace6ff1adde485f3315dc6cb1a5fdb700045cd45eab5075bb542" ] in
  let tree = MerkleTree.compute txs in
  let tree = MerkleTree.add tree (Cstruct.of_hex "8f355d90eec8252f1ba06df7ca45a82553398efcac93692961e2109b4f57ace8") in
  let root = Option.get @@ MerkleTree.root tree in

  let expected_root = Cstruct.of_hex "082370901631ec96525c04e3ecf985cecefe5b34788db9d36b00870a2ad6ed4b" in

  Alcotest.(check string) "Should have the right root"
    (Cstruct.to_string expected_root) (Cstruct.to_string root)

let test_with_a_duplicate_node () =
  let txs = [ Cstruct.of_hex "0727c505cd8ef35bc0c7cb1533f30938a810ab09fd62d4acc3fe24633e3a2890"
            ; Cstruct.of_hex "cce45eb5db95ace6ff1adde485f3315dc6cb1a5fdb700045cd45eab5075bb542" 
            ; Cstruct.of_hex "8f355d90eec8252f1ba06df7ca45a82553398efcac93692961e2109b4f57ace8" ] in
  let tree = MerkleTree.compute txs in
  Merkle.Print.pp_tree tree;


  let tree = MerkleTree.add tree (Cstruct.of_hex "082370901631ec96525c04e3ecf985cecefe5b34788db9d36b00870a2ad6ed4b") in
  let root = Option.get @@ MerkleTree.root tree in

  Merkle.Print.pp_tree tree;

  let expected_root = Cstruct.of_hex "5c7cae1836aa614ef9251cc9132384e1f6bc000a197bff6e9678f3a8f8a5ea7e" in

  Alcotest.(check string) "Should have the right root"
    (Cstruct.to_string expected_root) (Cstruct.to_string root)

let test_with_txs txs =
  let txs = List.map (fun s -> Hash.digest (Cstruct.of_string s)) txs in

  let tree = MerkleTree.compute txs in
  let proofs = List.filter_map (MerkleTree.find_proof tree) txs in
  let root = Option.get @@ MerkleTree.root tree in

  List.length proofs = List.length txs
  && List.for_all (MerkleTree.verify_path root) proofs
  && List.length txs = MerkleTree.length tree
  && log2 (List.length txs) = MerkleTree.height tree

let test_add_with_txs txs =
  let txs = List.map (fun s -> Hash.digest (Cstruct.of_string s)) txs in

  let computed_tree = MerkleTree.compute txs in
  let folded_tree = List.fold_left MerkleTree.add Merkle.Tree.empty txs in
  let computed_root = Option.map Cstruct.to_string @@ MerkleTree.root computed_tree in
  let folded_root = Option.map Cstruct.to_string @@ MerkleTree.root folded_tree in

  computed_root = folded_root

module StringSet = Set.Make(String)

let only_unique_values l =
  StringSet.(cardinal (of_list l)) = List.length l

let test_generative count =
  QCheck.Test.make ~count ~name:"Random digest lists"
    (* Sometimes it fails because it generates two empty strings which have the
       same hash *)
    QCheck.(list_of_size Gen.(1 -- 256) @@ string_of_size Gen.(1 -- 256))
    (fun l ->
      QCheck.assume (only_unique_values l);
      QCheck.assume (List.length l > 0);
      test_with_txs l)

let test_add_generative count =
  QCheck.Test.make ~count ~name:"Add and compute should equivalent"
    (* Sometimes it fails because it generates two empty strings which have the
       same hash *)
    QCheck.(list_of_size Gen.(1 -- 256) @@ string_of_size Gen.(1 -- 256))
    (fun l ->
      QCheck.assume (only_unique_values l);
      QCheck.assume (List.length l > 0);
      test_add_with_txs l)

let () =
  let open Alcotest in
  let generative_tests =
    List.map
      QCheck_alcotest.to_alcotest
      [ test_generative 600 ; test_add_generative 300 ]
  in
  run "Merkle" [
      "Base cases", [ test_case "Test with empty list"
                        `Quick test_with_empty_tx
                    ; test_case "Test with single hash"
                        `Quick test_with_single_tx ]

    ; "Larger cases", [ test_case "Test with 2^3 hashes"
                          `Quick test_tree_with_power_of_2_tx_count
                      ; test_case "Test with 2^3 - 2 (should have 1 duplication)"
                          `Quick test_tree_with_even_tx_count
                      ; test_case "Test with 2^4 + 1 (should have several duplications)"
                          `Quick test_tree_with_odd_tx_count ]

    ; "Adding", [ test_case "Empty + Leaf"
                    `Quick test_add_with_empty
                ; test_case "Leaf + Leaf"
                    `Quick test_add_with_leaf
                ; test_case "Node 2 + Leaf"
                    `Quick test_with_height_2_node
                ; test_case "Duplicate Node + Leaf"
                    `Quick test_with_a_duplicate_node ]

    ; "Generative", generative_tests
    ]

