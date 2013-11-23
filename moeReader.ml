
module Json = Yojson.Basic
module JU = Json.Util
open Romanisation


let (|>) x f = f x 

let extract_trs entry =
  let title = entry |> JU.member "title" |> JU.to_string in
  List.map
    (fun js -> let trs = js |> JU.member "trs" |> JU.to_string in (trs,title))
    (entry |> JU.member "heteronyms" |> JU.to_list )

let () =
  let data = Json.from_file Sys.argv.(1) in
  let readings = data |> JU.to_list |> List.map extract_trs |> List.flatten in
  let extended = List.fold_left
      ( fun acc (trs,ji) -> let parsed = TRS.parse trs in
        (TRS.string_of_list Fuzzify.(parsed |> fuzzify_parse tone |> fuzzify_parse final),ji)::(trs,ji)::acc
      ) 
      []
      readings   
  in
  List.iter 
    (fun (x,y) -> print_endline (x^" "^y))
    extended;

