
module Json = Yojson.Basic
module JU = Json.Util
open Romanisation


let (|>) x f = f x 
let trim s =
  let open Pcre in
  let re_g = Pcre.regexp ~flags:[`UTF8] "^[ 　]+" in
  let re_d = Pcre.regexp ~flags:[`UTF8] "[ 　]+$" in
  Pcre.replace ~rex:re_g s |> Pcre.replace ~rex:re_d


let no_space s = 
  let open Pcre in
  let rex = regexp ~flags:[`UTF8] "[ 　\t]" in
  try 
    ignore (exec ~rex s);
    false
  with
    Not_found -> true


let extract_trs entry =
  let title = entry |> JU.member "title" |> JU.to_string in
  List.fold_left
    (fun acc js -> let trs_list = js |> JU.member "trs" |> JU.to_string |> trim |> Pcre.split ~pat:"/" in
      let trs_filtered = List.filter no_space trs_list in
      List.append
        (List.map (fun s -> (s,title)) trs_filtered)
        acc
    )
    [] 
    
    (entry |> JU.member "heteronyms" |> JU.to_list )

let () =
  let data = Json.from_file Sys.argv.(1) in
  let readings = data |> JU.to_list |> List.map extract_trs |> List.flatten in
  let extended = List.fold_left
      ( fun acc (trs,ji) -> let parsed = TRS.parse trs in
        (TRS.string_of_list ~sepm:"" Fuzzify.(parsed |> fuzzify_parse tone),ji)
        ::(TRS.string_of_list ~sepm:"" Fuzzify.(parsed |> fuzzify_parse tone |> fuzzify_parse final),ji)
        ::(TRS.string_of_list ~sepm:"" Fuzzify.(parsed |> fuzzify_parse tone),trs)
        ::(TRS.string_of_list ~sepm:"" parsed,trs)

(*        ::(trs,ji)*)
        ::acc
      ) 
      []
      readings   
  in
  List.iter 
    (fun (x,y) -> print_endline (x^" "^y))
    extended;

