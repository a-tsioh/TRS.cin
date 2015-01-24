
module Json = Yojson.Basic
module JU = Json.Util
open Romanisation


let (|>) x f = f x 

let rec multimap_append funclist x acc =
  match funclist with
    | f::l' -> multimap_append l' x ((f x)::acc)
    | [] -> acc 

let multimap funclist x = 
  multimap_append funclist x []



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


let split_dialectal_variation s =
  let open Pcre in
  let aux s =
    match split  ~rex:(regexp "　") (trim s)  with
    | [lect] ->  lect
    | [w; lect] -> lect
    | _ -> raise (Invalid_argument s)
  in
  if pmatch ~pat:".*　.*" s
  then (* avec variante graphique *)
    List.map aux (split ~rex:(regexp "[,;]") s)
  else (* variante de prononciation uniquement *)
    [s]

let add_dialectal_variation acc var =
  let s = JU.to_string var in
  if Pcre.pmatch ~pat:"暫無資料.*" s
  then acc
  else  
    List.append acc (split_dialectal_variation s)
 


let extract_trs entry =
  let title = entry |> JU.member "title" |> JU.to_string in
  List.fold_left
    (fun acc js -> 
       let std_readings = js |> JU.member "trs" |> JU.to_string |> trim |> Pcre.split ~pat:"/" in
       let all_readings = match JU.member "dialects" js with
         | `Assoc d -> List.fold_left (fun acc (lieu,var) -> add_dialectal_variation acc var) std_readings d
         | _ -> std_readings
       in
       let filtered = List.filter (fun x -> (String.length x) < 16) (List.filter no_space all_readings) in 
       let couples = List.map (fun s -> (s,title)) filtered in
       List.append couples acc
    )
    [] 
    (entry |> JU.member "heteronyms" |> JU.to_list )



let mapping_list = [
   (fun (trs,ji) -> TRS.string_of_list ~sepm:"" ~discard_non_trs:true (TRS.parse trs),ji);
   (fun (trs,ji) -> TRS.string_of_list ~sepm:""  ~discard_non_trs:true (TRS.parse trs),trs);
   (*(fun (trs,ji) -> (TRS.string_of_list ~sepm:"" ~discard_non_trs:true Fuzzify.( TRS.parse trs 
                                                           |> fuzzify_parse tone 
                                                           |> fuzzify_parse final 
                                                           |> fuzzify_parse mediane),ji)); 
   (fun (trs,ji) -> (TRS.string_of_list ~sepm:"" ~discard_non_trs:true Fuzzify.(TRS.parse trs |> fuzzify_parse tone),trs));
   (fun (trs,ji) -> (TRS.string_of_list ~sepm:"" ~discard_non_trs:true Fuzzify.(TRS.parse trs |> fuzzify_parse tone),ji));*)
(*   (fun (trs,ji) -> TRS.string_of_list ~sepm:"" (TRS.parse trs),(Bopomo.string_of_list ~sepm:"" (TRS.parse trs))); *)
 (* (fun (trs,ji) -> (NonExtBopomo.string_of_list ~sepm:"" Fuzzify.(TRS.parse trs |> fuzzify_parse tone),trs));
  (fun (trs,ji) -> (NonExtBopomo.string_of_list ~sepm:"" Fuzzify.(TRS.parse trs |> fuzzify_parse tone),ji));  *)
  ]

let () =
  let data = Json.from_file Sys.argv.(1) in
  let readings = data |> JU.to_list |> List.map extract_trs |> List.flatten in
  let extended = List.fold_left
      (fun acc (trs,ji) -> multimap_append mapping_list (trs,ji) acc)
      []
      readings   
  in
  List.iter 
    (fun (x,y) -> if x <> "" && (y <> "") then print_endline (x^" "^y) else ())
    extended;

