module TRS = struct
  let (|>) x f = f x

  let list_of_opt_list l =
    List.fold_left
      (fun acc el -> match el with 
         | Some x -> x::acc
         | None -> acc
      )
      []
      l
  |> List.rev

  let flags = [`UTF8;`CASELESS]

  type syllable =
    {
      separateur : string option;
      initial : string option;
      mediane : string option;
      finale  : string option;
      ton     : int option;
    }  

  let decompose (s:string) : int array =
    let ia = Utf8.to_int_array s 0 (String.length s) in
    Array.fold_left (fun acc x -> match Uunf.decomp x with [||] -> Array.append acc [|x|] | y -> Array.append acc y ) [||] ia

  let string_of_array (a:int array) :string =
    Utf8.from_int_array a 0 (Array.length a)

  let extract_tone syl  =
    let a = decompose syl in
    let a' = Array.fold_left
      (fun (t,acc) c -> match c with
         | 0x300 -> (Some 3, acc)
         | 0x301 -> (Some 2, acc)
         | 0x302 -> (Some 5, acc)
         | 0x304 -> (Some 7, acc)
         | 0x30d -> (Some 8, acc)
         | letter -> (t, letter::acc) )
      (Some 1,[])
      a 
    in
    (fst a', string_of_array (Array.of_list (List.rev (snd a'))))
  
  let expand trs =
    let open Pcre in
    let rex = regexp ~flags "(ts|tsh|s)i" in
    let trs = replace_first ~rex ~templ:"$1ii" trs in
    let rex = regexp ~flags "^o([^ -o])" in
    let trs = replace ~rex ~templ:"oo$1" trs in
    let rex = regexp ~flags "([^o])o([^ -])" in
    replace ~rex ~templ:"$1oo$2" trs


  let trs_re_imf = Pcre.regexp ~flags "(p|b|ph|m|t|th|n|l|k|g|kh|ng|h|tsi|tshi|si|ts|j|tsh|s)?([aeiou+]+|ng|m)(ng|nn|N|m|n|r|p|t|h|k)?"

  type parsing_result = Syl of syllable | Other of string

  let syllable_of_trs s =
    let open Pcre in
    let aux s =
      let i = s.(1) in
      let m = s.(2) in
      let f = s.(3) in
      {separateur=None; initial=i; mediane=m; finale=f; ton=None}
    in
    let ton,syl = extract_tone s in 
    let syl = expand syl in 
    try 
      let m = (Pcre.exec ~rex:trs_re_imf syl) in
      let subs = get_opt_substrings m in
      let (debut,fin) = get_substring_ofs m 0 in
      let len = String.length syl in
      let prefix = if debut != 0 
        then Some (Other (String.sub syl 0 debut))
        else None
      in
      let suffix = if fin != len
        then Some (Other (String.sub syl fin (len-fin)))
        else None
      in
      list_of_opt_list 
        [prefix;  
         Some (Syl ({(aux subs) with ton=ton}));
         suffix]
    with
      Not_found -> [Other s] 

  let so_of_io = function
    |Some i -> Some (string_of_int i)
    |None -> None

  let string_of_option = function
    | Some s -> s
    | None -> ""


  let parse s =
    let open Pcre in
    let re = regexp ~flags " |--|-" in
    List.fold_left 
      (fun (delim,syls) m -> match m with
         | Text t -> (
             let parsed = syllable_of_trs t in
             List.fold_left 
               (fun (d,s) syl -> match syl with 
                  | Syl syl -> (None, (Syl {syl with separateur=d})::s)
                  | Other x -> (None, (Other ((string_of_option d)^x))::s)
               )
               (delim,syls)
               parsed
           )             
         | Delim d -> ((Some d),syls)
         | NoGroup -> raise (Invalid_argument "pb de regex, should not happen")
         | Group  _ -> raise (Invalid_argument "pb de regex, should not happen")
      )
      (None,[])
      (full_split ~rex:re  s)
  |> snd |> List.rev 


  let string_of_syl ?(sep="") syl =
    let i = match syl.initial with
      | None -> ""
      | Some s -> let len = String.length s in
        if String.get s (len-1) = 'i' then String.sub s 0 (len-1) else s
    in
    let m = string_of_option syl.mediane in
    let f = string_of_option syl.finale in
    let t = match syl.ton with
      | None -> ""
      | Some x -> (string_of_int x)
    in 
    let m' = if f != "" then Pcre.replace ~pat:"oo" ~templ:"o" m else m in
    String.concat sep [i;m';f;t]


  let string_of_list ?sepm:(sm="") ?sepp:(sp="") (l:parsing_result list) : string =
    let s_of_parse_result = function
      | Other s -> s
      | Syl syl -> string_of_syl ~sep:sp syl
    in
    String.concat sm 
      (List.map s_of_parse_result l)



end

module Fuzzify = struct
  let separator syl = TRS.( {syl with separateur=None } )
  let final syl = TRS.( {syl with finale=None} )
  let tone syl = TRS.( {syl with ton=None} )
  let fuzzify_parse f parse_result = 
    let open TRS in
    List.map 
      (function 
        | Other s -> Other ""
        | Syl syl -> Syl(f syl)
      )
      parse_result
end

