module TRS = struct
  let (|>) x f = f x

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
    let open Str in
    let re = regexp "\\(ts\\|tsh\\|s\\)i" in
    let trs = global_replace re "\\1ii" trs in
    let re2 = regexp "o\\([^ -]\\)" in
    global_replace re2 "oo\\1" trs


  let trs_re_imf = Str.regexp "\\(p\\|b\\|ph\\|m\\|t\\|th\\|n\\|l\\|k\\|g\\|kh\\|ng\\|h\\|tsi\\|tshi\\|si\\|ts\\|j\\|tsh\\|s\\)?\\([aeiou\\+]+\\|ng\\|m\\)\\(ng\\|nn\\|N\\|m\\|n\\|r\\|p\\|t\\|h\\|k\\)?"

  type parsing_result = Syl of syllable | Other of string

  let syllable_of_trs s =
    let open Str in
    let aux s =
      let i = try Some (matched_group 1 s)
        with Not_found -> None
      in
      let m = try Some (matched_group 2 s)
        with Not_found -> None
      in
      let f = try Some (matched_group 3 s)
        with Not_found -> None
      in
      {separateur=None; initial=i; mediane=m; finale=f; ton=None}
    in
    let ton,syl = extract_tone s in 
    let syl = expand syl in 
    if string_match trs_re_imf syl 0
    then Syl ({(aux syl) with ton=ton})
    else Other s 

  let so_of_io = function
    |Some i -> Some (string_of_int i)
    |None -> None

  let string_of_option = function
    | Some s -> s
    | None -> ""


  let parse s =
    let re = Str.regexp " \\|--\\|-" in
    List.fold_left 
      (fun (delim,syls) m -> match m with
         | Str.Text t -> ( match syllable_of_trs t with
             | Syl syl -> (None, (Syl {syl with separateur=delim})::syls)
             | Other x -> (None, (Other ((string_of_option delim)^x))::syls)
           )             
         | Str.Delim d -> ((Some d),syls))
      (None,[])
      (Str.full_split re s)
  |> snd |> List.rev 


  let string_of_list ?sepm:(sm="") ?sepp:(sp="") (l:parsing_result list) : string =
    let s_of_syl syl =
      String.concat sp (List.map string_of_option [syl.separateur; syl.initial; syl.mediane; syl.finale;(so_of_io syl.ton)]) in
    let s_of_parse_result = function
      | Other s -> s
      | Syl syl -> s_of_syl syl
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

