open Romanisation
open Pcre

let (|>) x f = f x

let flags = [`UTF8;`CASELESS]

(*let zhuyin_re = Pcre.regexp ~flags "(p|b|ph|m|t|th|n|l|k|g|kh|ng|h|tsi|tshi|si|ts|j|tsh|s)?([aeiou+]+|ng|m)(ng|nn|N|m|n|r|p|t|h|k)?"
"(ㄉ|ㄙ|ㄒ|ㄏ|ㆠ|ㄗ|ㄐ|ㄘ|ㄑ|ㆣ|ㆡ|ㆢ|ㄍ|ㄎ|ㄌ|ㄇ|ㄋ|ㄫ|ㄅ|ㄆ|ㄊ)?("*)
let zhuyin_re = Pcre.regexp ~flags "(ㄅ|ㄆ|ㄇ|ㄉ|ㄊ|ㄋ|ㄌ|ㄍ|ㄎ|ㄏ|ㄐ|ㄑ|ㄒ|ㄓ|ㄔ|ㄕ|ㄖ|ㄗ|ㄘ|ㄙ|)?([ㄚㄛㄜㄩㄨㄝㄧㄟ]+|ㄞ|ㄠ|ㄡ|ㄢ|ㄣ|ㄤ|ㄥ|ㄦ|ㄤ|ㄇ)([ㄇㄥㄅㄉㄍㄏ])?"


let convert s = 
  let i = match s.initiale with
    | "ㄅ" -> "p" 
    | "ㄆ" -> "ph"
    | "ㄇ" -> "m"
    | "ㄉ" -> "t"
    | "ㄊ" -> "th"
    | "ㄋ" -> "n"
    | "ㄌ" -> "l"
    | "ㄍ" -> "k"
    | "ㄎ" -> "kh"
    | "ㄏ" -> "h"
    | "ㄐ" -> "tsi"
    | "ㄑ" -> "tshi"
    | "ㄒ" -> "si"
    | "ㄖ" -> "j"
    | "ㄗ" -> "ts"
    | "ㄘ" -> "tsh"
    | "ㄙ" -> "s"
  in

type syl = TRS.syllable

type word = syl list

let filter_option l =
  let rec aux l' = match l' with
    | [] -> []
    | (Some x)::l'' -> x::(aux l'')
    | None::l'' -> aux l''
  in
  List.rev (aux l)

let unique l =
  let rec aux acc l' = match l' with
    | [] -> List.rev acc
    | x::l'' -> if (List.mem x acc) then aux acc l'' else aux (x::acc) l'' 
  in aux [] l




  let syllable_of_zhuyin input =
    let open TRS in 
    let open Pcre in
    let aux s =
      let i = s.(1) in
      let m = s.(2) in
      let f = s.(3) in
      {separateur=None; initial=i; mediane=m; finale=f; ton=None}
    in
    try 
      let m = (Pcre.exec ~rex:zhuyin_re input) in
      let subs = get_opt_substrings m in
      let (debut,fin) = get_substring_ofs m 0 in
      let len = String.length input in
      let prefix = if debut <> 0 
        then Some (Other (String.sub input 0 debut))
        else None
      in
      let suffix = if fin <> len
        then Some (Other (String.sub input fin (len-fin)))
        else None
      in
      filter_option
        [prefix;  
         Some (Syl ({(aux subs) with ton=None}));
         suffix]
    with
      Not_found -> [Other input] 


let discard_non_zhuyin l =
    List.map (function (TRS.Syl s) -> Some s | _ -> None) l |> filter_option

let s_test =
  TRS.(
    {separateur=None;initial=Some "k";mediane=Some"io";finale=None;ton=None}
  )




let single_edit word func : word list=
  let (_,out) = List.fold_left
    (fun (before,output) current -> 
       let output' = List.map (fun l -> current::l) output in
       let output'' = match (func current) with
         | Some x -> (x::before)::output'
         | None -> output'
       in
       (current::before,output'')
    )
    ([],[])
    word
  in
  List.map List.rev out





let more_edits (word_l: word list) fun_l = 
  word_l @ 
  (List.map 
     (fun w ->
        List.map (*TODO: ne pas appliquer à toutes les syl, mais à chacune !!! *) 
           (single_edit w) fun_l)
         
    word_l
  |> List.flatten |> List.flatten ) |> unique


let change_entering_tone t syl =
  let open TRS in
  match syl.finale with 
  | Some x -> None
  | None -> Some {syl with finale=t}


let vocalize syl =
  let open TRS in
  match syl.initial with
  | Some "p" -> Some {syl with initial=Some "b"}
  | Some "k" -> Some {syl with initial=Some "g"}
  | _ -> None


let func_list = 
  vocalize::(List.map 
               (fun t -> change_entering_tone t)
               [Some "t"; Some "p" ;Some "h"; Some "k"])


