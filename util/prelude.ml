open Latex

let cite ?extra t =
  let opt = match extra with
    | None -> None
    | Some x -> Some (T,x)
  in
  command "cite" ?opt [T,t] T


let concat_with_sep l sep =
  if l = [] then
    concat []
  else 
    let l_with_one_too_many = List.flatten (List.map (fun t -> [ sep ; t ]) l) in
    concat (List.tl l_with_one_too_many)

type author = {
  name : Latex.t;
  address : Latex.t;
  email : Latex.t option;
}
let make_author a =
  concat [
    command "author" [A,a.name] A; text"\n";
    command "address" [A,a.address] A; text"\n";
    begin match a.email with
    | None -> empty
    | Some m -> command "email" [A,m] A ^^ text "\n"
    end;
    text"%\n";
  ]

let document ~title ?short_title ~authors ~keywords ~acmclass ~abstract ~prelude ~packages body =
  let extra_required = [ text"hyperref", empty ] in
  let short_title = match short_title with None -> None | Some st -> Some (A,st) in
  let keywords = concat_with_sep keywords (text", ") in
  let acmclass = concat_with_sep acmclass (text", ") in
  concat_with_sep [
    documentclass (text"lmcs");
    require_packages (extra_required @ packages);
    required_packages;
    prelude;
    documentmatter (concat_with_sep [
      command "title" ?opt:short_title [A,title] A;
      concat (List.map make_author authors);
      command "keywords" [A,keywords] A;
      command "subjclass" [A,acmclass] A;
      environment "abstract" (A, noindent ^^ text" " ^^ abstract) A;
      text "\\maketitle";
      body;
    ] (text"\n%\n"));
  ] par

let ae = command "ae" [] T

let foreign = emph

let grammardef = mode M (text"::=")

type block_line = (Latex.size option)*Latex.t list
let block_line ?sep l = sep , l

type block = t * alignment list * block_line list
let block title alignment content =
  (title,alignment,content)
let simple_block t c =
  block t [`C] [block_line [c]]
(*
  let title_line x = array_line ~sep:(`Mm 3.) [textsc x] in
  let block l = array_line ~sep:(`Mm 15.) l in
  [ title_line title; block [ content ] ]
*)
(* arnaud: une implémentation correcte du ppcm est plus que bienvenue *)
(* arnaud: puis enlever les printf *)
let rec euclid a b =
  (* precondition a>=b *)
  let c = a mod b in
  if c = 0 then b
  else euclid b c
let gcd a b =
  if a>=b then euclid a b
  else euclid b a
let lcm p q = p*(q/(gcd p q))
let figurerules ~label ~caption (l:block list) =
  (*arnaud: separation entre les blocks ?*)
  let widths = List.map (fun (_,a,_) -> List.length a) l in
  let lcm = List.fold_left lcm 1 widths in
  Format.printf "lcm: %i\n" lcm;
  let title_line x = array_line ~layout:[lcm,`C]~sep:(`Mm 3.) [textsc x] in
  let block a bs =
    let n = List.length a in
    let w = lcm/n in
    let layout = List.map (fun x -> w,(x:>[alignment|`I])) a in
    (*array_line ~layout ~sep:(`Mm 15.) b*)
    Format.printf "w: %i\n" w;
    Format.printf "length layout: %i\n" (List.length layout);
    List.map (fun (sep,b) -> array_line ~layout ?sep b) bs
  in
  let l =
    List.flatten (List.map (fun (t,a,b) -> title_line t :: block a b ) l)
  in
  let a =
    let rec mk n =
      if n = 0 then []
      else `C::(mk (n-1))
    in
    mk lcm
  in
  Format.printf "length a: %i\n" (List.length a);
  figure ~label ~caption ~pos:[`T;`P] begin
    array a l
  end

let tensor = otimes
let parr = command "parr" ~packages:["cmll",""] [] M
let larrow = command "multimap" ~packages:["amssymb",""] [] M
let one = mode M (text"1")
let bottom = bot
let plus = oplus
let withc = mode M (text" \\& ")
let top = Latex.top
let zero = mode M (text"0")
let idrule = mode T(text"id")
let iddup = mode T (text"id'")
let cutrule = mode T(text"cut")
let iota1rule = mode T(plus^^text"l")
let iota2rule = mode T(plus^^text"r")
let apprule = mode T(text"app")
let recordrule = mode T(text"record")
let pi1rule = index pi (mode M (text"1"))
let pi2rule = index pi (mode M (text"2"))
let bangrule = mode M (text"!")
let whynotrule = mode M (text"?")
let positiveshift = uparrow
let negativeshift = downarrow

let lambdap p = mode M (lambda^^p)
let cutp p = mode M (cutrule^^text"\\,"^^p)
let mup p = mode M (mu^^p)

(*** Wrapper around existing commands ***)

let bigdelim = text"\\setlength\\delimitershortfall{-0.1pt}"
let smalldelim = text"\\setlength\\delimitershortfall{2pt}"

type delimsize =
| Small
| Big
let set_delimsize = function
| Small -> smalldelim
| Big -> bigdelim

let ambiantdelim = Latex.variable Small
let scope_delim s x =
  get ambiantdelim (fun init ->
   set ambiantdelim s ^^
   x
   ^^ set ambiantdelim init)

let declareambiantdelim =
  get ambiantdelim set_delimsize

(* let displaymath x = Latex.displaymath (scope_delim Big x) *)
let displaymath x = Latex.displaymath (scope_delim Big (declareambiantdelim ^^ x))

let just_left d x =
  get ambiantdelim begin function
    | Small -> just_left d x
    | _ -> mode T (smalldelim ^^ mode M (just_left d (mode T (declareambiantdelim^^(mode M x)))))
  end

(*** A short module for proof.sty *)
module Infer = struct

  let infer_gen cmd ?label premisses concl =
    let premisses = concat_with_sep premisses (text" & ") in
    let label = match label with None -> None | Some l -> Some (M,l) in
    command cmd ~packages:["proof",""] ?opt:label [ M,concl ; M,premisses ] M

  let rule = infer_gen "infer"
  let derived = infer_gen "infer*"

end

(*** Holes ***)

let citation_needed = small (text"[citation]")


