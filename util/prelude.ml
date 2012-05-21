open Latex



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



(*** A short module for proof.sty *)
module Infer = struct

  let infer_gen cmd ?label premisses concl =
    let premisses = concat_with_sep premisses (text" & ") in
    let label = match label with None -> None | Some l -> Some (M,l) in
    command cmd ~packages:["proof",""] ?opt:label [ M,concl ; M,premisses ] M

  let rule = infer_gen "infer"
  let derived = infer_gen "infer*"

end
