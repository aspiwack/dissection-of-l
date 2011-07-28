(*************************************************************************)
(*                                                                                                                            *)
(*                          General purpose melt ocamlbuild plugin                                 *)
(*                                                                                                                            *)
(*************************************************************************)


open Ocamlbuild_plugin
open Command

(*** Mode for Mlpost ***)
let mode = `Cairo
let mode = match mode with
  | `Mps -> A"-mps"
  | `Cairo -> A"-cairo"

(*** List of plugins to metlpp ***)
let plugins = []

(*** List of extra dependencies on the .tex file compilation ***)
let texdeps = []

(*** Global constants ***)

 let ocamlfind_query pkg =
   let cmd = Printf.sprintf "ocamlfind query %s" (Filename.quote pkg) in
   Ocamlbuild_pack.My_unix.run_and_open cmd (fun ic ->
     Ocamlbuild_pack.Log.dprintf 5 "Getting Ocaml directory from command %s" cmd;
     input_line ic)

 let meltpp arg out = 
   let meltpp = P "meltpp" in
   let meltpp = S [meltpp ; Ocamlbuild_pack.Tools.flags_of_pathname arg ] in
   let opn m = S [A "-open" ; A m] in
   let add_open = S [ opn "Latex" ; opn "Melt" ] in
   let dir d = S [ A"-P" ; P d ] in
   let add_dirs = S [ dir "util" ] in
   Cmd (S [meltpp ; add_dirs ; add_open ; A "-o" ; Px out ; P arg ])

 let dypgen arg =
   let dypgen = P "dypgen" in
   let dypgen = 
     S [dypgen ; A"--no-mli"; Ocamlbuild_pack.Tools.flags_of_pathname arg ] 
   in
   Cmd (S [dypgen ; Px arg ])

(* working around ocamlbuild's bugs *)
module Myocamlbuild = struct

  open Pathname.Operators
  open Tags.Operators
  open Command

  let menhir mly env build =
    let mly = env mly in
    let menhir = if !Options.ocamlyacc = N then V"MENHIR" else !Options.ocamlyacc in
    Cmd(S[menhir;
          A"--ocamlc"; Quote(S[!Options.ocamlc; T(tags_of_pathname mly++"ocaml"++"compile")]);
          T(tags_of_pathname mly++"ocaml"++"parser"++"menhir");
          A"--infer"; Px mly])
end

(*** Dispatch ***)

let _ = dispatch begin function

  (*c Here one can change the default value of options, they can still be updated by a command line option. *)
  | Before_options -> ()

  (*c Here one can change the final value of options. *)
  | After_options -> ()

      (*c This avoids the creation of symbolic links to the build directory. *)

  (*c This hook is called before the hygiene phase.
      This phase also serve as collecting all the information about the
      source tree. *)
  | Before_hygiene -> ()

      (*c Here you can dynamically tag some files or directories. *)

  (*c One can also do things after the hygiene step. *)
  | After_hygiene -> ()

  (*c One can setup rules before the standard ones but that's not recommended. *)
  | Before_rules -> ()

  (*c Here one can add or override new rules *)
  | After_rules -> 
      (*** Interpretation of Tags ***)
      flag ["melt";"show"] (A "-show");
      flag ["melt";"moche"] (A "-moche");
      flag ["melt";"intro"]  (A "-intro" );
      flag ["melt";"part1"] (A "-part1");
      flag ["melt";"part2"] (A "-part2");
      flag ["melt";"part3"] (A "-part3");

      (*** rules ***)
      rule "ocamlopt: cmxa -> cmxs"
        ~prod:"%.cmxs"
        ~dep:"%.cmxa"
        begin fun env _ ->
          let cmxs = Px (env "%.cmxs") and cmxa = P (env "%.cmxa") in
          Cmd (S [!Options.ocamlopt;A"-linkall";A"-shared";A"-o";cmxs;cmxa])
        end;

       rule "meltpp: mlt -> ml"
	~prod:"%.ml"
	~deps:(["%.mlt"]@plugins)
	begin fun env build ->
	  let src = env "%.mlt" in
	  let prod = env "%.ml" in
	  meltpp src prod
	end;

	rule "meltrun: native -> tex"
	  ~prods:["%.tex"]
	  ~deps:["%.native"]
	  begin fun env build ->
	    let gen = env "./%.native" in
	    let tags = tags_of_pathname gen in
	    let date x = S[Sh "date >> ../melt.log ;";x] in
	    let time x = S[
	      Sh "/usr/bin/time -ao ../melt.log -f \"User %Us\\nReal %es\\n\"" ;
	      x
	    ]
	    in
	    Cmd (date (
		   time (S[
		     Sh gen; mode;
		     T(tags++"melt");(*A"-depends" ;*) Sh "> /dev/null"
		   ])
		))
	  end;

(*
	rule "meltrun: byte -> tex"
	  ~prods:["%.tex"]
	  ~deps:["%.byte"]
	  begin fun env build ->
	    let gen = env "./%.native" in
	    Cmd (S[Sh gen; mode; Sh "> /dev/null"])
	  end;
*)

	rule "meltrun: native -> spiwack.tex"
	  ~prods:["%.spiwack.tex"]
	  ~deps:(["%.native"]@texdeps)
	       (*texdeps here, since they are used by mlpost | doesn't work yet*)
	  begin fun env build ->
	    let gen = env "./%.native" in
	    let tags = tags_of_pathname gen in
	    Cmd (S[Sh gen; mode; T(tags++"melt"++"show"); Sh "> /dev/null"])
	  end;

	rule "pdflatex: tex -> aux"
	  ~prods:["%.aux";"%.idx"]
	  ~deps:(["%.tex"]@texdeps)
	       (* texdeps here, because if they change, the pdf should change
		  as well *)
	  begin fun env build ->
	    let src = env "%.tex" in
	    let src_stripped = env "%" in
	    let latex_c =
	      Cmd(S [ P "pdflatex" ;
		      A "-halt-on-error" ;
		      Px src ; Sh "> /dev/null"])
	    in
	    let bibtex_c =
	      Cmd(S [P "bibtex" ; Px src_stripped ; Sh "> /dev/null || /bin/true"])
	    in
	    Seq [ latex_c ; bibtex_c ; latex_c ]
	  end;

	rule "makeindex: idx -> ind"
	  ~prod:"%.ind"
	  ~deps:["%.idx"]
	  begin fun env build ->
	    let src = env "%.idx" in
	    Cmd(S [ P "makeindex" ;
                     Px src ; Sh "> /dev/null || /bin/true"])
	  end;

	rule "pdflatex: aux -> pdf"
	  ~prod:"%.pdf"
	  ~deps:(["%.aux";"%.ind"]@texdeps)
	       (* texdeps here, because if they change, the pdf should change
		  as well *)
	  begin fun env build ->
	    let src = env "%.tex" in
	    (*let src_stripped = env "%" in*)
	    let latex_c = Cmd(S [ P "pdflatex" ;
                     A "-halt-on-error" ; A "-interaction=batchmode" ;
                     Px src ; Sh "> /dev/null"])
	    in
	    (*let bibtex_c =
	      Cmd(S [P "bibtex" ; Px src_stripped ; Sh "> /dev/null"])
	    in
	    Seq [ bibtex_c ; latex_c ; latex_c ]*)
	    latex_c
	  end;

	rule "dypgen: dyp -> ml"
	  ~prods:["%.ml"]
	  ~deps:["%.dyp"]
         begin fun env build ->
	   let src = env "%.dyp" in
	   dypgen src
	 end;

	rule "Fix bib"
	  ~prods:["%-fix.bib"]
	  ~deps:["%.bib"]
         begin fun env build ->
	   let src = env "%.bib" in
	   let tgt = env "%-fix.bib" in
	   Cmd (S[Sh "sed \"s/~{}/~/g\""; Px src; Sh">"; P tgt])
	 end;

          (*** Libs ***)
          let bitstring = ocamlfind_query "bitstring" in 
	  ocaml_lib ~extern:true
	                   ~dir:bitstring
	                   "bitstring" ;

	  ocaml_lib ~extern:true
	                   ~dir:"+cairo"
	                   "cairo" ;

	  let mlpostdir = ocamlfind_query "mlpost" in
	  ocaml_lib ~extern:true
	                   ~tag_name:"use_mlpost"
	                   ~dir:mlpostdir
	                    "mlpost" ;


	  let meltdir = ocamlfind_query "melt" in
	  ocaml_lib ~extern:true
	                   ~tag_name:"use_melt"
	                   ~dir:meltdir
	                   "melt" ;
      
	  let ulex_dir = ocamlfind_query "ulex" in
	  ocaml_lib ~extern:true 
	                  ~tag_name:"use_ulex"
                           ~dir:ulex_dir 
                           "ulexing";
	  flag ["ocaml"; "pp"; "use_ulex.syntax"] &
	    S[A "-I"; P ulex_dir; P "pa_ulex.cma"] ;

      (* workaround ocamlbuild's menhir bugs *)
	    rule "ocaml: menhir (workaround)"
	      ~prods:["%.ml"; "%.mli"]
	      ~deps:["%.mly"; "%.mly.depends"]
	      ~insert:(`before "ocaml: menhir")
	      (Myocamlbuild.menhir "%.mly");


      (*c Rules can be declared by a call of the form
          [rule name ~prod ~dep action].
          The first argument is the name of the rule.
          [~prod:string] specifies the product of the rule.
          Note that [~prods:string list] also exists.
          [~dep] and [~deps] are for dependencies *)
        
end
