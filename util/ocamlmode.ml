let ocamlmode = 
  Melt.Verbatim.convert
    (fun s -> Latex.(small (textsf (Verbatim.verbatim s))))
