
val concat_with_sep : Latex.t list -> Latex.t -> Latex.t

type author = {
  name : Latex.t;
  address : Latex.t;
  email : Latex.t option;
}

val document :
  title:Latex.t ->
  ?short_title:Latex.t ->
  authors:author list ->
  keywords:Latex.t list ->
  acmclass:Latex.t list ->
  abstract:Latex.t ->
  prelude:Latex.t ->
  packages:(Latex.t * Latex.t) list ->
  Latex.t
  -> Latex.t

(*** A short module for proof.sty *)
module Infer : sig

  (** A single deduction step *)
  val rule : ?label:Latex.t -> Latex.t list -> Latex.t -> Latex.t
  (** Multiple deduction steps *)
  val derived : ?label:Latex.t -> Latex.t list -> Latex.t -> Latex.t

end
