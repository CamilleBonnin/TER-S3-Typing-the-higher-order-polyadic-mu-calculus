(* Represente une variance *)
type variance  =
  | None
  | Any
  | Monotone 
  | Join
  | Meet
  | Additive
  (* duals of previous variances*)
  | Antitone
  | NJoin 
  | NMeet
  | NAdditive

(* Transforme une chaine de caractere en variance *)
val variance_from_string : string -> variance

(* Transforme une varaiance en chaine de caracteres *)
val v_to_string : variance -> string

(* Fait la liste de toutes les variances *)
val all_variances : variance list