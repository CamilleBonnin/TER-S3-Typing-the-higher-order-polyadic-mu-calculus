open Variance_syntax2


type var = string

type mu_type =
  | Ground
  | Arrow of mu_type * variance * mu_type
  | Parameter of string
  | Untypable

type sugared_formula =
  | Top
  | Bottom
  | And of sugared_formula * sugared_formula
  | Or of sugared_formula * sugared_formula
  | Neg of sugared_formula
  | Diamond of var * (* * int for polyadic * *) sugared_formula
  | Box of var * (* * int for polyadic * *) sugared_formula
  | PreVariable of var
  | Mu of var * mu_type * sugared_formula (* smallest fix point *)
  | Nu of var * mu_type * sugared_formula (* greatest fix point *) 
  | Lambda of var * sugared_formula      (*for higher order*) 
  | Application of sugared_formula * sugared_formula


type formula =
  | Top
  | And of formula * formula
  | Neg of formula
  | Diamond of var * (* * int for polyadic * *) formula
  | PreVariable of var
  | Mu of var * mu_type * formula (* smallest fix point *)
  | Lambda of var * formula      (*for higher order*) 
  | Application of formula * formula


type type_assignment = {
  phi : formula;
  variance : variance;
  tau : mu_type
  }

type typing_environment =
  type_assignment list

type type_judgment = {
  gamma : typing_environment;
  phi : formula;
  tau : mu_type
}

val neg_var : formula -> var -> formula

val desugar : sugared_formula -> formula

val t_to_string : mu_type -> string 

val te_to_string : typing_environment -> string

val sf_to_string : sugared_formula -> string

val f_to_string : formula -> string

(** Creer une liste associative des variables libres d'une formule qui compte leur nombre 
d'occurences dans la formule. **)

(* Transforme une liste associative de vriables en chaine de caracteres*)
val assoc_list_var_to_string : (var * int) list -> string

(* Ajoute la variable x a la liste de variables associative lvar *)
val add_var_to_list : (var * int) list -> var -> (var * int) list

(* Fusionne les listes de variables lvar et lvar2 sans creer de doublons *)
val concat_lists : (var * int) list -> (var * int) list -> (var * int) list

(* Enleve les elements de la liste de variables toRemove de la liste associative de variables lvar *)
val remove_list : (var * int) list -> var list -> (var * int) list 

(* Utilitaire pour renvoyer la liste associative des variables libresde la formule phi *)
val rec_f_free_variables : formula -> (var * int) list -> var list -> (var * int) list 

(* Renvoie la liste associative des variables libres de la formule phi *)
val f_free_variables : formula -> (var * int) list
