open Variance_syntax

(* Represente une variable *)
type var = string

(* Represente un type (de base ou fleche), 
le type Untypable peux etre utilise en interne *)
type mu_type =
  | Ground
  | Arrow of mu_type * variance * mu_type
  | Untypable

(* Represente une formule y compris les formules avec du sucre syntaxique *)
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
  | Lambda of var * mu_type * sugared_formula      (*for higher order*)
  | Application of sugared_formula * sugared_formula

(* Represente une formule sans le sucre syntaxique *)
type formula =
  | Top
  | And of formula * formula
  | Neg of formula
  | Diamond of var * (* * int for polyadic * *) formula
  | PreVariable of var
  | Mu of var * mu_type * formula (* smallest fix point *)
  | Lambda of var * mu_type * formula      (*for higher order*)
  | Application of formula * formula
  
(* Represente un environnement de typage incomplet (delta). *)
type incomplete_typing_environment = (var * mu_type) list

(* Represente un environnement de typage complet (gamma). *)
type complete_typing_environment = (var * (variance * mu_type)) list

(* Represente le resultat de type(delta, formule) *)
type my_assignment = (complete_typing_environment * mu_type)

(* Utilisee dans desugar *)
val neg_var : formula -> var -> formula

(* Enleve le sucre syntaxique d'une formule *)
val desugar : sugared_formula -> formula

(* Transforme un type en chaine de caracteres *)
val t_to_string : mu_type -> string 

(* Transforme une formule avec sucre syntaxique en chaine de caracteres *)
val sf_to_string : sugared_formula -> string

(* Transforme une formule sans sucre syntaxique en chaine de caracteres *)
val f_to_string : formula -> string

(* Transforme un environnement de typage incomplet en chaine de caracteres *)
val inc_typ_env_to_string  : incomplete_typing_environment -> string

(* Transforme un environnement de typage complet en chaine de caracteres *)
val comp_typ_env_to_string : complete_typing_environment -> string

(* Transforme un assignement en chaine de caracteres *)
val my_assig_to_string : my_assignment -> string

(** Creer une liste associative des variables libres d'une formule qui compte leur nombre 
d'occurences dans la formule. **)

(* Transforme une liste associative de variables en chaine de caracteres*)
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

