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
  | Lambda of var * variance * mu_type * sugared_formula      (*for higher order*)
  | Application of sugared_formula * sugared_formula

(* Represente une formule sans le sucre syntaxique *)
type formula =
  | Top
  | And of formula * formula
  | Neg of formula
  | Diamond of var * (* * int for polyadic * *) formula
  | PreVariable of var
  | Mu of var * mu_type * formula (* smallest fix point *)
  | Lambda of var * variance * mu_type * formula      (*for higher order*)
  | Application of formula * formula

(* Represente un environnement de typage incomplet (delta). *)
type incomplete_typing_environment = (var * mu_type) list

(* Represente un environnement de typage complet (gamma). *)
type complete_typing_environment = (var * (variance * mu_type)) list

(* Represente le resultat de type(delta, formule) *)
type my_assignment = (complete_typing_environment * mu_type)

(* Utilisee dans desugar *)
let rec neg_var (phi : formula) (x : var) : formula =
  match phi with
    | Top -> Top
    | And (phi, psi) -> And (neg_var phi x, neg_var psi x)
    | Neg (phi) -> Neg(neg_var phi x)
    | Diamond (a, phi) -> Diamond (a, neg_var phi x)
    | PreVariable (y) ->  if (String.equal x y) then Neg(phi) else phi
    | Mu (y,tau,phi) -> Mu(y,tau,neg_var phi x)
    | Lambda (y, var, tau, phi) -> Lambda (y, var, tau, neg_var phi x)
    | Application (phi,psi) -> Application(neg_var phi x, psi)

(* Enleve le sucre syntaxique d'une formule *)
let rec desugar (sf : sugared_formula) : formula =
  match sf with
  | Top -> Top
  | Bottom -> Neg Top
  | And (phi, psi) -> And (desugar phi, desugar psi)
  | Or (phi,psi) -> Neg (And (Neg(desugar phi), Neg(desugar psi)))
  | Neg (phi) -> Neg(desugar phi)
  | Diamond (a,phi) -> Diamond (a, (desugar phi))
  | Box (a,phi) -> Neg (Diamond (a, Neg(desugar phi)))
  | PreVariable (x) -> PreVariable(x)
  | Mu (x,t,phi) -> Mu (x, t, desugar phi)
  | Nu (x,t,phi) -> Neg (Mu (x, t, Neg( neg_var (desugar phi) x)))
  | Lambda (x, var, t, phi) ->  Lambda (x, var, t, desugar phi)
  | Application (phi,psi) -> Application (desugar phi, desugar psi)

(* Transforme un type en chaine de caracteres *)
let rec t_to_string (tau : mu_type) : string =
  match tau with
    | Ground -> "Ground"
    | Arrow (tau1, v, tau2) -> "(" ^ t_to_string tau1 ^ " " ^ v_to_string v ^ " -> " ^ t_to_string tau2 ^ ")"
    | Untypable -> "Untypable"

(* Transforme une formule avec sucre syntaxique en chaine de caracteres *)
let rec sf_to_string (phi : sugared_formula) : string =
  match phi with
    | Top -> "T"
    | Bottom -> "Bot"
    | Diamond (a, psi) -> " <" ^ a ^ "> "  ^ sf_to_string psi
    | Box (a, psi) -> " [" ^ a ^ "] "  ^ sf_to_string psi
    | And (psi, chi) -> sf_to_string psi ^ " ^ " ^ sf_to_string chi
    | Or (psi, chi) -> sf_to_string psi ^ " U " ^ sf_to_string chi
    | Neg (psi) -> "! " ^ sf_to_string psi
    | PreVariable (x) -> x
    | Mu (f, tau, psi) -> "Mu "^ f ^":"^ t_to_string tau ^".("^ sf_to_string psi^")"
    | Nu (f, tau, psi) -> "Nu "^ f ^":"^ t_to_string tau ^"."^ sf_to_string psi
    | Lambda (x, var, tau, psi) -> "Lambda " ^ x  ^ "^" ^ v_to_string var ^":"^ t_to_string tau ^"."^ sf_to_string psi
    | Application (f, psi) -> sf_to_string f ^ sf_to_string psi

(* Transforme une formule sans sucre syntaxique en chaine de caracteres *)
let rec f_to_string (phi : formula) : string =
  match phi with
    | Top -> "T"
    | Diamond (a, psi) -> "<" ^ a ^ "> "  ^ f_to_string psi^""
    | And (psi, chi) -> f_to_string psi ^ " ^ " ^ f_to_string chi
    | Neg (psi) -> "!(" ^ f_to_string psi^")"
    | PreVariable (x) -> x
    | Mu (f, tau, psi) -> "Mu "^ f ^":"^ t_to_string tau ^".("^ f_to_string psi ^")"
    | Lambda (x, var, tau, psi) -> "Lambda " ^ x  ^ "^" ^ v_to_string var ^":"^ t_to_string tau ^"."^ f_to_string psi
    | Application (f, psi) -> f_to_string f ^ f_to_string psi  

(* Transforme un environnement de typage incomplet en chaine de caracteres *)
let rec inc_typ_env_to_string (delta : incomplete_typing_environment) : string =
  match delta with
    | [] -> ""
    | (v, t)::rest -> "(" ^ v ^ " : " ^ t_to_string t ^ ") \n" ^ inc_typ_env_to_string rest

(* Transforme un environnement de typage complet en chaine de caracteres *)
let rec comp_typ_env_to_string (gamma : complete_typing_environment) : string =
  match gamma with
    | [] -> ""
    | (v, (va, tau))::rest -> "(" ^ v ^ " : (" ^ v_to_string va ^ ", " ^ t_to_string tau ^ ")) \n" ^ comp_typ_env_to_string rest

(* Transforme un assignement en chaine de caracteres *)
let my_assig_to_string (assign : my_assignment) : string =
  "[ " ^ (comp_typ_env_to_string (fst assign)) ^ " type : " ^ (t_to_string (snd assign)) ^ " ]\n"

(** Creer une liste associative des variables libres d'une formule qui compte leur nombre 
d'occurences dans la formule. **)

(* Transforme une liste associative de variables en chaine de caracteres *)
let rec assoc_list_var_to_string (lvar : (var * int) list) : string =
  match lvar with
    | [] -> ""
    | (v, n)::rest -> "(" ^ v ^ ", " ^ string_of_int n ^ ") \n" ^ assoc_list_var_to_string rest

(* Ajoute la variable x a la liste de variables associative lvar *)
let  add_var_to_list (lvar : (var * int) list) (x : var) : (var * int) list =
  match (List.mem_assoc x lvar) with
    | true -> (x, ((List.assoc x lvar) + 1))::(List.remove_assoc x lvar)
    | false -> (x, 1)::lvar

(* Fusionne les listes de variables lvar et lvar2 sans creer de doublons *)
let rec concat_lists (lvar : (var * int) list) (lvar2 : (var * int) list) : (var * int) list =
  match lvar with
    | [] -> lvar2
    | (v, n)::rest -> match (List.mem_assoc v lvar2) with
      | false -> (v, n)::(concat_lists rest lvar2)
      | true -> (v, n + (List.assoc v lvar2))::(concat_lists rest (List.remove_assoc v lvar2))

(* Enleve les elements de la liste de variables toRemove de la liste associative de variables lvar *)
let rec remove_list (lvar : (var * int) list) (toRemove : var list) : (var * int) list =
  match toRemove with
  | [] -> lvar
  | v::rest -> remove_list (List.remove_assoc v lvar) rest

(* Utilitaire pour renvoyer la liste associative des variables libres de la formule phi *)
let rec rec_f_free_variables (phi : formula) (lvar : (var * int) list) (toRemove : var list) 
  : (var * int) list =
  match phi with
  | Top -> lvar 
  | And (phi2, psi) -> concat_lists (rec_f_free_variables phi2 lvar toRemove) 
                                      (rec_f_free_variables psi lvar toRemove)
                        
  | Neg (phi2) -> rec_f_free_variables phi2 lvar toRemove
  | Diamond (a, phi2) -> rec_f_free_variables phi2 lvar toRemove
  | PreVariable (y) -> add_var_to_list lvar y
  | Mu (y,tau,phi2) -> remove_list (rec_f_free_variables phi2 lvar (y::toRemove)) (y::toRemove)
  | Lambda (y, var, tau, phi2) -> remove_list (rec_f_free_variables phi2 lvar (y::toRemove)) (y::toRemove)
  | Application (phi2,psi) -> concat_lists (rec_f_free_variables phi2 lvar toRemove) 
                                              (rec_f_free_variables psi lvar toRemove)

(* Renvoie la liste associative des variables libres de la formule phi *)
let f_free_variables (phi : formula) : (var * int) list =
  rec_f_free_variables phi [] []

(*
(* Test*)
let () = print_string (assoc_list_var_to_string 
  (f_free_variables (And((Lambda("x", Ground, PreVariable("x"))),PreVariable("x")))))
*)
