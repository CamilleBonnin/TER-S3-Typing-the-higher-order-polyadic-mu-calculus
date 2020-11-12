open Variance_syntax


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
  | Lambda of var  * formula      (*for higher order*)
  | Application of formula * formula

let rec neg_var (phi : formula) (x : var) : formula =
  match phi with
    | Top -> Top
    | And (phi, psi) -> And (neg_var phi x, neg_var psi x)
    | Neg (phi) -> Neg(neg_var phi x)
    | Diamond (a, phi) -> Diamond (a, neg_var phi x)
    | PreVariable (y) ->  if (String.equal x y) then Neg(phi) else phi
    | Mu (y,tau,phi) -> Mu(y,tau,neg_var phi x)
    | Lambda (y, phi) -> Lambda (y, neg_var phi x)
    | Application (phi,psi) -> Application(neg_var phi x, psi)

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
  | Lambda (x,phi) ->  Lambda (x, desugar phi)
  | Application (phi,psi) -> Application (desugar phi, desugar psi)



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

let rec t_to_string (tau : mu_type) : string =
  match tau with
    | Ground -> "Ground"
    | Arrow (tau1, v, tau2) -> t_to_string tau1 ^ " " ^ v_to_string v ^ " -> " ^ t_to_string tau2
    | Parameter (s) -> s
    | Untypable -> "Untypable"


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
    | Lambda (x, psi) -> "Lambda " ^ x  ^ " : Ground ." ^ sf_to_string psi
    | Application (f, psi) -> sf_to_string f ^ sf_to_string psi

let rec f_to_string (phi : formula) : string =
  match phi with
    | Top -> "T"
    | Diamond (a, psi) -> "<" ^ a ^ "> "  ^ f_to_string psi^""
    | And (psi, chi) -> f_to_string psi ^ " ^ " ^ f_to_string chi
    | Neg (psi) -> "!(" ^ f_to_string psi^")"
    | PreVariable (x) -> x
    | Mu (f, tau, psi) -> "Mu "^ f ^":"^ t_to_string tau ^".("^ f_to_string psi^")"
    | Lambda (x, psi) -> "Lambda " ^ x ^ " : Ground ." ^ f_to_string psi
    | Application (f, psi) -> f_to_string f ^ f_to_string psi


let  ta_to_string (ta : type_assignment) : string =
  f_to_string ta.phi ^" ^ "^ v_to_string ta.variance ^" : "^ t_to_string ta.tau


let rec te_to_string (gamma : typing_environment) : string =
  match gamma with
    | [] -> ""
    | ta::g -> ta_to_string ta ^ "\n" ^ te_to_string g

(** Creer une liste des variables libres d'une formule **)

(* Verifie si une variable x est dans une liste de variables lvar *)
let rec is_in_list_var (lvar : var list) (x : var) : bool =
  match lvar with
    | [] -> false
    | v::rest -> if v = x then true else is_in_list_var rest x
    (*| v::rest -> is_in_list_var rest x*)

(* Ajoute la variable x a la liste de variables lvar si elle n'y est pas deja *)
let rec add_var_to_list (lvar : var list) (x : var) : var list =
  match (is_in_list_var lvar x) with
    | true -> lvar
    | false -> x::lvar

(* Utilitaire pour supprimer la variable x de la liste de variables lvar *)
let rec util_supr_var_to_list (lvar : var list) (debut : var list) (x : var) : var list =
  match lvar with
    | [] -> List.rev debut
    | v::rest -> if v = x then (List.rev debut) @ rest else util_supr_var_to_list rest (v::debut) x
    (*| v::rest -> util_supr_var_to_list rest (v::debut) x *)

(* Supprime la variable x de la liste de variables lvar *)
let supr_var_to_list (lvar : var list) (x : var) : var list =
  match (is_in_list_var lvar x) with
    | false -> lvar
    | true -> util_supr_var_to_list lvar [] x

(* Fusionne les listes de variables lvar et lvar2 sans creer de doublons *)
let rec concat_lists (lvar : var list) (lvar2 : var list) : var list =
  match lvar with
    | [] -> lvar2
    | v::rest -> concat_lists rest (add_var_to_list lvar2 v)

(* Renvoie lvar sans les variables presentes dans la liste de variables supr *)
let rec supr_list_from_list (lvar : var list) (supr : var list) : var list =
  match supr with
  | [] -> lvar
  | v::rest -> supr_list_from_list (supr_var_to_list lvar v) rest

(* Utilitaire pour renvoyer la liste des variables de la formule phi *)
let rec rec_f_variables (phi : formula) (lvar : var list) : var list =
  match phi with
  | Top -> lvar
  | And (phi2, psi) -> concat_lists (rec_f_variables phi2 lvar) (rec_f_variables psi lvar)
  | Neg (phi2) -> rec_f_variables phi2 lvar
  | Diamond (a, phi2) -> rec_f_variables phi2 lvar
  | PreVariable (y) -> (y::lvar)
  | Mu (y,tau,phi2) -> rec_f_variables phi2 (y::lvar)
  | Lambda (y, phi2) -> rec_f_variables phi2 (y::lvar)
  | Application (phi2,psi) -> concat_lists (rec_f_variables phi2 lvar) (rec_f_variables psi lvar)

(* Renvoie la liste des variables de la formule phi *)
let f_variables (phi : formula) : var list =
  rec_f_variables phi []

(* Utilitaire pour renvoyer la liste des variables liees de la formule phi *)
let rec rec_f_variables_not_free (phi : formula) (lvar : var list) : var list =
  match phi with
  | Top -> lvar
  | And (phi2, psi) -> concat_lists (rec_f_variables_not_free phi2 lvar) (rec_f_variables_not_free psi lvar)
  | Neg (phi2) -> rec_f_variables_not_free phi2 lvar
  | Diamond (a, phi2) -> rec_f_variables_not_free phi2 lvar
  | PreVariable (y) -> lvar
  | Mu (y,tau,phi2) -> rec_f_variables_not_free phi2 (y::lvar)
  | Lambda (y, phi2) -> rec_f_variables_not_free phi2 (y::lvar)
  | Application (phi2,psi) -> concat_lists (rec_f_variables_not_free phi2 lvar) (rec_f_variables_not_free psi lvar)

(* Renvoie la liste des variables liees de la formule phi *)
let f_variables_not_free (phi : formula) : var list =
  rec_f_variables_not_free phi []

(* Renvoie la liste des variables libres de la formule phi *)
let f_free_variables (phi : formula) : var list  =
  supr_list_from_list (f_variables phi) (f_variables_not_free phi)

let () = print_string (String.concat ";" (f_free_variables (Lambda ("x",PreVariable "x"))))  
