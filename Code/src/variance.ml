open Variance_syntax
open Mu_calculus_syntax

(* Returns true if v1 is smaller or equal than v2 *)
let rec smaller (v1 : variance) (v2 : variance) : bool =
	match (v1,v2) with
		| (v1,v2) when v1 == v2 -> true 
		| (Any, _) -> true
		| (_, None) -> true
		| (Monotone, v2) -> if (v2 == Join || v2 == Meet) then true else (smaller Join v2)
		| (Antitone, v2)  -> if (v2 == NJoin || v2 == NMeet) then true else (smaller NJoin v2)
		| (v1, Additive) when (v1 == Join || v1 == Meet) -> true
		| (v1, NAdditive) when (v1 == NJoin || v1 == NMeet) -> true
		| (_, _) -> false

(* Calcule le dual de v *)
let dual (v : variance) : variance =
	match v with	
 		| Join -> Meet
  		| Meet -> Join
  		| NJoin -> NMeet
  		| NMeet -> NJoin
  		| _ -> v (* ? undefined cases ? *)

(* Calcule la negation de v *)
let not_v (v : variance) : variance =
	match v with
		| Monotone -> Antitone
		| Antitone -> Monotone
		| Join -> NJoin
		| NJoin -> Join
		| Meet -> NMeet
		| NMeet -> Meet
		| Additive -> NAdditive
		| NAdditive -> Additive
		| _ -> v (* ? undefined cases ? *)

(* Regarde si v est dans la partie additive de l'arbre des variances *)
let in_additive (v : variance) : bool =
	match v with
		| Monotone | Join | Meet | Additive -> true
		| _ -> false

(* Regarde si v est dans la partie negation additive de l'arbre des variances *)
let in_Nadditive (v : variance) : bool =
	in_additive (not_v v)

(* Should only be called between two variances in {Monotone; Join ; Meet ; Additive}
@raise Failure if v1 ^ v2 isn't properly defined 
Utilisee dans composition *)
let inter (v1 : variance) (v2 : variance) : variance =
	match (v1, v2) with
		| (v1,v2) when v1 == v2 -> v1
		| (Additive, v) | (v, Additive) -> v
		| (v1,v2) when not ((in_additive v1) && (in_additive v2)) -> failwith ("Error in variances intersection : "^(v_to_string v1)^" ^ "^(v_to_string v2)^" is not defined.")
		| _,_ -> Monotone 

(* Calcule la composition de deux variances
@raise Failure if v1 o v2 isn't properly defined *)
let composition (v1 : variance) (v2 : variance) : variance =
	match (v1,v2) with 
		| (None, _) |(_, None) -> None
		| (Any, _) |(_, Any) -> Any
		| (v1, v2) when (in_additive v1) && (in_additive v2) -> inter v1 v2
		| (v1, v2) when (in_Nadditive v1) && (in_additive v2) -> not_v  (inter (not_v  v1) v2)
		| (v1, v2) when (in_additive v1) && (in_Nadditive v2) -> not_v  (inter (dual v1) (not_v  v2))
		| (v1, v2) when (in_Nadditive v1) && (in_Nadditive v2) -> inter (dual (not_v  v1)) (not_v  v2)
		| _ ->  failwith ("Error in variances composition : this case should never occur !")

(* Calcule "l'intersection" de deux variances *)
let greatest_smaller_variances  (v1 : variance) (v2 : variance) : variance =
	let rec test (vl : variance list) : variance =
		match vl with
			| [] -> Any
			| v::l when (smaller v v1) && (smaller v v2) -> v
			| v::l -> test l
	in test all_variances

