open Mu_calculus_syntax
open Variance_syntax
open Variance
open Util

(* a typing environment is smaller or equal than another if they differ only with respects to the variances,
  and all variances from the first typing environment are smaller or equal than the ones for the same variable in the second one *)
let rec smaller_environment (gamma1 : typing_environment) (gamma2 : typing_environment) : bool =
  let rec fetch_bigger (ta : type_assignment) (gamma : typing_environment) : typing_environment option = 
    match gamma with 
      | [] -> None
      | ta2::g2 -> if (ta.phi == ta2.phi) then 
                        (if not((smaller ta.variance  ta2.variance) && (ta.tau == ta2.tau)) then None else Some (g2)) (* leaving out ta2 *)
                   else match (fetch_bigger ta g2) with
                        | Some (g) -> Some(ta::g)
                        | None -> None
  in
  match (gamma1, gamma2) with
    | [], [] -> true
    | [], _ | _, [] -> false
    | ta::g1, _ -> match fetch_bigger ta gamma2 with
                    | Some(g2) -> smaller_environment g1 g2
                    | None -> false

(* adds an assignment to a typing environment and eventually deletes a previous assignment of the same formula *)
let rec replace_type_assignment  (gamma : typing_environment) (ta : type_assignment) : typing_environment = 
  match gamma with
    | [] -> [ta]
    | t::g when (t.phi == ta.phi) -> ta::g 
    | t::g -> t::(replace_type_assignment g ta)

let rec get_assignment (gamma : typing_environment) (phi : formula) : type_assignment option = 
  match gamma with
    | [] -> None
    | t::g when (t.phi == phi) -> Some(t)
    | _::g -> get_assignment g phi

let rec get_variable_assignment (gamma : typing_environment) (x : var) : type_assignment option = 
  match gamma with
    | [] -> None
    | t::g -> (match t.phi with  
                  | PreVariable(y) -> if (String.equal y x) then  Some(t)
                else get_variable_assignment g x
                  | _ -> get_variable_assignment g x)


(* returns gamma' = v o gamma *)
let composition_environment (v : variance) (gamma : typing_environment) : typing_environment =
  List.map (fun  (ta : type_assignment) -> {phi = ta.phi; variance = (composition v ta.variance); tau = ta.tau})  gamma

let inverse_assignment (v : variance) (ta : type_assignment) : type_assignment list = 
  let rec filter_variance (vl : variance list) : type_assignment list =
      match vl with
        | [] -> []
        | v'::l -> {phi = ta.phi; variance = v'; tau = ta.tau}:: filter_variance l
    in filter_variance (inverse_variance_composition v ta.variance)


(* gives all the possible combinations of typing environments st that environment is the inverse of gamma in respect to v *)
let inverse_environment (v : variance) (gamma : typing_environment) : typing_environment list = 
  let rec filter_gamma (gamma : typing_environment) : type_assignment list list =
      match gamma with
        | [] -> []
        | ta::te ->  (inverse_assignment v ta) :: (filter_gamma te) 
  in
  let rec filter_assignments (inv_list : type_assignment list list) : typing_environment list =

      match inv_list with 
        | [] ->  [[]]
        | tal::invl ->   match tal with 
                          | [] -> (filter_assignments invl)
                          | ta::l ->  (cons_list_list ta (filter_assignments invl))@(filter_assignments (l::invl)) 
    in filter_assignments (filter_gamma gamma)

(* returns a list containing all the bigger assignments than ta *)
let bigger_assignment  (ta : type_assignment) : type_assignment list = 
  let rec filter_variance (vl : variance list) : type_assignment list =
      match vl with
        | [] -> []
        | v'::l -> {phi = ta.phi; variance = v'; tau = ta.tau}:: filter_variance l
    in filter_variance (bigger_variances ta.variance)

(* returns a list containing the environments bigger or equal to gamma *)
let  bigger_environments (gamma : typing_environment) : typing_environment list = 
  let rec filter_gamma (gamma : typing_environment) : type_assignment list list =
      match gamma with
        | [] -> []
        | ta::te -> bigger_assignment ta :: (filter_gamma te) 
  in
  let rec filter_assignments (inv_list : type_assignment list list) : typing_environment list =
      match inv_list with 
        | [] -> [[]]
        | tal::invl -> match tal with 
                          | [] -> []
                          | ta::l -> (cons_list_list ta (filter_assignments invl))@(filter_assignments (l::invl))
    in filter_assignments (filter_gamma gamma)

(* returns the negated typing environment of gamma *)
let rec not_e (gamma : typing_environment) : typing_environment =
  match gamma with 
    | [] -> []
    | ta::g -> {phi = ta.phi ; variance = not_v ta.variance; tau = ta.tau}::not_e g

(* Utilitaire pour calculer l'environnement de typage complet gamma = gamma1 /\ gamma2 *)
let rec util_G1_inter_G2 (gamma1 : complete_typing_environment) (gamma2 : complete_typing_environment) 
(gamma : complete_typing_environment) : complete_typing_environment =
  match gamma1 with
    | [] -> gamma @ gamma2 (* Dans Gamma2 mais pas dans Gamma1 *)
    | (v, (va, tau))::rest -> match List.assoc_opt v gamma2 with
        | None -> util_G1_inter_G2 rest gamma2 ((v, (va, tau))::gamma) (* Dans Gamma1 mais pas dans Gamma2 *)
        | Some (va2, tau2) -> if (mu_type_equality tau tau2)
                          then util_G1_inter_G2 rest gamma2 ((v, ((inter va va2), tau))::gamma)
                          else failwith ("Error in G1_inter_G2 : " ^ v ^ " has two different types " ^ (t_to_string tau) ^ " and " ^ (t_to_string tau2))

(* Calcule l'environnement de typage complet gamma = gamma1 /\ gamma2 *)
let typ_env_G1_inter_G2 (gamma1 : complete_typing_environment) (gamma2 : complete_typing_environment) 
: complete_typing_environment =
  util_G1_inter_G2 gamma1 gamma2 []

(* Utilitaire pour realiser l'operation variance rond gamma *)
let rec util_rond (var : variance) (gamma : complete_typing_environment) 
(result : complete_typing_environment) : complete_typing_environment =
  match gamma with 
    | [] -> result
    | (v, (va, tau))::rest -> util_rond var rest ((v, ((composition var va), tau))::result)

(* Realiser l'operation variance rond gamma *)
let rond (var : variance) (gamma : complete_typing_environment) : complete_typing_environment =
  util_rond var gamma []

(* Effecte le typage de f *)
(* Il manque la regle {i <- j} *)
let rec typing (delta : incomplete_typing_environment) (f : formula) : my_assignment =
  match f with
    | Top -> ([], Ground)
    | Diamond (a, psi) -> (match (typing delta psi) with
        | (gamma, Ground) -> ((rond Join gamma), Ground)
        | ( _, (Untypable|Parameter _)) -> failwith ("Error in typing with case Diamond : " ^ (f_to_string psi) ^ " has not type Ground")
        | ( _, Arrow(_, _, _ )) -> failwith ("Error in typing with case Diamond : " ^ (f_to_string psi) ^ " has not type Ground"))
    | And (psi, chi) -> begin
        match typing delta psi,typing delta chi with
        | (gamma1, Ground),(gamma2, Ground) -> ((typ_env_G1_inter_G2 gamma1 gamma2), Ground)
        | _ -> failwith ("Error in typing with case And : " ^ (f_to_string chi) ^ " has not type Ground")
      end
    | Neg (psi) -> (match (typing delta psi) with
        | (gamma, tau) -> ((rond NAdditive gamma) ,tau))
    | PreVariable (x) -> (match (List.assoc_opt x delta) with
        | None -> failwith ("Error in typing with case PreVariable : " ^ x ^ " is not in delta")
        | Some (tau) -> ((List.cons (x, (Additive, tau)) []), tau))
    | Mu (x, tau, psi) -> (match (typing ((x, tau)::delta) psi) with
        | (gamma, omega) -> (match (mu_type_equality tau omega) with
            | false -> failwith ("Error in typing with case Mu : error type tau")
            | true -> (match (List.assoc_opt x gamma) with 
                | None -> failwith ("Error in typing with case Mu : x not in gamma")
                | Some (var, omega2) -> (match (mu_type_equality omega omega2) with
                    | false -> failwith ("Error in typing with case Mu : error type omega2")
                    | true -> (match (in_additive var) with 
                        | false -> failwith ("Error in typing with case Mu : error variance not additive")
                        | true -> ((List.remove_assoc x gamma), tau))))))
    | Lambda (x, psi) -> failwith ("Error in typing with case Lambda : not done yet")
    | Application (f, psi) -> (match (typing delta f) with
        | (gamma1, Arrow (omega, var, tau)) -> (match (typing delta psi) with
            | (gamma2, omega2) -> (match (mu_type_equality omega omega2) with
                | false -> failwith ("Error in typing with case Application : no type compatibility")
                | true -> ((typ_env_G1_inter_G2 gamma1 (rond var gamma2)) ,tau)))
        | (_, (Ground|Untypable)) -> failwith ("Error in typing with case Application : " ^ (f_to_string f) ^ " has not type Arrow")
        | (_, Parameter (_)) -> failwith ("Error in typing with case Application : " ^ (f_to_string f) ^ " has not type Arrow"))
