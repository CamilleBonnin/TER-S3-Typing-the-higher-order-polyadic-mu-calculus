open Mu_calculus_syntax
open Variance_syntax
open Variance

(* Utilitaire pour calculer l'environnement de typage complet gamma = gamma1 /\ gamma2 *)
let rec util_G1_inter_G2 (gamma1 : complete_typing_environment) (gamma2 : complete_typing_environment) 
(gamma : complete_typing_environment) : complete_typing_environment =
  match gamma1 with
    | [] -> gamma @ gamma2 (* Dans Gamma2 mais pas dans Gamma1 *)
    | (v, (va, tau))::rest -> match List.assoc_opt v gamma2 with
        | None -> util_G1_inter_G2 rest gamma2 ((v, (va, tau))::gamma) (* Dans Gamma1 mais pas dans Gamma2 *)
        | Some (va2, tau2) -> if (tau = tau2)
                          then util_G1_inter_G2 rest (List.remove_assoc v gamma2) ((v, ((greatest_smaller_variances va va2), tau))::gamma)
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
    | Diamond (a, psi) -> begin 
      match (typing delta psi) with
        | (gamma, Ground) -> ((rond Join gamma), Ground)
        | _ -> failwith ("Error in typing with case Diamond : " ^ (f_to_string psi) ^ " has not type Ground")
      end
    | And (psi, chi) -> begin
      match (typing delta psi), (typing delta chi) with
        | (gamma1, Ground),(gamma2, Ground) -> ((typ_env_G1_inter_G2 gamma1 gamma2), Ground)
        | _ -> failwith ("Error in typing with case And : " ^ (f_to_string chi) ^ " has not type Ground")
      end
    | Neg (psi) -> begin 
      match (typing delta psi) with
        | (gamma, tau) -> ((rond NAdditive gamma) ,tau)
      end
    | PreVariable (x) -> begin 
      match (List.assoc_opt x delta) with
        | None -> failwith ("Error in typing with case PreVariable : " ^ x ^ " is not in delta")
        | Some (tau) -> ((List.cons (x, (Additive, tau)) []), tau)
      end
    | Mu (x, tau, psi) -> begin 
      match (typing ((x, tau)::delta) psi) with
        | (gamma, omega) -> begin 
          match (tau = omega) with
            | false -> failwith ("Error in typing with case Mu : error type tau")
            | true -> begin 
              match (List.assoc_opt x gamma) with 
                | None -> failwith ("Error in typing with case Mu : x not in gamma")
                | Some (var, omega2) -> begin 
                  match (omega = omega2) with
                    | false -> failwith ("Error in typing with case Mu : error type omega2")
                    | true -> begin
                      match (in_additive var) with 
                        | false -> failwith ("Error in typing with case Mu : error variance not additive")
                        | true -> ((List.remove_assoc x gamma), tau)
                      end
                  end
              end
          end
      end
    | Lambda (x, omega, psi) -> begin
      match (typing ((x, omega)::delta) psi) with
        | (gamma, tau) -> begin
          match (List.assoc_opt x gamma) with 
          | None -> failwith ("Error in typing with case Lambda : x not in gamma")
          | Some (var, omegaPrime) -> ((List.remove_assoc x gamma), Arrow (omegaPrime, var, tau))
        end
      end
    | Application (f, psi) -> begin 
      match (typing delta f), (typing delta psi) with
        | (gamma1, Arrow (omega, var, tau)), (gamma2, omega2) -> begin 
          match (omega = omega2) with
            | false -> failwith ("Error in typing with case Application : no type compatibility")
            | true -> ((typ_env_G1_inter_G2 gamma1 (rond var gamma2)) ,tau)
          end
        | _ -> failwith ("Error in typing with case Application : " ^ (f_to_string f) ^ " has not type Arrow")
      end

(* Test*)
let () = print_string (my_assig_to_string 
  (typing (("x", Ground)::[]) (And((Lambda("x", Ground, PreVariable("x"))),PreVariable("x")))))