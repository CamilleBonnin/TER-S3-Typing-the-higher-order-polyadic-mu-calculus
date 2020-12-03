open Mu_calculus_syntax
open Variance_syntax

(* Utilitaire pour calculer l'environnement de typage complet gamma = gamma1 /\ gamma2 *)
val util_G1_inter_G2 : complete_typing_environment -> complete_typing_environment -> complete_typing_environment -> complete_typing_environment

(* Calcule l'environnement de typage complet gamma = gamma1 /\ gamma2 *)
val typ_env_G1_inter_G2 : complete_typing_environment -> complete_typing_environment -> complete_typing_environment

(* Utilitaire pour realiser l'operation variance rond gamma *)
val util_rond  : variance -> complete_typing_environment -> complete_typing_environment -> complete_typing_environment 

(* Realiser l'operation variance rond gamma *)
val rond : variance -> complete_typing_environment -> complete_typing_environment

(* Effecte le typage de f *)
val typing : incomplete_typing_environment -> formula -> my_assignment
