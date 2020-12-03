open Variance_syntax
open Mu_calculus_syntax

(* Returns true if v1 is smaller or equal than v2 *)
val smaller : variance -> variance -> bool

(* Calcule le dual de v *)
val dual : variance -> variance

(* Calcule la negation de v *)
val not_v : variance -> variance

(* Regarde si v est dans la partie additive de l'arbre des variances *)
val in_additive : variance -> bool

(* Regarde si v est dans la partie negation additive de l'arbre des variances *)
val in_Nadditive : variance -> bool

(* Should only be called between two variances in {Monotone; Join ; Meet ; Additive}
@raise Failure if v1 ^ v2 isn't properly defined 
Utilisee dans composition *)
val inter : variance -> variance -> variance

(* Calcule la composition de deux variances
@raise Failure if v1 o v2 isn't properly defined *)
val composition : variance -> variance -> variance

(* Calcule "l'intersection" de deux variances *)
val greatest_smaller_variances : variance -> variance -> variance
