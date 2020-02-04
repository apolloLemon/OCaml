(*Exercice 4*)
let rec trouver_tous p li =
	match li with
		[] -> []
		| x::r -> if (p x) then x::(trouver_tous p r)
					else (trouver_tous p r)
;;

let ajout n li =
	List.map (fun i -> i+n) li
;;

let rec appartient x l =
	match l with
		[] -> false
		| y::r -> if y=x then true else appartient x r
;;

let appartient_sous_liste x ll =
	trouver_tous (fun l -> appartient x l) ll
;;

let supprime x l =
	trouver_tous (fun i -> i!=x) l
;;

let supprime_sous_liste x ll =
	List.map (fun l -> supprime x l) ll
;;

(*Exercice 5*)
List.fold_left (+) 0 [1;2;3];;

List.fold_left (+) 0 (List.map (fun i -> i*i) [1;2;3]);;

List.fold_left (fun a b -> a+b*b) 0 [1;2;3];;

let rec longueur = function
	[] -> 0
	| x::r -> 1 + longueur r
;;

let lengthsumL ll =
	List.fold_left (+) 0 (List.map longueur ll)
;;

let lengthsumR ll =
	List.fold_right (+) (List.map longueur ll) 0
;;

let nb_occ a l =
	List.fold_left (fun n x -> if x = a then n+1 else n) 0 l
;;