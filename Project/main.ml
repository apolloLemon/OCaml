(* Code for Ocaml project *)
(* ocamlc -o project main.ml *)

let graphe1 = [(1,[6;7;8]) ; (2,[1;4]) ; (3, [2]) ; (4, [3;5]) ; (5, [1]) ;
				(6, [5;7]) ; (7, []) ; (8, [6;7])]
;;

let rec liste_sommets g =
	match g with
		[] -> []
		| x::r -> (fst x)::(liste_sommets r);;

let rec liste_succ n g =
	match g with
		[] -> failwith "Can't find vertex."
		| (s, succ)::r -> if not (n=s) then liste_succ n r
					else
						succ
;;

let rec findPred n g =
	match g with
		[] -> []
		| (s,succ)::r -> if List.exists (fun p -> if p=n then true else false) succ
							then s::findPred n r
							else findPred n r
;;

let inverse_graphe g =
	List.fold_left (fun a (s,succ) -> a@[(s, findPred s g)]) [] g
;;

let rec newFold_left f a = function
	| [] -> a
	| h::t -> newFold_left f (f a h) t
;;

let rec betterFold_left f a b = function
	| [] -> (a,b)
	| h::t -> betterFold_left f arg (f a a h) t
;;

let parcours_prof graph =
    let rec rExplorer visited node =
        if not (List.mem node visited) then
            let s = liste_succ node graph in
                List.fold_left rExplorer (node::visited) s
        else
            visited
    in List.fold_left rExplorer [] (liste_sommets graph)
;;
(*
let parcours_prof graph =
	let rec rExplorer visited suffixe succ =
		if not (List.mem succ visited) then
			let s = liste_succ succ graph in
				betterFold_left rExplorer (succ::visited) suffixe s
		else
			visited
	in betterFold_left rExplorer [] [] (liste_sommets graph)
;;
*)
parcours_prof graphe1;;