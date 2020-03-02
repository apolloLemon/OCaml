(* Code for Ocaml project *)
(* ocamlc -o project main.ml *)

let rec liste_sommets g =
	match g with
		[] -> []
		| x::r -> (fst x)::(liste_sommets r);;

let rec liste_succ g n =
	match g with
		[] -> failwith "Can't find vertex."
		| (s, succ)::r -> if not (n=s) then liste_succ r n
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

let dPLog s (suffixe, visited) =
	(s::suffixe, visited)
;;

let rec depthProbe s graph (suffixe, visited) =
	if (List.mem s visited) then
		(suffixe, visited)
	else
		dPLog s (List.fold_left
					(fun (o,j) t ->
						depthProbe t graph (o,j))
					(suffixe, s::visited)
					(liste_succ graph s)
				)
;;

let parcours_prof graph =
	let f (x,_) = x in
		f 	(List.fold_left
				(fun (p,i) s -> depthProbe s graph (p,i))
				([],[])
				(liste_sommets graph)
			)
;;

let rec remove res l1 l2 =
	match l1 with
		| [] -> (List.rev res)
		| h::t -> 	if List.mem h l2 then
						remove res t l2
					else
						remove (h::res) t l2
;;

let connexites graph =
	let suffixe = parcours_prof graph in
	let inverse_suffixe = parcours_prof (inverse_graphe graph) in
	let rec rConnexites (suff, invSuff) save res =
		match invSuff with
			| [] -> res
			| h::t -> 	if h=(List.hd suff) then
							rConnexites
							((remove [] suff (h::t))
							,(remove [] save (h::t))) 
							(remove [] save (h::t))
							[(List.rev (h::t))]@res
						else
							rConnexites
							(suff,t)
							save
							res
	in rConnexites (suffixe,inverse_suffixe) inverse_suffixe []
;;


let graphe1 = [	(1, [6;7;8]) ;
				(2, [1;4]) ;
				(3, [2]) ;
				(4, [3;5]) ;
				(5, [1]) ;
				(6, [5;7]) ;
				(7, []) ;
				(8, [6;7])]
;;

parcours_prof graphe1;;
parcours_prof (inverse_graphe graphe1);;
connexites graphe1;;