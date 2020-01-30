

let rec longueur = function
	[] -> 0
	| x::r -> 1 + longueur r
;;

let rec concat li re =
	match li with
		[] -> re
		| x::r -> x::(concat r re)
;;

let rec nieme li n =
	match li with
		[] -> failwith "error: out of bound"
		| x::r -> if n = 0 then x
					else nieme r (n-1)
;;

let rec npremiers li n =
	match li with
		[] -> failwith "error: out of bound"
		| x::r -> if n = 1 then x::[]
					else x::(npremiers r (n-1))
;;

let rec met_a_plat li =
	match li with
		[] -> []
		| x::r -> concat x (met_a_plat r)
;;

let rec paire_vers_liste c =
	match c with
		(a,b) -> if longueur a = longueur b then
					match a with
						[] -> []
						| x::r -> match b with
									[] -> []
									| y::s -> (x,y)::(paire_vers_liste (r,s))
				 else failwith "lengths not equals"
;;

let rec liste_vers_paire li =
	match li with
		[] -> ([],[])
		| (a,b)::r -> match (liste_vers_paire r) with
						(c,d) -> (a::c, b::d)
;;

let rec supprime1 li s =
	match li with
		[] -> []
		| x::r -> if x = s then r
				else x::(supprime r s)
;;

let rec supprime2 li s =
	match li with
		[] -> []
		| x::r -> if x = s then supprime2 r s
				  else x::(supprime2 r s)
;;

let min_liste li =
	match li with
		[] -> failwith "empty list"
		| x::r -> let m = x in
			let rec find li min =
				match li with
					[] -> min
					| x::r -> if x < min then find r x
			in find li m
;;