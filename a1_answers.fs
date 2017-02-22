(* module hw1_sol.  Use this if you want to load the file into an interactive session.*)

(* Question 1 illustrating lists *)

(* 
	sumlist
	param: l: float list 
	return: sum of list: float
	
	recursively get all the float elements in list 'l'
	sum them
	
	terminating condition: empty list -> add 0.0 to sum 
*)

let rec sumlist l =
  match l with
  | [] -> 0.0 
  | (x::xs) -> x + sumlist(xs)
  
(*
	pairlists
	param: twolists: (a' list * b' list)
	return: (a' * b') list 
	
	recursively get the head of the both lists 
	pair them together (comma separated)
	terminating conditions:
		both lists are empty -> cons with [] list 
		one list empty, other still not empty
			-> failwith error message 
*)  
  
let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: (pairlists (xs,ys))

(*
	w_mean
	params: weights: list, data: list 
	return: w_mean: float 
	
	calculate the denominator for return value 
		sum the 'weights' list 
	
	generate a pair list of (weights * data) with (weights, data)
	
	compute the numerator 
		with List.map 
		passing function that takes 2 floats (x, y)
			and computes x * y 
			for each (x, y) input pair 
		map method applies such this function 
			on all elements in given list 'pairs'
		then sum the list afterwards 
	
	do numerator / denominator
*)	
	
let w_mean weights data =
  let denom = sumlist weights
  let pairs = pairlists (weights, data)
  (sumlist (List.map (fun (x,y) -> x * y) pairs))/denom
  
(* w_mean [1.0;1.5;2.5;0.5;1.5] [10.3;11.7;2.0;5.0;6.5] *)

(* Question 2. *)

(*
	memberof
	param: pair: (n: element, a: list)
	return: boolean value 
	
	recursively get the head of the list 
	check if element 'n' is equal to the head 
		true -> true
		false -> check recursively on the tail of the list 
		
	terminating condition: empty list -> false 
*)

let rec memberof pair =
  match pair with
  | (n,[]) -> false
  | (n,(x::xs)) -> if (x = n) then true else memberof(n,xs)

(*
	remove 
	param: (item: item, lst: list)
	return: list without 'item' 
	
	recursively get the head of the list 
	if head = item
		do not :: with the recursive call on the tail of list 
	else keep the head
		ie. :: with the recursive call on the tail of list 
	
	terminating condition: empty list -> :: with [] and return new list 
*)  
  
let rec remove(item, lst) = 
  match lst with
  | [] -> []
  | (x::xs) -> if (x = item) then remove(item, xs) else x::(remove(item, xs))

(* Question 3. *)

(*
	findMax
	param: l: list 
	return: max value in list 
	
	define recursive helper function
	params: (l: list, m: current max value)
	
	recursively get the head of 'l'
	check if head > 'm'
		true -> recursive call on tail of l, with new max value as head 
		false -> recursive call on tail of l, with old max value
	terminating condition: empty list -> return m 
	
	initialize with by getting the head of input list 
		set that as current max 
		and tail of input list l as the rest of list 
*)

let findMax l =
  let rec helper(l,m) =
    match l with
      | [] -> m
      | (x::xs) ->
          if (m < x) then helper(xs,x) else helper(xs,m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)

(*
	selsort 
	param: l: list 
	return: sorted list 
	
	recursively find the max of list 'l' -> m 
	do m :: recursively call (with new (remove max 'm' from l) list)

	terminating condition: empty list -> :: with [], return 
*)  
  
let rec selsort l =
  match l with
  | [] -> []
  | _ -> let m = (findMax l)
         m::(selsort(remove(m,l)))

(* Question 5. *)

(*
	common
	param: twolists: (a' list, a' list)
	return: list with common elements
	
	recursively get the head of the first list 
	check if head is member of 2nd list 
		yes -> keep the head in the recursive call
			do head :: recursive call with tail of first list
					   and 2nd list after removing head
		no -> dont keep head
			recursive call on the tail of first list 
			and 2nd list 
	
	terminating condition: one list empty -> return :: with []
	
*)

let rec common twolists =
  match twolists with
    | (l,[]) -> []
    | ([],l) -> []
    | ((x::xs),l) ->
        if memberof(x,l)
        then
          x::(common(xs,remove(x,l)))
        else
          common(xs,l)

(* Question 6. Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

(*
	split
	param: l: list 
	return: (a' list * a' list)
	
	recursively get the first 2 items in the list 
	recursive call on the tail of list is stored (odds, evens)
	
	use that result and do
		1st item :: odds, 2nd item :: evens
		
	terminating condition: 
		when the original list is even size
			empty list -> return ([], []) to :: with 
		when original list odd size
			only 1 item left -> return (item :: [], []) to :: with
			
*)

let rec split l =
  match l with
  | [] -> ([],[])
  | x :: [] -> (x::[],[])
  | (x::y::l) ->
  let (odds,evens) = split(l)
  in
      (x::odds,y::evens)

(*
	merge
	param: twolists: (a' list, a' list)
	return: sorted single list using all items
	
	recursively get the head of both lists
	check which head 'x' or 'y' greater
	if x < y
		x :: recursive call with tail of x, whole y list 
	else 
		y :: recursive call with whole x list, tail of y 
	
	terminating conditions:
		left list empty -> return right list to :: with 
			note: rest of right list already sorted
		vice versa for right list empty 
*)	  	
	  
let rec merge twolists =
  match twolists with
  | ([],R) -> R
  | (L,[]) -> L
  | (x::xs, y::ys) ->
    if (x < y)
    then
      x::merge(xs, y::ys)
    else
      y::merge(x::xs,ys)

(*
	mergesort
	param: l
	return: sorted list 
	
	recursively get the head of list 'l'
	store result of split original list 'l' in (M, N)
		store result of recursive call on split lists: M2, N2
			
			terminating condition: only 1 item left 'n'
				-> return n
				
			then merge M2, N2 down the stack 
	
	why the result is sorted
		split lists into lists of size 1
		lists of size 1 already sorted
	when we merge together -> another sorted array produced
		continue merging down the stack 
		
	
*)	  
	  
let rec mergesort l =
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns ->
    let (M,N) = split(l) in
      let M2 = mergesort(M)
      let N2 = mergesort(N)
      in
      merge(M2,N2)
	  
	  
	  
	  
	  
	  

