module Hw2

(* Question 1 *) 

(*
	given fn: deriv
	params: (f: fn, dx: float)
	return: derviative of fn 
*)

let deriv (f, dx: float) = fun x -> ((f(x + dx) - f(x))/dx)

(*
	newton
	params: (f: fn, guess: float, tol: float, dx: float)
	return: solve the given fn 'f' with newton's method
	
	recursively check if our guess is within tolarence range
*)

let rec newton(f,guess:float,tol:float,dx:float) =
  let close(x:float,y:float,tol:float) = abs(x-y) < tol
  let improve(guess:float,f,dx:float) = guess - (f(guess)/((deriv(f,dx))(guess)))
  if close(f(guess),0.0,tol)
  then
    guess
  else
    newton(f,improve(guess,f,dx),tol,dx)



(* For testing *)
let make_cubic(a:float,b,c) = fun x -> (x*x*x + a * x*x + b*x + c)
let test1 = newton(make_cubic(2.0,-3.0,1.0),0.0,0.0001,0.0001)
(* Should get -3.079599812; don't worry if your last couple of digits are off. *)
let test2 = newton(sin,5.0,0.0001,0.0001)
(* Should get 9.424.... *)


(* Question 2 *)

(*
	type: term
	constructor: Term applied on a (float * int)
	
	type: poly
	constructor: Poly applied on a (float * int) list
*)

type term = Term of float * int
type poly = Poly of (float * int) list

(*
	exception: EmptyListException
	thrown when input list is empty 
*)

exception EmptyListException// we changed the name from the template

(*
	multiplyPolyByTerm
	params: (term, poly)
	return: poly 
	
	given poly p, multiply each term by given term (c, e)
	if p empty -> raise exception
	else
		if c = 0.0 -> return 0 poly
		else 
			pipe p through List.map
				multiplying each (a: float) by c
				adding each (d: int) by e
			pipe p through Poly constructor 
*)

let multiplyPolyByTerm(Term (c, e), Poly p) =
  if p = [] then raise EmptyListException
  else
    if c = 0.0 then
      Poly [(0.0, 0)]
    else
      p
      |> List.map (fun (a, d) -> a * c, e + d)
      |> Poly

let p1: poly = Poly [(3.0, 5); (2.0, 2); (7.0, 1); (1.5, 0)]
let t1: term = Term (2.0, 2)

(*
	addTermToPoly
	params: term (c, e), poly
	return: poly
	
	create recursive helper method
	param: (q: (float * int) list)
	
		recursively get the head (a: float, d: int) of q
		since we are assuming the given poly is in descending order
		check if d (exponent) is less than e
		yes -> given term has the highest degree
			:: with q -> return 
		no -> check if d = e
			yes -> check if a (constant) + c = 0
				yes -> 0 coefficient terms not displayed, return tail
				no -> update constant value, then :: with tail 
			no -> keep Term (a, d), :: with recursive call on tail 
	
	check if given p is empty -> raise exception
	else store the result of the helper p
		and return through Poly constructor 
		if result of helper is empty -> return 0 poly 
	
*)

let addTermToPoly (Term (c, e), Poly p) =
  let rec helper q = 
    match q with
    | [] -> [(c, e)]
    | (a, d) :: ptail ->
              if d < e then
                (c, e) :: q
              elif d = e then
                if a + c = 0.0 then ptail else (a + c, d) :: ptail
              else
                (a, d) :: (helper ptail)

  match p with
    | [] -> raise EmptyListException
    | _ ->
       let tmp = helper p
       if tmp = [] then Poly [0.0, 0] else Poly tmp
                
(*
	addPolys
	params: (p1: poly, p2: poly)
	return: poly 
	
	create recursive helper method
	params: (q1: poly, q2: poly)
		
		get the head (c, e) from 1st list recursively
		recursive call on (tail, q2 + head)
		
		terminating condition: q1 empty -> return q2
	
	if p1 or p2 empty -> exception
	if p1 is 0 poly -> return p2
		vice versa
	store poly 'l' from result of helper
	return l through Poly constructor 
	
*)
				
let addPolys (Poly p1, Poly p2) =
  let rec helper (q1, q2) =
    match q1 with
    | Poly [] -> q2
    | Poly ((c, e) :: q1tail) -> helper (Poly q1tail, addTermToPoly (Term (c, e), q2))

  if p1 = [] || p2 = [] then raise EmptyListException
  elif p1 = [(0.0,0)] then Poly p2
  elif p2 = [(0.0,0)] then Poly p1
  else
    let (Poly l) = helper (Poly p1, Poly p2)
    if l = [] then Poly [(0.0,0)] else Poly l

(*
	multPolys
	param: (p1: poly, p2: poly)
	return: poly 
	
	create recursive helper method
	params: (q1: poly, q2: poly)
	
		recursively get the head (c, e) from q1 
		mutiply q2 by head term
			and add it to recursive call on q1 tail and original q2
	
		terminating condition:
			one item left in q1 -> multiply q2 by term
			empty list -> exception
		
	check if p1 or p2 empty -> exception
	check if p1 or p2 0 poly -> 0 poly
	else helper on given polys 
	
*)	
	
let multPolys (p1, p2) =
  let rec helper (q1, q2) = 
    match q1 with
    | Poly [] -> raise EmptyListException
    | Poly [(c,e)] -> multiplyPolyByTerm (Term (c, e), q2)
    | Poly ((c,e) :: q1tail) -> addPolys (multiplyPolyByTerm (Term (c, e), q2), helper (Poly q1tail, q2))

  if p1 = Poly [] || p2 = Poly [] then raise EmptyListException
  elif p1 = Poly [(0.0,0)] || p2 = Poly [(0.0,0)] then Poly [(0.0,0)]
  else helper (p1, p2)

(*
	given fn: exp
	b^e	// special algorithm
*)  
  
let exp (b, e) =
  let rec helper (b, e, a) =
    if b = 0.0 then 0.0
    elif e = 0 then a
    elif e % 2 = 1 then helper (b, e - 1, b * a)
    else helper (b * b, e / 2, a)
  helper (b, e, 1.0)

(*
	given fn: evalTerm
	params: (v: to sub in, term (c, e))
	
	do c (constant) * v^e
*)
let evalTerm v (Term (c, e)) = if e = 0 then c else c * exp(v, e)

(*
	evalPoly
	params: (p: poly, v: to sub in)
	return: float
	
	check if p empty -> raise exception
	else pipe p through List.map
		evaluate each term -> list of floats 
	then pipe p through fold
		add all items, initial counter = 0.0 
	
*)

let evalPoly (Poly p, v) =
  if p = [] then raise EmptyListException
  else 
     p
     |> List.map (fun (c:float,e:int) -> evalTerm v (Term(c, e)))
     |> List.fold (+) 0.0

(*
	diffPoly
	param: (p: poly)
	return: derivative of poly p
	
	create recursive helper method
		param: list 'l'
		return: derivative in list form
	
		recursively get the head (c, e)
		(c * e, decrement e) :: recursive call on tail
	
	if p empty -> exception
	else 
		store result of helper in 'tmp'
		if tmp empty -> return 0 poly
		else return tmp through Poly constructor 
*)	 
	 
let diffPoly (Poly p) =
  let rec helper l  =    
    match l with
    | [] -> []
    | [(c, e)] -> if e = 0 then [] else [(c*float(e), e-1)]
    | (c, e) :: ptail -> (c * float(e), e - 1) :: (helper ptail)
  if p = [] then raise EmptyListException
  else
    let tmp = helper(p)
    if tmp = [] then Poly [(0.0,0)] else Poly tmp

(* Question 3 *)

(*
	type: Exptree
	constructors:
		Const applied on int
		Var applied on string
		Add applied on (ExpTree * Exptree)
		Mul applied on (ExpTree * Exptree)
*)

type Exptree =
  | Const of int 
  | Var of string 
  | Add of Exptree * Exptree 
  | Mul of Exptree * Exptree
    
(*
	type: Bindings
	list of (string, int) pairs
*)  

type Bindings = (string * int) list

(* exception notFound *)

(*
	lookup
	params: (name: string, env: Bindings)
	return: options
	
	recursively get the head from 'env' (key: string, value: int)
	check if 'key' matches 'name'
		yes -> return 'Some value'
		no -> check if the name > key
			since we assume the env list is in order
			a < b < ... z
			then recursive call on the tail
		else 
			return None
	
	terminating condition: empty list -> return 'None'
*)

let rec lookup(name:string, env: Bindings) =
  match env with
    | [] -> None
    | (key,value)::rest ->
      if (name = key)
      then
        Some value
      elif (key < name)
      then
        lookup(name,rest)
      else None

(*
	insert
	params: (name: string, value: int, b: Bindings)
	return: new list of type Bindings
	
	recursively get the head of 'b' (key, x)
	if the 'name' we want to insert is less than 'key'
		eg. 'a' < 'b'
		return (name, value) :: b
		this is in order since we assume the given Bindings list 
			is already in order
	do the same if name = key
		the most recent value for 'name' will be at the beginning of the list 
	otherwise (key > name)
		keep the (key, x) pair and :: with recusive call on (name, value, tail of b)
		
	terminating condition: empty list 'b'
		so all keys in b are < 'name' -> :: with name, value pair at the end
*)
	  
let rec insert(name:string, value: int, b: Bindings) =
  match b with
    | [] ->  [(name,value)]
    | (key,x)::rest ->
        if (name < key) 
        then
          (name,value)::b
        elif (name = key)
        then
          (name,value)::b
        else
          (key,x)::(insert(name,value,rest))

(*
	eval
	params: (exp: Exptree, env: Bindings)
	return: options
	
	if exp is a Const n -> return Some n (int)
	if exp is a Var name -> call lookup on given name and input env
		-> returns options (Some int, or None)
	if exp is a Add of (Exptree * Exptree)
		store the results (options) 
			of recurisive call on e1 and e2 (with same env) in a1, a2
		if a1 is Some v -> 
			if a2 is Some u -> do u + v
		else -> None
	same for exp is a Mul...
		
*)
                                           
let rec eval(exp : Exptree, env:Bindings) =
  match exp with
    | Const n -> Some n
    | Var name -> lookup(name, env)
    | Add(e1,e2) ->
      let a1 = eval(e1,env)
      let a2 = eval(e2,env)
      match a1 with
        | None -> None
        | Some v ->
            match a2 with
              | None -> None
              | Some u -> Some (u + v)
    | Mul(e1,e2) ->
      let m1 = eval(e1,env)
      let m2 = eval(e2,env)
      match m1 with
        | None -> None
        | Some v ->
            match m2 with
              | None -> None
              | Some u -> Some (u * v)


(* For testing *)

let env:Bindings = [("a",3);("b",4);("c",5)]                                

let exp1 = Add(Const 3, Const 4)
let exp2 = Add(Const 3, Var "b")
let exp3 = Add(Var "c", Var "b")
let exp4 = Mul(exp3,exp2)
let exp5 = Add(Var "d",exp3)
let env2 = insert("b",10,env)




(* Question 4 *)

(*
	type: Team
		a string
	type: Goals
		constructor: Goals on type int
	type: Points
		constructor: Points on type int
	type: Fixture
		(Team, Team) pair
	type: Result
		((Team, Goals), (Team, Goals)) pair
		matches both teams with the amount of goals they scored
	type: Table
		Map of (Teams and their Points)
*)

type Team = string
type Goals = Goals of int
type Points = Points of int
type Fixture = Team * Team  
type Result = ((Team * Goals) * (Team * Goals))
type Table = Map<Team,Points>
    
(*
	league
	list of Teams
*)	
	
let league =
  ["Chelsea"; "Spurs"; "Liverpool"; "ManCity"; "ManUnited"; "Arsenal"; "Everton"; "Leicester"]

(*
	pointsMade
	param: (r: Result)
	return: ((Team * Points), (Team * Points))
	
	we want to convert the 'r' of a game to a points system
		later used to add to a Table
		
	recall that Result 'r' is a ((Team, Goals), (Team, Goals)) pair
	check who scored more points by comparing g1 and g2
	team with more goals gets 3 points
	otherwise (g1 = g2)
		both teams get 1 point
		
	type Team does not need a construtor
	return Points type through Points constructor 
*)  
  
let pointsMade (r: Result) = 
  match r with 
  | ((t1,Goals g1),(t2, Goals g2)) -> 
         if (g1 > g2) then ((t1, Points 3),(t2, Points 0))
         elif (g2 > g1) then ((t1, Points 0),(t2, Points 3))
         else ((t1, Points 1),(t2, Points 1))

(*
	given fn: initEntry
	param: (name: Team)
	return: (name, Points) pair
	
	set the Team 'name' with 0 points to start out with
*)
		 
let initEntry (name:Team) = (name, Points 0)
  
(*
	given fn: initializeTable 
	param: l
	return: (Map: Table)
	
	apply List.map with initEntry on input 'l'
		to get a list with each Team starting with 0 Points
	Map.ofList on this new list to create a Table 
		recall type Table is a Map<Team,Points>
*)
  
let initializeTable l = Map.ofList (List.map initEntry l)

(*
	weekend1, weekend2
	lists of type Result
	
	recall type Result is a pair of (Team, Goals)
	
*)

let weekend1:Result list = [(("Chelsea", Goals 2),("Spurs", Goals 1)); (("Liverpool", Goals 3),("ManCity", Goals 2));(("ManUnited", Goals 1),("Arsenal", Goals 4));(("Everton", Goals 1),("Leicester", Goals 5))]

let weekend2:Result list = [(("Chelsea", Goals 5),("Arsenal", Goals 0)); (("Spurs", Goals 3),("ManCity",Goals 2)); (("ManUnited", Goals 1),("Liverpool", Goals 0));(("Everton", Goals 3),("Leicester", Goals 5))]

(*
	s
	list of list of type Result
	
*)

let s = [weekend2;weekend1]

(*
	updateTable 
	params: (t: Table, r: Result)
	return: Table 
	
	store result of 'pointsMade r' in ((team1, Points p1),(team2, Points p2))
	get the amount of points by team1 currently
		with 'Map.find team1 t' 
			Map.find returns the value of key 'team1' in Map 't'
		store value in (Points x1)
	do the same for team2
	
	use 'Map.add' on 'team1' 
		with updated int value
		passed through constructor Points
		
	note: Map.add returns a Map and requires a Map as a parameter
	use the returned Map from 
		use 'Map.add' on 'team2' 
			with updated int value
			passed through constructor Points
		with t as given Map
	as the Map parameter
*)

let updateTable(t:Table,r:Result):Table = 
  let ((team1, Points p1),(team2, Points p2)) = pointsMade r
  let (Points x1) = Map.find team1 t
  let (Points x2) = Map.find team2 t
  Map.add team1 (Points(x1+p1)) (Map.add team2 (Points(x2+p2)) t)

(*
	weekendUpdate 
	params: (t: Table, rl: Result list)
	return: Map of type Table
	
	recursively get the head of 'rl' (r)
	recursive call on (updateTable(t, r)) with rest (tail) of Result list
	
	terminating condition: empty list -> no more results to update, return t
*)  
  
let rec weekendUpdate(t:Table,rl: Result list): Table =
  match rl with
  | [] -> t
  | r::rs -> weekendUpdate(updateTable(t,r),rs)

(*
	seasonUpdate
	params: (t: Table, sll: Result list list)
	return: Map of type Table 
	
	recursively get the head of 'sll' (w) -> this is a Result list 
	recursive call on (weekendUpdate(t, w)) with tail 
	
	terminating condition: no more Result lists left in list
		-> return t
*)  
  
let rec seasonUpdate(t:Table, sll:Result list list) : Table =
  match sll with
  | [] -> t
  | w::ws -> seasonUpdate(weekendUpdate(t,w),ws)

(*
	less
	params: (s1, n1), (s2, n2); both are (Team, Points) pairs
	return: boolean 
	
	check if Points of s1 < Points of s2
		yes -> true
		no -> false
	if Points are equal -> return s1 > s2
		recall type Team is a string
		'b' > 'a' -> return true
		for ordering by reverse alphabetically 
		
	recall
		obtain int value of Points
		with 'let Points p1 = n1'
		where n1 is input arg of type Points
		p1 is the int value -> passed through Points constructor
	
*)

let less((s1,n1):Team * Points, (s2,n2):Team * Points) = 
  let Points p1 = n1
  let Points p2 = n2
  if (n1 < n2) then true 
  elif (n1 > n2) then false
  else (s1 > s2) //n1 = n2, we order them reverse alphabetically

(*
	given fn: myinsert
	params: (item: a') (lst: a' list)
	return: (a' list)
	
	recursively get the head of 'lst' (x)
	check if input arg 'item' < 'x'
		yes -> x > item, so it is in right place
			since we want to show the standings by (biggest to smallest) points
			x :: with recursive call of same 'item' and tail
		no -> 
			
			terminating condtion (1): 
			
			item > x, so place item in front of lst
			return item :: lst 
			
			no need for further recursive calls 
				since item is now placed in correct sub list
				return item :: lst to :: with rest of stack
	
	terminating condition (2):
		end of list
		item we want to insert indeed < all elements in list
		place return [item] to :: with rest of stack 
*)  
  
let rec myinsert item lst =
  match lst with
  | [] -> [item]
  | x::xs -> if less(item,x) then x::(myinsert item xs) else item::lst

(*
	given fn: isort
	param: (lst: list)
	return: sorted list (biggest to smallest)
	
	recursively get the head in 'lst' (x)
	insert x into recursive call on tail 
	
	terminating condition: empty list
		-> return empty list to :: with the last element
		(only 1 element so sorted indeed)
	
*)  
  
let rec isort lst =
  match lst with
  | [] -> []
  | x::xs -> myinsert x (isort xs)

(*
	given fn: showStandings
	param: (t: Table)
	
	convert t to list with Map.toList 
	and perform isort on new list 
*)  
  
let showStandings (t:Table) = isort (Map.toList t)
                                                  
(* Question 5 *)

(*
	type: Destination
		constructor: City applied on a string
	type: RoadMap	
		constructor: Roads applied on 'Map<Destination, Set<Destination>>'
	
	a RoadMap is a Map
		key: current 'Destination'
		value: Set of neighbouring Destinations
*)

type Destination = City of string
type RoadMap = Roads of Map<Destination, Set<Destination>>

(*
	given: roadData
	
	list of (string, list of strings) pairs 
	later: turn into a RoadMap
*)

let roadData = [
  "Andulo", ["Bibala"; "Cacolo"; "Dondo"]
  "Bibala", ["Andulo"; "Dondo"; "Galo"]
  "Cacolo", ["Andulo"; "Dondo"]
  "Dondo",  ["Andulo"; "Bibala"; "Cacolo"; "Ekunha"; "Funda"]
  "Ekunha", ["Dondo"; "Funda"]
  "Funda",  ["Dondo"; "Ekunha"; "Galo"; "Kuito"]
  "Galo",   ["Bibala"; "Funda"; "Huambo"; "Jamba"]
  "Huambo", ["Galo"]
  "Jamba",  ["Galo"]
  "Kuito",  ["Ekunha"; "Funda"]
]

(*
	makeRoadMap
	param: (data: list)
	return: RoadMap 
	
	pipe data through:
		List.map
			fn: 
				params: originString, destinationStrings
				
				store originString as a Destination with City constructor -> origin
				pipe destinationStrings through:
					List.map
						constructor function City
							-> every string now a Destination type
					Set.ofList
						convert to Set<Destination>
					
					store -> destinations
				
				return ((origin: Destination, destinations: Set<Destination>) list)
		Map.ofList
			creates Map<Destination, Set<Destination>>
		Roads
			constructor creates a RoadMap
			
*)

let makeRoadMap data = 
  data
  |> List.map (fun (originString, destinationStrings) -> 
    let origin = City originString
    let destinations = 
      destinationStrings 
      |> List.map City
      |> Set.ofList
    origin, destinations
  )
  |> Map.ofList
  |> Roads

(*
	upToManySteps 
	params: (RoadMap) (n: int) (startCity)
	return: Set
	
	Map.tryFind startCity r
		returns options
		
		recall r is a Map<Destination, Set<Destination>>
		so startCity is a Destination 
		
		return None if not in r 
		return Some neighbours (Set<Destination>) otherwise
	
	in the case where Some neighbours is returned
	match n (number of steps)
		if 0 -> create a new empty Set (Set.empty) and add startCity -> return 
			Set.add startCity Set.empty
			
		else 
			store result of recursive call on (Roads r) (n - 1) (startCity) -> prev
				
				when n goes from 1 to 0 
					-> this will contain a Set of the current City 
			
			store result of 
				pipe neighbours (Set<Destination>) through:
					Set.map 
						recursive call on (Roads r) (n - 1)
						
						each Destination in neighbours will be traversed
						when n goes from 1 to 0
							-> this will produce a Set of a neighbouring city
						
					Set.unionMany
						
						combine all the neighbouring cities 
						from popping off the stack 
						
				-> newCities
				
			return Set.union prev newCities 
				
				combine all current + neighbouring cities 
				
*)  
  
let rec upToManySteps (Roads r) n startCity =
  match Map.tryFind startCity r with
  | None -> failwithf "%A not in map" startCity//You are not required to check this.
  | Some neighbours ->
    match n with
    | 0 -> Set.add startCity Set.empty
    | _ -> let prev = (upToManySteps (Roads r) (n - 1) startCity)
           let newCities = (neighbours |> Set.map (upToManySteps (Roads r) (n - 1)) |> Set.unionMany)
           Set.union prev newCities


		   
		   
		   
		   
		   
		   
		   
        