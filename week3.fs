
//
// week 3 - comp 302
//

//                  //
// HIGHER ORDER FNS //
//                  //

let rec sumInts(a, b) = 
    if(a > b) then 
        0
    else
        a + sumInts(a + 1, b)

// takes 2 ints: a, b
    // teminating condition: when a > b
    // recursively sum value of a, add 1 to a each time 

// summation from i = 1 to 10 of i 
sumInts(1, 10) |> ignore  

let rec sumSquares(a, b) = 
    if(a > b) then 
        0
    else 
        (a * a) + sumSquares(a + 1, b)

// summation from i = a to b of i 
sumSquares(1, 10) |> ignore 

let rec sumCubes(a, b) = 
    if(a > b) then
        0
    else    
        (a * a * a) + sumCubes(a + 1, b)

let square n = 
    n * n 

let cube n = 
    n * n * n 

let rec sum_inc(f, a, b, inc) = 
    if(a > b) then 
        0
    else
        (f a) + sum_inc(f, (inc a), b, inc)

// now we can choose what fn to perform on a 
// and how much to 'increment' a by    
sum_inc(square, 1, 10, fun x -> x + 1) |> ignore 

let byTwo n = 
    n + 2 

let rec product(f, a, b, inc) = 
    if(a > b) then 
        1
    else 
        (f a) * product(f, (inc a), b, inc)

let id x = x 
let inc n = n + 1
product(id, 1, 5, inc) |> ignore 

let acc(comb, f, a, b, inc, init) = 
    let rec helper(a, res) = 
        if(a > b) then 
            res 
        else 
            helper(inc(a), comb(res, f(a))) 
    helper(a, init) 

let test = acc(
            (fun (x, y) -> x + y),      // comb 
            (fun n -> n * n),           // f 
            1,                          // a 
            5,                          // b 
            (fun m -> m + 1),           // inc 
            0)                          // init 

// call helper recusively 
    // starting with init value '0' 
    // apply changes to init 
    // increment a 
    // return when a > b (terminating condition) 

// helper(inc(a), comb(res, f(a)))
    // recursive call to helper
    // with incremented a 
    
    // do a * a 
    // then add with result value 
    // pass this as the 'next' result 

//
// double and self application
// 

let twice f =
    fun x -> f (f x)

// apply function twice to x 
twice (fun x -> x + 1) 1 |> ignore 

let fourtimes f = 
    (twice twice) f 

let compose(f, g) = 
    fun x -> g(f(x)) 

// 
// calculus examples   
//

let deriv(f, dx: float) =   
    fun x ->
        ((f(x + dx) - f(x)) / dx) 

let abs(x: float) = 
    if(x < 0.0) then
        -x 
    else 
        x 

// calculates absolute value of x 


let close(x: float, y: float, tol: float) = 
    (abs(x - y) < tol) 

// checks if x and y are 'close'
    // their difference is within 'tol' 


//let square(x: float) = x * x 

let rec halfint(f, pos_value: float, neg_value: float, epsilon: float) = 
    let mid = (pos_value + neg_value) / 2.0 
    if(abs(f(mid)) < epsilon) then
        mid 
    elif(f(mid) < 0.0) then 
        halfint(f, pos_value, mid, epsilon)
    else  
        halfint(f, mid, neg_value, epsilon)

let rec iter_sum(f, lo: float, hi:float, inc) = 
    let rec helper(x: float, result: float) = 
        if(x > hi) then 
            result 
        else 
            helper(inc(x), f(x) + result)
    helper(lo, 0.0) 

// see above for explanation 
// used for integral calculations 

let integral(f, lo: float, hi: float, dx: float) =
    let delta(x: float) = x + dx 
    dx * iter_sum(f, (lo + (dx / 2.0)), hi, delta)

let r_sq(x: float) = x * x 

integral(r_sq, 0.0, 1.0, 0.001) |> ignore 

// integral of x^2 from 0 to 1
    // r_sq = f 
    // 0.0 = lo 
    // 1.0 = hi 
    // 0.001 = dx (these are the widths of the rectangles) 

// do dx * iter_sum
    // r_sq = f 
    // 0.0 + 0.001 / 2.0 = lo 
    // 1.0 = hi 
    // delta = inc 

integral(sin, 0.0, 3.14159, 0.001) |> ignore 

//
// church nums 
// todo: WATCH RECORDING -- jan 25 -- 

let zero = fun f -> (fun x -> x)
let one = fun f -> (fun x -> (f x))
let two = fun f -> (fun x -> (f (f x)))

let showcn cn =
    (cn (fun n -> n + 1)) 0

let r1 = showcn one 
// showcn takes a fn as input 'one'
    // one applys the fn once 
    // 0 + 1 -> 1 

let r2 = showcn two 

//
// todo: WATCH RECORDING -- jan 25 -- 
// 

let succ cn =
    (fun f -> 
        (fun x ->
            f((cn f) x)))

let r3 = showcn (succ two)

let add n m = 
    fun f ->
        (fun x -> ((n f) ((m f) x))) 

let times n m = 
    fun f ->
        (fun x -> 
            (n (m f) x))

let exp n m =
    fun f ->
        (fun x ->
            (m n) f x) 

//
// --- thursday + friday ---  
//


//                             //
// PIPES/COMPOSITION OPERATORS //
//                             //

// normal use of map
let map = List.map
//let inc n = n + 1

map inc [1 .. 5] |> ignore 
// int list = [2; 3; 4; 5; 6]

//
// can construct ARG FIRST, then pass it to fn using |>
// x |> f same as f x 

[1 .. 5] |> map inc |> ignore 

// useful to apply sequnce of functions to expression
[1 .. 5] 
    |> map (fun n -> n * n) 
    |> List.fold (+) 0
    |> ignore 
// sum squares 


// forward composition operator 
    // >>
    // f >> g same as fun x -> g(f(x))

// map then fold 
((map (fun n -> n * n)) >> (List.fold (+) 0)) [1 .. 5] |>  ignore 

// TYPE ERROR if no outer () 
    // there is a backward pipe <|
    // f <| x same as f x 

List.fold (+) 0 <| map (fun n -> n * n)  [1..5] |> ignore 

// backward composition 
    // <<
    // (f << g) same as fun x -> f(g x)

//let square n = n * n
let negate n = -n

(square >> negate) 5 |> ignore 
    // square then negate 
    // int = -25
(square << negate) 5 |> ignore 
    // negate then square 
    // int = 25

//
// power set -- watch lecture 
//

let rec powerlist l =
    match l with
    | [] -> [[]]
    | h :: t ->
        [for x in (powerlist t) -> h :: x] @ (powerlist t)

let rec powerlist2 l =
    match l with
    | [] -> [[]]
    | x :: xs -> 
        let p = powerlist xs
        (List.map (fun u -> x :: u) p) @ p

let rec powerset (s: Set<int>) = 
    if Set.isEmpty s then 
        Set.add (Set.empty) Set.empty
    else 
        let i = s.MaximumElement 
        let s2 = Set.remove i s
        let p = powerset s2
        Set.union (Set.map (fun x -> Set.add i x) p) p

//                      //
// IMPERIAL PROGRAMMING //  
//                      //

// imperial programming in f#
    // see pdf

let mutable x = 1

// forbidden (cannot pass mutable arg) 
    // let change (mutable n: int) = n <- n + 1 

// instead declare a mutable variable inside the fn 
// apply changes to mutable variable 
let munge (n: int) =
    let mutable m = n
    (m <- m + 1)

// recall: x is mutable, x = 1
let mess_with () = 
    x <- x + 1

let result = 
    (printfn "x is %i" x); 
    (mess_with ()); 
    x

let mash (n: int) =
    let mutable m = n
    (printfn "m is %i" m); 
    (m <- m + 1); 
    (printfn "n is %i" n); 
    (printfn "m is %i" m); 
    m

mash x |> ignore 

let foo n =
    let mutable x = n in     
        while (x < 10) do 
            (printfn "x is %i" x);
            (x <- x + 1)

foo x

//
// basic record syntax
//

// define a type 'Person'
    // with properties:
        // name, birthday, title 
type Person = 
    {
        name: string; 
        birthday: int * int; 
        title: string
    }

// create a Person 'prakash' 
let prakash = 
    { 
        name = "Prakash"; 
        birthday = (3, 11); 
        title = "Bane of while loops"
    }

// define type 'intRec'
    // with mutable property: count
type intRec = {mutable count: int}

let testintRec = {count = 0}

// fn: increment
    // args: counter (intRec) 
    // increments mutable property 'count' 
let increment (counter: intRec) =
    counter.count <- counter.count + 1
    counter.count

// what happens below?
    // use:
        // ref, :=, !

// create a mutable reference cell
let x1 = ref 1      // val x : int ref = {contents = 1;}
let y = x1          // val y : int ref = {contents = 1;}

let incr n =        // assign to a mutable reference cell 
    (n := !n + 1)          

incr x1            

!x1 |> ignore       // '!' dereferences mutable reference cell
!y                  // y points to x1
                        // contents of y = 2 

// create a mutable reference cell z 
    // contents of z = contents of y 
    // does not point to y 
let z = ref (!y)

incr z

!z |> ignore        // contents of z = 3
!y |> ignore        // y unchanged (contents of y = 2)



















