
//
// week 5
// 

//            //
// EXCEPTIONS //
//            //

// quadratic formula
    // solves: ax^2 + bx + c = 0
    // first check if discriminant valid 
let solve(a, b, c) =
    let disc = (b * b - 4.0 * a * c)
    if disc < 0.0 || a = 0.0 then
        failwith "The discriminant is negative or a was zero."
    else
        ((-b + sqrt(disc)) / (2.0 * a), (-b - sqrt(disc)) / (2.0 * a))

// same as above, but now raises exception 
exception Solve

let solve_quad(a, b, c) =
    let disc = (b * b - 4.0 * a * c)
    if disc < 0.0 || a = 0.0 then
        raise Solve 
    else
        ((-b + sqrt(disc)) / (2.0 * a), (-b - sqrt(disc)) / (2.0 * a))
        
let solve_text(a, b, c) = string(solve_quad(a, b, c))

let solve_catch(a, b, c) = 
    try
        string(solve_quad(a, b, c))
    with
    | Solve -> "No real solutions"

// error if not casted to string 

// same as above, more detailed 

exception NegDisc   // raised if discriminant < 0
exception AisZero   // raised if a = 0 

// this method returns a (float * float)
    // define exceptions to also return same type 
let solve_robust(a, b, c) =
    try
        let disc = (b * b - 4.0 * a * c)
        if a = 0.0 then
            raise AisZero
        elif disc < 0.0 then
            raise NegDisc
        else
            ((-b + sqrt(disc)) / (2.0 * a), (-b - sqrt(disc)) / (2.0 * a))
    with
    | AisZero -> 
        (printfn "This is not a quadratic you idiot!");
        (- c / b, - c / b)
    | NegDisc -> 
        (printfn "The roots are complex. The real and imaginary parts are: ")
        let disc = (b * b - 4.0 * a * c)
        let realpart = -b / (2.0 * a)
        let imagpart = (sqrt(-disc)) / (2.0 * a)
        (realpart, imagpart)

//
// built in exceptions
//

// failwith - throws error message 
let basic n = 
    if n = 0 then 
        0 
    else 
        failwith "N was not 0"

// invalidArg - throws name of arg + message 
let fact n =
    let rec helper (n, m) =
        if (n = 0) then 
            m 
        else 
            helper(n - 1, n * m)
    if n < 0 then 
        invalidArg "n" "cannot be negative"
    else 
        helper(n, 1)

// nullArg - self explanatory 
let gethead (l: int list) =
    match l with
    | [] -> nullArg "l" "cannot be the empty list"
    | x :: xs -> x

// invalidOp - invalid operation 
    // like division by 0 
let divide(n, m) =
    if m = 0 then 
        invalidOp "tried to divide by 0" 
    else n / m

// sequence to string 
let ConvertToString (l: 'a seq) = System.String.Join(",", l)

// 
// exceptions for backtracking
//

// first define the exception 
exception Change

let makeChange coinTypes amt = 

    // define helper function 
    // use pattern matching to calculate how much change you can get 
    let rec helper(coins: int list, amt: int): int list =
        match (coins, amt) with
        | (_, 0) -> []
        | ([], _) -> raise Change // cannot make change, raise exception 
        | (coin :: rest, amt) -> 
            try
                // if the 'coin' is too big 
                // recursively call on the next biggest coin 
                if (coin > amt) then 
                    helper(rest, amt) 
                else 
                    coin :: (helper(coins, amt - coin))
            with 
            | Change -> helper(rest, amt)

    try
        let C = helper(coinTypes, amt) in
            // use our convert to string method on the returned int list 
            ("Return the following coins: " + (ConvertToString C) + "\n")
    with
    | Change -> "Sorry, I cannot make change.\n"

// testing makeChange 
makeChange ([200; 100; 25; 10; 5; 1]) (547) |> ignore 
makeChange ([]) (547) |> ignore 

//                                //
// DESTRUCTIVE INSERT INTO A LIST //
//                                //

// create a data type 'Cell'
    // has int data, RList next 
    // Rlist is another cell 
type Cell = { 
    data: int; 
    next: RList
}
and RList = Cell option ref

// first cell's next value is a null mutable reference cell 
let c1 = {
    data = 1; 
    next = ref None
}

// create cell 2
// Some c1 is the representation of value type c1 (a Cell) 
    // store in mutable reference cell 
let c2 = {
    data = 2; 
    next = ref (Some c1)
}
let c3 = {
    data = 3; 
    next = ref (Some c2)
}
let c5 = {
    data = 5; 
    next = ref (Some c3)
}

// converting RList to list 
    // given a RList 'c'
    // dereference it 
// pattern match it 
    // terminating condition: None
    // if it is some data type Cell 
        // recursively get the data from each Cell 
let rec displayList (c: RList) = 
    match !c with
    | None -> []
    | Some { 
            data = d; 
            next = l 
        } -> d :: (displayList l)

// cell to RList 
let cellToRList (c: Cell): RList = ref (Some c)
    
// fn bigger, checks if 1st int > 2nd int 
let bigger(x: int, y: int) = (x > y)

// insert function overview
// given a value 'item' 
    // and a comparison function 'comp'
    // insert a new Cell with item into the given RList 

// terminating condition: we reached the end of the RList
    // in our case, using the bigger fn 
    // this would be the beginning of the list 
    // create a new Cell, insert it into the RList

// get the data of the current node 
    // if value item > data d 
    // insert a new Cell with value item 
// otherwise recusively call the fn on the next list 'tail' 

let rec insert comp (item: int) (list: RList) =
    match !list with
    | None -> 
        list := Some { 
                    data = item; 
                    next = ref None
                }
    | Some {data = d} when comp (item, d) ->
            let newCell = Some { 
                            data = item; 
                            next = ref (!list) 
                        } 
                            in list := newCell
    | Some {next = tail} ->
            insert comp item tail

//              //
// TIMING TESTS //
//              //

(* This is how you turn on timing

> #time;;

--> Timing now on

*)

let lst = [1; 2]

let rec wastetime n =
    match n with
    | 0 -> lst
    | _ -> 
        let ys = n :: wastetime(n - 1)
        List.rev ys

// test wastetime 3
wastetime 3

// 3 :: wt(2)
    // 3 :: 2 :: wt(1)
    // 3 :: 2 :: 1 :: wt(0)
    // 3 :: 2 :: 1 :: 1, 2
    // 3 :: 2 :: 2, 1, 1
    // 3 :: 1, 1, 2, 2
    // 2, 2, 1, 1, 3

let rec fib n =
    match n with
    | 0 -> 1
    | 1 -> 1
    | _ -> fib(n - 1) + fib(n - 2)

// fib fn - tail recursive 
// tailfib 5
    // helper 5, 1, 1
    // helper 4, 1, 2
    // helper 3, 2, 3
    // helper 2, 3, 5
    // helper 1 -> 5 

// notice we are calculating the fib sequence 
    // and passing fib(i) and fib(i + 1) as parameters 
let tailfib n =
    let rec helper (n, a, b) =
        match n with
        | 0 -> a
        | 1 -> b
        | _ -> helper(n - 1, b, a + b)
    helper(n, 1, 1)

// reverse a list 
let rec naive_reverse l =
    match l with
    | [] -> []
    | x :: xs -> naive_reverse(xs) @ [x]

// tail recursive: factorials
// pass the result of n * (n - 1) as a parameter 
let tfact n =
    let rec helper(n, m) =
        match n with
        | 0 -> m
        | _ -> helper(n - 1, n * m)
    helper(n, 1)

// factorials with objects -- timing purposes 
let factW n =
    let ni = ref n
    let r = ref 1
    while (!ni > 0) do
        (r := !r * !ni); 
        (ni := !ni - 1)
    !r

(* Timing examples

> wastetime 10000;;
Real: 00:00:01.131, CPU: 00:00:01.131, GC gen0: 187, gen1: 5
val it : int list =
  [9999; 9997; 9995; 9993; 9991; 9989; 9987; 9985; 9983; 9981; 9979; 9977;
   9975; 9973; 9971; 9969; 9967; 9965; 9963; 9961; 9959; 9957; 9955; 9953;
   9951; 9949; 9947; 9945; 9943; 9941; 9939; 9937; 9935; 9933; 9931; 9929;
   9927; 9925; 9923; 9921; 9919; 9917; 9915; 9913; 9911; 9909; 9907; 9905;
   9903; 9901; 9899; 9897; 9895; 9893; 9891; 9889; 9887; 9885; 9883; 9881;
   9879; 9877; 9875; 9873; 9871; 9869; 9867; 9865; 9863; 9861; 9859; 9857;
   9855; 9853; 9851; 9849; 9847; 9845; 9843; 9841; 9839; 9837; 9835; 9833;
   9831; 9829; 9827; 9825; 9823; 9821; 9819; 9817; 9815; 9813; 9811; 9809;
   9807; 9805; 9803; 9801; ...]
> Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0

Pretty fast!

> > fib 20;;
Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0
val it : int = 10946
> fib 30;;
Real: 00:00:00.008, CPU: 00:00:00.008, GC gen0: 0, gen1: 0
val it : int = 1346269
> fib 40;;
Real: 00:00:00.996, CPU: 00:00:00.996, GC gen0: 0, gen1: 0
val it : int = 165580141
> fib 50;;
  ^C ^C
- Interrupt I couldn't wait!
> fib 45;;
Real: 00:00:10.997, CPU: 00:00:10.997, GC gen0: 0, gen1: 0
val it : int = 1836311903

> > tailfib 40;;
Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0
val it : int = 165580141
> tailfib 45;;
Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0
val it : int = 1836311903
> tailfib 50;;
Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0
val it : int = -1109825406

Fast but cannot handle big numbers with ordinary integers.

> > let result = naive_reverse [1 .. 10000];;
Real: 00:00:01.483, CPU: 00:00:01.483, GC gen0: 190, gen1: 6

val result : int list =
  [10000; 9999; 9998; 9997; 9996; 9995; 9994; 9993; 9992; 9991; 9990; 9989;
   9988; 9987; 9986; 9985; 9984; 9983; 9982; 9981; 9980; 9979; 9978; 9977;
   9976; 9975; 9974; 9973; 9972; 9971; 9970; 9969; 9968; 9967; 9966; 9965;
   9964; 9963; 9962; 9961; 9960; 9959; 9958; 9957; 9956; 9955; 9954; 9953;
   9952; 9951; 9950; 9949; 9948; 9947; 9946; 9945; 9944; 9943; 9942; 9941;
   9940; 9939; 9938; 9937; 9936; 9935; 9934; 9933; 9932; 9931; 9930; 9929;
   9928; 9927; 9926; 9925; 9924; 9923; 9922; 9921; 9920; 9919; 9918; 9917;
   9916; 9915; 9914; 9913; 9912; 9911; 9910; 9909; 9908; 9907; 9906; 9905;
   9904; 9903; 9902; 9901; ...]

> let r2d2 = List.rev [1 .. 10000];;
Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0

val r2d2 : int list =
  [10000; 9999; 9998; 9997; 9996; 9995; 9994; 9993; 9992; 9991; 9990; 9989;
   9988; 9987; 9986; 9985; 9984; 9983; 9982; 9981; 9980; 9979; 9978; 9977;
   9976; 9975; 9974; 9973; 9972; 9971; 9970; 9969; 9968; 9967; 9966; 9965;
   9964; 9963; 9962; 9961; 9960; 9959; 9958; 9957; 9956; 9955; 9954; 9953;
   9952; 9951; 9950; 9949; 9948; 9947; 9946; 9945; 9944; 9943; 9942; 9941;
   9940; 9939; 9938; 9937; 9936; 9935; 9934; 9933; 9932; 9931; 9930; 9929;
   9928; 9927; 9926; 9925; 9924; 9923; 9922; 9921; 9920; 9919; 9918; 9917;
   9916; 9915; 9914; 9913; 9912; 9911; 9910; 9909; 9908; 9907; 9906; 9905;
   9904; 9903; 9902; 9901; ...]

Built in list reversal is very fast.

I computed 16! a million times and threw away the answer.  I used a tailrecursive
version and a familiar imperative version with a while loop.  The tail recursive
version was actually faster.

> > for i in 1 .. 1000000 do let _ = tfact(16) in ();;
Real: 00:00:00.022, CPU: 00:00:00.022, GC gen0: 0, gen1: 0
val it : unit = ()
> for i in 1 .. 1000000 do let _ = factW(16) in ();;
Real: 00:00:00.054, CPU: 00:00:00.054, GC gen0: 8, gen1: 0
val it : unit = ()

*)

//                                 //
// IMPERATIVE PROGRAMMING          //
// WITH COLLECTIONS: LISTS, QUEUES //

// iterate and apply function to each element of list 
List.iter (fun x -> printf "%i\n" x) [1 .. 5]
List.iteri (
    fun n -> 
        fun x -> 
            let m = n + 1 in 
                printf "Value at position %i is %i\n" m x 
) [15 .. -2 .. 1]

// n starts at n - corresponds to index of array 
    // x is the list value 

let S = Set.ofList ["alpha"; "beta"; "gamma"; "delta"]

Set.iter (
    fun x -> 
        let m = String.length x in 
            printf "The length of %s is %i\n" x m
) S

// Set behave similarily 

// give type to binary tree 
    // either a leaf 
    // or a Node(pointer to left tree, value, pointer to right tree) 
type intBinTree = Leaf of int | Node of intBinTree * int * intBinTree

// preorder, post order, inorder traversal 
let rec preIter f t =
    match t with
    | Leaf n -> f n
    | Node(left, x, right) -> 
        (f x); 
        (preIter f left); 
        (preIter f right)

let rec postIter f t =
    match t with
    | Leaf n -> f n
    | Node(left, x, right) -> 
        (postIter f left); 
        (postIter f right); 
        (f x)

let rec inIter f t =
    match t with
    | Leaf n -> f n
    | Node(left, x, right) -> 
        (inIter f left); 
        (f x); 
        (inIter f right)

(*
                20
        10              25
    3       27      22      27 

*)
let t0 = Node(Leaf 3, 10, Leaf 17)
let t1 = Node(t0, 20, Node(Leaf 22, 25, Leaf 27))

preIter (fun n -> printfn "%i\n" n) t1
postIter (fun n -> printfn "%i\n" n) t1
inIter (fun n -> printfn "%i\n" n) t1

// another tree example - ListTree 
    // each node connected with nodes in the list 
type 'a ListTree = Node of 'a * ('a ListTree list)

let t3 = Node("x", [])
let t4 = Node("y", [])
let t5 = Node("z", [])
let t6 = Node("u", [])
let t7 = Node("v", [])
let t8 = Node("xx", [t3; t4])
let t9 = Node("yy", [t4; t5; t6; t7])
let t10 = Node("abc", [t8; t9; t6])
let t11 = Node("pqr", [t10; t7; t9])

// depth first traversal on a ListTree graph 
let rec depthFirstIter f (Node(x, ts)) =
    (printf "\n"); 
    (f x);  // print node 
    (List.iter (depthFirstIter f) ts)

depthFirstIter (fun x -> printf " %s " x) t11

//
// queues 
//

open System.Collections.Generic

let qt = Queue<int> ()
(* We could also have done
let qt = new Queue<int> ()
*)

qt.Enqueue 1
qt.Enqueue 2
qt.Enqueue 3
qt.Count
let n = qt.Dequeue
(* This does not produce an item as you may think.  Its type is
val n : (unit -> int) 
So in order to get an integer one has to apply it to () *)

qt.Dequeue ()
(*
> val it : int = 1
*)
let x = n ()
(* "n" has been defined to dequeue from qt so we will get
val x : int = 2
and also
>qt.Count;;
val it : int = 1
*)
 
qt.Peek //lets you see the first item without removing it.


  
             


        

      
  
   

  
