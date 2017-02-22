
// 
// week 2 - comp 302
//

// no methods, only functions
// care: types for assignment, will crash if does not work
// more recursion...

// int * int -> int
// input     // output

// lists -> inductive type
// an empty tree is a tree
    // ie. 0 is the base case

// if t is tree
    // can build new tree 
    // by adding item at root

// if i have 2 trees t1, t2
// and item n 
    // can build a new treee

        (*  
            
           n
        t1   t2

        *)

// not built into f#
    // need type definition 

//       //
// TREES //
//       //

type 'a tree = 
    Empty | Node of 'a tree * 'a * 'a tree

let t1 = Node(Empty, 0, (Node(Empty, 1, Empty)))

    (* 
        
        0
      x   1
        x   x

    *)

let t2 = Node(Node(Empty, 5, Empty), 6, Empty)
let t3 = Node(Node(t2, 2, Node(Empty, 3, Empty)), 4, t1)

let max (n, m) = 
    if(n < m) then  
        m
    else
        n

//
// height of tree
//

let rec height (t: 'a tree) = 
    match t with
    | Empty -> 0
    | Node (l, _, r) ->
        1 + max(height l, height r)

// overview
// we dont care about the key value of a tree
    // recursively obtain the height of the left and right subtree

//
// compute sum of all nodes in (specified int) tree
// similar to code above

let rec sumNodes (t : int tree) = 
    match t with
    | Empty -> 0
    | Node(l, n, r) ->
        n + sumNodes(l) + sumNodes(r)

//                 //
// TREE TRAVERSALS //
//                 //

let showInt n = 
    printf "%i\n" n

let rec inOrder (t: int tree) = 
    match t with
    | Empty -> printf " "
    | Node(l, n, r) ->
        inOrder(l);
        showInt n;
        inOrder(r)

let rec preOrder (t: int tree) = 
    match t with
    | Empty -> printf " "
    | Node(l, n, r) ->
        showInt n;
        preOrder(l);
        preOrder(r);

// post order similar
    // l, r, print 

let isEmpty t = 
    match t with
    | Empty -> true
    | _ -> false

//
// flatten
//

let rec flatten (t: 'a tree) = 
    match t with
    | Empty -> []
    | Node(lft, v, rt) ->
        v :: ((flatten(lft) @ (flatten(rt))))

// returns a list in preOrder form
// original tree does not change (obviously)

// no need to dive deep into recursion
    // accept that flatten will be called on smaller subtrees
    // with terminating Empty tree

// application of tree  

        (*

            +
        x       *
              y   4

        *)

// x + (y * 4)

// Env
    // [(x, 5); (y, 17); (z, 1729); (w, 4104)]

type Exptree =  
    | Const of int
    | Var of char
    | Plus of Exptree * Exptree
    | Times of Exptree * Exptree

type Env = Map<char, int>

let e1 = Const 3
let e2 = Const 4
let e3 = Var 'x'
let e4 = Var 'y'
let e5 = Const 5
let e6 = Var 'z'
let e7 = Plus(e1, e2)
let e8 = Plus(e3, e5)
let e9 = Times(e6, e8)
let e10 = Plus(e9, e7)

let rec eval(e: Exptree, rho: Env) =
    match e with
    | Const n -> n 
    | Var v -> Map.find v rho
    | Plus (e1, e2) ->
        let v1 = eval(e1, rho)
        let v2 = eval(e2, rho)
        v1 + v2
    | Times (e1, e2) ->
        let v1 = eval(e1, rho)
        let v2 = eval(e2, rho)
        v1 * v2

// takes in Exptree 'e' and Environment 'rho'
    // if expression e is a constant -> return constant
    // if e is var -> find int value of e in map 'rho' 

    // if Plus 
        // get value of left subtree
        // recursively call eval on e1 
        // same for right subtree
        // add together
            // could also do
            // eval(e1, rho) + eval(e2, rho)
    
    // same for Times

// note: dont focus on recusive calls that much
    // have faith that function progresses to termination

//                     //
// LIST COMPREHENSIONS //
//                     //

let listofSquares n = 
    [
        for i in 1 .. n do
            yield i * i
    ]

let listofCubes n = 
    [
        for i in 1 .. n ->
            i * i * i
    ]

let rec choose n m = 
    if(m = 0) || (n = m) then 
        1
    else    
        (choose (n - 1) (m - 1)) + (choose (n - 1) m)

let pascalRow n = 
    [
        for i in 0 .. n -> 
            choose n i
    ]

// look at testing primes using list comprehensions

// value restriction
    // cannot return polymorphic value at ground type
    // empty list is polymorphic

//      //
// BSTs //
//      //

type 'key bstree = 
    Empty | Node of 'key * 'key bstree * 'key bstree
//
// find
//

// comparison function less passed as arg
let rec find less x (t: 'key bstree) = 
    match t with
    | Empty -> false
    | Node(y, left, right) ->
        if(less(x, y)) then
            find less x left
        elif(less(y, x)) then
            find less x right
        else            
            true

// recursively finds x in t 
// due to BST properties
    // go left recursively if x smaller than node y 
    // vice versa right

    // if reach empty node 
        // then we know x not in t by BST properties
    
let rec insert less x (t: 'key bstree) = 
    match t with
    | Empty -> Node(x, Empty, Empty)
    | Node(y, left, right) as T ->
        if(less(x, y)) then
            Node(y, (insert less x left), right)
        elif(less(y, x)) then 
            Node(y, left, (insert less x right))
        else 
            T   // do nothing, maybe raise exception?

// insert x into t recursively
// take root node of tree 
    // if value we want to insert 'x' is smaller than y 
        // recursively insert into left subtree
        // Node(y, (insert less x left), right)
    // vice versa for right

    // once we reach Empty
        // insert x as a node

exception EmptyTree 

//
// deletemin
//

let rec deletemin (t: 'key bstree) = 
    match t with
    | Empty -> raise EmptyTree
    | Node(y, Empty, right) ->
        (y, right)
    | Node(y, left, right) ->
        let (z, L) = deletemin(left)
        (z, Node(y, L, right))

// z is the min value, return the new tree 

// overview
// given t, delete min Node
    // terminating cases
        // empty tree -> error
            // cannot delete from empty tree
        // cannot traverse left 
            // return current node (smallest)
            // return right
    
    // recusive call on left subtree
    // extract min 'z' from terminating cases
    // extract right node 'L' from terminating cases
        // build new tree with node y pointing to L and right

let rec delete less x (t: 'key bstree) = 
    match t with
    | Empty -> raise EmptyTree
    | Node(y, left, right) ->
        if(less(x, y)) then
            Node(y, (delete less x left), right)
        elif(less(y, x)) then
            Node(y, left, (delete less x right))
        else
            match (left, right) with
            | (Empty, r) -> r 
            | (l, Empty) -> l 
            | (l, r) ->
                let (z, r1) = 
                    deletemin(r) in Node(z, l, r1)

// overview
// given: fn 'less', node x, tree t 

// if t empty -> error
// if x < y 
    // go left by BST property
// if x > y 
    // go right
// if found (x = y)
    // if left empty, attach right subtree 'r'
        // in place of node y 
    // same for right empty 

    // if left and right subtrees exist 
        // call deletemin on right subtree
        // let the min 'z' be the new node in place of y (deleted)
        // keep the original left subtree (unmodified) 
        // point to new right subtree 'r1' 

//          //
// SET TYPE //
//          //

let s1 = set ["Albert"; "Bowen"; "Caroline"]
let s2 = set [1; 3; 2]
let s3 = set [(1, "Prakash"); (5, "Quentin"); (4, "Rashid")]

// test for equality 
set[1; 2; 3] = set[2; 1; 3; 1] |> ignore
    // true

// convert to list, but set will remove duplicates + rearrange items
Set.toList(set [2; 4; 3; 5; 1]) |> ignore
Set.toList (set [1; 1; 1; 2; 2]) |> ignore

// examples
let profSet = set ["Prakash"; "Greg"; "Luc"; "David"]

// creates new set, does not change old set 
Set.add "Doina" profSet |> ignore 
let newProfSet = Set.add "Doina" profSet

// contain item?
Set.contains "Joelle" profSet |> ignore 

// contain subset?
    // isSubset 'testing set' 'original set'
Set.isSubset profSet (set ["Prakash";"David"]) |> ignore       // false 
Set.isSubset (set ["Prakash";"David"]) profSet |> ignore       // true 

// min/max element?
Set.minElement profSet |> ignore
Set.maxElement profSet |> ignore 

// how many elements?
Set.count profSet |> ignore 

let rockStars = set ["Jimi"; "Mick"; "Robert"; "David"]

// union - combines both sets, no duplicates
Set.union profSet rockStars |> ignore
// intersect - common in both sets 
Set.intersect profSet rockStars |> ignore
// difference 'whats in here' 'but not in here'
Set.difference rockStars profSet |> ignore 

// additional fns 
    // Set.map, Set.filter, Set.exists, Set.forall, Set.fold, Set.foldback

//          //
// MAP TYPE //
//          //

// map from A to B 
    // is a finite subset of A, say D 
    // and a fn m from D to B 
// think of maps as tables 

let profsAges = Map.ofList 
                    [
                        ("Prakash", 32);
                        ("Greg", 26);
                        ("Luc", 36);
                        ("David", 35)
                    ]

let result = Map.find "Prakash" profsAges 

// Map is a type constructor 
type AssocList = Map<string, int>

// Map.find 32      // gives type error 
Map.find "Einstein" profsAges |> ignore // raises exception 
Map.tryFind "Einstein" profsAges |> ignore // returns option 
Map.tryFind "Prakash" profsAges |> ignore 

// addition fns 
    // filter, forall, exists, map, fold, foldback

// following fns expect key as well 
let older (s: string) n = 
    n + 1

let updatesAges = 
    Map.map older profsAges 

//
// example of cash register using Map collections 
//

type ArticleCode = string
type ArticleName = string
type NumItems = int
type Price = int

// create a type 'Register' 
    // Map of <ArticleCode: string, (ArticleName: String, Price: int)>
type Register = Map<ArticleCode, ArticleName * Price> 

// Info: (NumItems: int, ArticleName: String, Price: int)
type Info = NumItems * ArticleName * Price
type InfoSeq = Info list 
type Bill = InfoSeq * Price 

type Item = ArticleCode * NumItems
type Purchase = Item list 

let rec makeBill (reg: Register) (pur: Purchase) = 
    match pur with 
    | [] -> ([], 0)
    | (acode, nitems) :: more ->
        let (aname, aprice) = Map.find acode reg 
        let itemprice = nitems * aprice 
        let (infos, sumbill) = makeBill reg more 
        ((nitems, aname, itemprice) :: infos, itemprice + sumbill)

// overview
// makeBill takes in Register 'reg' and Purchase 'pur'  

// decompose pur
    // (acode, nitems) :: more 
    // recall Purchase: (ArticleCode: String, NumItems: int) list 
    // find name + price of item based on 'acode' and given 'reg' of items 
        // using Map.find 
        // recall Map.find searches based on 'acode'
        // returns (ArticleName: String, Price: int)
    // calculate price of selected item based on quantity 'nitems' 

    // recursively call makeBill on same 'reg' and rest of list 'more'
    // terminating case: empty list
        // infos = [], sumbill = 0
    // cons with infos, add to sumbill 

// demo 

let reg1 = Map.ofList 
            [
                ("ac1", ("Tylenol", 5));
                ("ac2", ("Vitamin D", 7));
                ("ac3", ("Statin", 35));
                ("ac4", ("Nyquil", 12))
            ]

let myPurchase = 
    [
        ("ac3", 2);
        ("ac1", 3);
        ("ac4", 1);
        ("ac2", 3)
    ]












