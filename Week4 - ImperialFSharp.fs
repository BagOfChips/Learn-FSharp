
//                            //
// IMPERIAL PROGRAMMING IN F# //
//                            //

// functional paradigm
    // framework - basic entities:
        // expressions and values 
    
// values: special expressions at end result of computation
// evaluation: process of changing general expression -> value 
    // no values ever modified

// sort a list process 
    // create new, sorted copy of original list

// binding: associate name with value (let) 
// scope: expression where binding is in force 
// environment: correspondence between names and values 

// fundamental new semantic entity 
    // commands

// instruction: order to do something
// basic: change value of variable 

// new kind of data - location
// memory location

// address of something in memory which can be modified 
// assume we have access to unlimited supply of memory cells : locations

// how to indicate name denotes location (rather than value)
let mutable x = 1

// x is mutable and of type int 
    // x: name of memory cell that stores ints 
    // x is NOT 1
    // x is name of memory cell that stores 1 at that time 

// change value stored in cell, but x always mean the same cell 
x 
x <- 2
x 

// value stored changed
    // association of x and cell not changed
// environment: correspondence between names and values 
    // include locations and values 

// new mapping: store is map from locations to values 
    // can update assignment
    // cannot update binding

// create / destroy bindings by entering / exiting new scopes 
    // or making function calls 
// cannot rebind same name to new value 

let mutable lst = [1; 2; 3; 4]
lst <- [5; 6; 7]

let mutable f = cos
f 1.57 

let mutable (u, v) = (2, "abc") 
v <- 3 
// error 
v 

v <- "foo"
v 

// basic update command: exp1 <- exp2 
// evaluation rule:
    // evaluate exp1, verify that result is a location 
    // evaluate exp2, verify that value obtained has type appropriate
        // to location 
    // replace contents of location from step 1
        // with value in step 2

// assignemnet destorys old value  
    // programmer has control over lifetime of data 
    // gets to decide whether value is needed any more 
    // makes choice to reuse storage cell 
    // could not do with functional programming

// name of variable means 2 different things 
    // depends where it appears in assignment statement

// x <- x + 1
// x on left: location denoted by x
// x on right: value stored in location 

// do not need special syntax for 'contents of' 
    // automatic for names on right hand side of assignment statments 
    // auto dereferencing 

// functions w/ mutable values 
let modify n = 
    n <- n + 1
// error: value not mutable 

// cannot do 'let modify (mutable n: int) = ...'

// use mutable values in functions? 
    // x declared mutable 
let mess_with () = x <- x + 1 
x 
mess_with () 
x 

// local variables? 
let munge (n: int) = 
    let mutable m = n 
    (m <- m + 1) 

// useless, what happens to m is hidden to outside world 
    // when munge exists, local bindings thrown
    // any changes / the existence of m is invisible 

// if x declared mutable and global to function definition 
    // can see effects

// need some way to see effects 
// return to mutable local vars 

// command: special expression that returns 'unit' 
// several commands in row?
    // sequential composition
    // done with ';' 
    // sequence of commands ending with expression
        // last value returned 
let result = (printfn "x is %i" x); (mess_with()); x 
// prints 'x is 3'
// var result : int = 4 

// local variables 
// print to show that mutable variables are being changed?
let mash (n: int) = 
    let mutable m = n 
    (printfn "m is %i" m);
    (m <- m + 1); 
    (printfn "n is %i" n); 
    (printfn "m is %i" m); 
    m 

mash 5  

// while loops 
// b is boolean expression
// e is expression of any type 
// while b do e : expression of type unit  
    // always returns unit even if e has another type 
let foo n = 
    let mutable x = n in 
        while (x < 10) do 
            (printfn "x is %i" x);
            (x <- x + 1) 

foo 5 

// cannot pass mutalbe values as args 
// must use records 

type intRec = {
    mutable count: int 
}

let r1 = { count = 0 }
r1.count <- 1729 

let increment (counter: intRec) =   
    counter.count <- counter.count + 1
    counter.count 

increment r1 
increment r1  

// if want single variable as mutalbe var that can be passed as arg 
// f# provides shorthand to do so 

// define reference type to be a record type with one mutable field 'contents' 
type counter = int ref 
// equivalent to  
(*type counter = {
    mutable contents: int 
}*)

// following equivalences 
type 'a ref = {
    mutable contents: 'a 
}
let ref v = {
    contents = v 
}

// declare 'let x = ref int'
// access data by writing x.contents 
    // also write as !x 
// can update contents by x := x + 17 
    // instead of x.contents <- ... 

//type counter = int ref 
let i = ref 0 

let increment (c: counter) = 
    c := !c + 1 

increment i 
i 

// aliasing: 2 names refer to same data 
let j = i 
increment i 
j // int ref = {contents = 2;}

// when declare record like i 
    // name i associated with pointer / reference to actual record
    // contains address in memeory where record actually resides 
// binding 'let j = i'
    // new name j is created 
    // bound to the result obtained by evaluating i 
    // value is address of record
    // same address associated with j 
// i, j aliases for same record 




