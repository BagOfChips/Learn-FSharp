
//
// OBJECTS AS CLOSURES 
//

// basic objects 
// object: package of data and code together 
    // data associated with vars
    // code associated with methods 

// there can be many instances of the same kind of object 
// each of these objects
    // have their own copies of data 
    // may be updated separately / distinctly in each object 

let mutable a = 0
let flip0 () = 
    (a <- (1 - a));
    (printf "%i\n" a) 

flip0 ()    // 1
flip0 ()    // 0 

// once can have a fn with internal state 
    // fn flip0 accesses global var and updates it 
// behaviour of fn is time dependant 

// state: description of all values in memory cells 
// program above not well protected
    // anyone can mess with global var 

a <- 3 
flip0
flip0 ()    // -2 

// package 'a' with code so its not globally visible
let flop() = 
    let mutable a = 0 in
        (a <- (1 - a));
        (printf "%i\n" a)

flop()      // 1
flop()      // 1 

// this does not work 
// no1 can touch internal var 
    // upon running
    // let is executed afresh
    // name a reinitialized 
// upon finish, name a cannot be accessed -> garbage collected 

// need internal to be local (private) and persistent 
// how to do?
// need function to be constructed inside scope of let 
    // closure will trap binding -> makes permanent 

type counter = {
    mutable i: int
}

let flip = 
    let c: counter = {
        i = 0
    }
    fun () -> 
        (c.i <- 1 - c.i);
        (printf "%i\n" c.i) 

flip()  // 1
flip()  // 0

// want way to make flipper objects 
let makeFlipper =
    fun () ->
        let c: counter = {
            i = 0
        }
        fun () ->
            (c.i <- 1 - c.i);
            (printf "%i\n" c.i)

let flipPancakes = makeFlipper() 
let flipEggs = makeFlipper()

flipEggs()      // 1
flipEggs()      // 0

flipPancakes()  // 1
flipEggs()      // 1

// binding introduced by 'let' construct 
    // supposed to die when exit at the end 
// if name cannot be reference then storage associated with it 
    // is garbage collected 
// when exit a let block -> this happens naturally 

// if let is active when function is being defined
    // binding that set up by let becomes part of closure 
    // of a function and persists 

// part of essence of objects: private local vars that persist 
// notice
    // flip object can be made by generator 
    // constructor method: class definition will have generator

// adding methods 

// want to set up bank accs with private var 'balance'
// accounts should also respond to methods
    // check balance, deposit, withdraw 

// define bank acc that can only withdraw 

let withdraw = 
    let mutable balance = 100 
    fun amount -> 
        if(amount < balance) then 
            balance <- balance - amount; 
            (printfn "Balance is %i" balance)
        else
            printfn "Insufficient funds."

withdraw(20)    // 80
withdraw(30)    // 50

// persistence exists
    // balance is remembered 
    // nothing we can do to alter balance, except using the withdraw fn 
        // privacy exists 
// however, only 1 acc 

// want generator of accs 
// add power of multiple transactions to give our object multiple methods 

// datatype for transactions 
type transaction = Withdraw of int | Deposit of int | CheckBalance 

// define generator that pattern matches constructor fn 
let makeAccount (opening_balance: int) = 
    let balance = ref opening_balance 
    fun (t: transaction) ->
        match t with 
        | Withdraw(m) ->
            if(!balance > m) then
                balance := !balance - m 
                printfn "Balance is %i" !balance 
            else 
                printfn "Insufficient funds."
        | Deposit(m) -> 
            (balance := !balance + m; (printf "Balance is %i\n" !balance))
        | CheckBalance ->
            (printf "Balance is %i\n" !balance)

// example with 2 independent accounts 
let AliceAccount = makeAccount (100)
let BowenAccount = makeAccount (100)
let EmilyAccount = makeAccount (200)

BowenAccount(Withdraw 50)   // 50

EmilyAccount(Deposit 25)    // 225

AliceAccount(CheckBalance)  // 100 

// wrappers 
// example
    // monitoring: keep track of number of times a fn called 
// dont rewrite code
    // embed it inside wrapper
    // can do with higher-order fns 

type 'a tagged = Query | Normal of 'a
type 'b answers = Numcalls of int | Ans of 'b 

// monitored fn will behave like object 
    // respond to diff types of messages (have diff methods)
// types above used as follows
    // monitored fn must still do whatever it was designed to do 
    // tag the 'normal' input 
        // with constructor fn 'Normal'
// want to ask how often it was called 
    // use special input 'Query' 
    // put together in one type called 'a tagged 
// answers may be of usual type
    // or a response to query 
    // combine these types into one 
        // 'a answers 
    
// code for making fn monitored 
let makeMonitored f = 
    let c = ref 0
    fun x -> 
        match x with
        | Query -> (Numcalls !c) 
        | (Normal y) ->
            (c := !c + 1; (Ans (f y)))

// notice that it takes fn f as input 
    // returns fn as result 
// this fn has trapped var 
    // used to track how often it was called 

// simple increment fn -> monitored function 
let inc n = n + 1
let moninc = makeMonitored(inc)

moninc (Normal 3)       // answers = Ans 4 
moninc (Normal 4014)
moninc (Normal 1137) 
moninc Query            // answers = Numcalls 3 

// note that makeMonitored fn does not need 
    // to know anything about its arg 
    // except its type 
// is polymorphic 

// non polymorphic version 
let monLength = makeMonitored (fun (l: int list) -> (List.length l))

monLength (Normal [1 .. 17])
monLength (Normal [3 .. 4 .. 25])
monLength Query 

// can even use monic inside map 
    // tag everything in list first 
let taglist l = List.map (fun x -> Normal(x)) l 

let test = taglist [1 .. 2 .. 10]
List.map moninc test 
moninc Query    







