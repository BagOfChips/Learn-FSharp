
//
// ENVIRONMENT MODEL 
//

// binding: association between name and value 
// scope: part of program where binding is valid 

// two ways:
    // let construct  
    // function application  

// let expressions
    // first part gives binding 
    // indentation determines scope 
// f# versions of syntax:
    // verbose - contains keyword 'in' to show explicitly the scope 
    // lightweight - relies on indentation 

let x = 1
let y = 2
x + y 

// bindings for x, y on same level 
// if wanted binding for y hidden inside x binding 

let result = 
    let x = 1
    let y = 2
    x + y 

// cannot use same name twice 
    // can do inside outer let 

let result2 = 
    let x = 1
    let x = 2
    x 

// verbose syntax 
let x = 1 in 
    let y = 2 in 
        x + y 

// let expression closed
    // once evaluation complete
    // bindings are removed, no binding reported 

let x = 1 in 
    let x = 2 in 
        x + x 
// int = 4 
// inner binding masks outer binding 

let x = 1 in 
    let y = x in 
        let x = 2 in 
            x + y 
// int = 3 
// bound name

// free name: name that does not have binding 
// relative to expression
    // name is free in an expression when there is no binding
    // within that expression

// there will be a binding for the name from an outer expression
let y = x in 
    let x = 2 in 
        x + y 
// x appearing in 1st line is free
    // x appearing in line 3 bounded by binding in line 2 
    // expression by itself -> error 

// substitution model 
// function application creates binding 

// when fn f with single param x is applied to expression e 
    // the evaluation proceeds:
        // expression e evaluated to produce value v 
        // free occurrrences of x in body of f 
            // replaced with v 
        // resulting expression is evaluated  

// evaluation rules based on substitutions 
    // formalized by inductive definition 
// advantage of substitution model
    // easy to use in understanding programs 
    // provides high level abstract view of how programs are eval 

// drawbacks 
    // not actually what happens 
// impelementing language literally using sub method:
    // if x occured multiple times in e 
    // copy value of v multiple times 

// more efficient 
    // just remember correspondence between x and value v 
    // if needed during evaluation of expression e 
    // look it up 

// other drawbacks
    // does not easily extend to references and assignment 

// introduce ENVIRONMENT MODEL 
// provide view of eval where structure 'environment' 
    // keeps track of correspondence between name and value 

// terminology  
    // binding: association between name and value 
    // name can name a value of any type    
        // int, list, or memory location (reference) 
    // let x = 10 in x + 3
        // encounter binding between name x and value 10 
    // let square = (fun x -> x * x)
        // binding between name of function 'square' 
        // and input arg x, and function body x * x 

    // frame: collection of zero or more bindings
        // and pointer to another frame
        // called enclosing env 
    // env: structed collection of frames 
        // starting from particular frame
        // going back through each frame's enclsing env 
        // until global env reached 
    
// in env model
    // expression always evaluated in context of particular env 

// env determines what values corresponds to names 
    // occurring in expression 
// purpose - provide way to associate value with particular name 

// first frame in env searched to see 
    // if contains binding for name 
    // yes -> associated value used 
    // no -> first frame searched, and so on to global env 
    // if frame not found -> error reported 
    // evaluator never follows pointer backwards 

// 3 kinds of bindings
    // to int, float, etc... 
    // to function 
    // to location in memory 
// fn complicated, not just body of function   
    // complex entity 'closure' 

// binding represented by box w/ 2 parts
    // left part - name of binding
    // right - contains value if value is int, boolean, etc 
        // fns later 

// to look up binding for x 
    // follow pointers until find first binding for it 

let x = 1 in
    let x = 2 

(* 
    [x|1]   <--
              |   
            [x|2]
*)

// first x declared with let x = 1
    // inside scope of this binding
    // another declaration for x 
// not changing value of x with second declaration 
    // creating new x with new binding in its own frame 
    // both bindings exist at same time 

let x = 1 in  
    let y = 2 in 
        x + y 
    
(*
    x|1
    ^
    |
    y|2
      ^
      | 
    x + y 
*)
 
// arrows show pointers from frame to enclosing frame
    // need bindings for x and y to evaluate expression x + y 
    // follow arrows looking for bindings in each frame 

// function evaluation 
let x = 10 in 
    let square n = n * n 
    square x  

(*         <--------------------------        
    x|10   <---------------          |   
    ^---------            |         n|10
             |            |         ^
    square|param: n       |         |
          |body: n * n ---          n * n 
*)

// try to eval square x 
// expression inside inner let 
    // sees both frames above 
    // evaluator first looks for 'square' 
        // finds it, indeed a fn 
    // then look for x 
        // not in frame, follows pointer up 
            // finds x 
    // look for parameter name in fn: 'n'
    // creates new frame 
        // binds n to result it got from evaluating x 
        // frame temp, removed when evaluation over 
    // body evaluated in env 

// why does arrow from temp fram for n point to frame x 
    // rather than frame for square 
    
// new frame points to same place as the pointer from fn 
    // if inside body of square there were another reference to square 
    // error 

// recursion 
let rec fact n = 
    if n = 0 then 
        0
    else 
        n * fact(n - 1) 

// let rec signals arrow to be sent back to frame being created 

// consider fact 2 
(*
    fact|param: n 
    ^   |body:...    <---
    |      |            |
    -------             |
                        n|2
                        ^
                        |
                        n|1
                        ^
                        |
                        n|0
*)

// bind input arg n to fact of 2 
    // top most binding in frame 
    // executing body of fact 
        // if 2 = 0 then 1 else 2 * fact(1) 
    // call fact recursively 
        // now input arg bound to 1 

// establish another binding for n 
    //which will point to previous binding where n was 2 
// each recursion step,
    // keep track of binding between input arg and current value it's bound to 
    // until reach final value 

// local scopes 
    // created with let  
    // delimited by let keyword 
// when expression evaluated, let bindings removed 
    // local / temp 

let x = 1 in 
    let y = x in 
        let z = 2 in 
            y + z 

// each nested let creates new layer 
    // do NOT store binding as y:x 
    // when binding for y established 
        // system evaluates x and finds value 1 
    // if later frames define bindings for x 
        // bindings for y do not change 
    // called static binding 

let x = 1 in 
    let y = x in 
        let x = 2 in 
            x + y 

(*
    x|1
    y|1
    X|2
*)
// eval x + y, x  is looked up 
    // latest value 2 is seen 
    // y is bound to same value as it was when it was set up 
    // bindings do not change 
    // static
 
let x = 1 in 
    let foo n = n + x in 
        let x = 2 in 
            foo x

// gives 3 
    // when fn foo defined 
        // contains pointer to env that exists at time it is created 
        // only binding for x is (x, 1) at that point 
    // even when foo is called with new binding
    // env inside closure for foo   
        // will not have this new binding
// when foo x evald
    // name foo looked up first
    // found to be a fn with one param 
    // frame is set up for param 
        // pointer from this frame foes to wherever
        // the pointer from definition of foo goes 

(*
    x|1   <--------------------
    ^ ^------------           |
    -----         |           |
        |         |           |
    foo|n         |           |
       |n + x ----|           n|2
    ---^                      ^
    |                         |
    x|2  <-                 n + x 
          |
          |
        foo x 
*)

// functions are called by value 
    // before body can be executed
    // arg evaluated first 
        // x is looked up and value 2 returned 
    // 2 is bound to n 
        // new frame created 
        // will be removed when fn eval complete 
        // placed on top of a copy
            // of env in closure 

// evaluating body: n + x 
    // 2 names looked up 
    // n found -> 2 
    // x found -> 1 
        // binding (x, 2) will not be seen 

let x = 1 in 
    let f = 
        (let u = 3 in (fun y -> u + y + x)) in 
    let x = 2 in 
        f(x) 

(*
    x|1
 ---^
 |  --- 
 |    |
 |  f|?
 |
 |--u|3
*)

// frame for binding (x, 1) set up 
// definition of f not complete 
// evaluate expression on right side of 'let f = ...'
    // starts with let u...
// frame for binding (u, 3) set up 
    // points to frame that exists (top)
    // binding for f no yet set up at this time 

// construct closure for f 

(*
        x|1
    ---^
    |  --- 
    |    |
    |  f|y 
    |   |u + y + x ----
    |                 |
    |                 |
    |                 |
    |--u|3  <----------
*)

// frame (u, 3) is latest frame
    // closure of f points to this frame
    // 'traps the frame'

// frame for inner x binding is set up 
    // let binding for u is closed 
        // still trapped inside closure of f 
    // pointer goes to frame for f 

(*
        x|1
    ---^
    |  --- 
    |    |
    |  f|y 
    |   |u + y + x ----
    |                 |
    |                 |
    |                 |
    |--u|3  <----------

    x|2 (points to f)
*)

// now make fn call f(x) 
    // evaluation of x gives 2 
    // new temp frame binding (y, 2) set up 
        // points to whatever the closure of f points to f 

(*
        x|1
    ---^
    |  --- 
    |    |
    |  f|y 
    |   |u + y + x ----
    |                 |
    |                 |
    |                 |
    |--u|3  <----------
        ^ 
        |
        |    
        y|2  <------ u + y + x 

    x|2 (points to f)
*)
// result = 6 

// summary 
    // env is structured collection of frames 
    // each frame is box (possibly empty) of bindings 
        // associates names with values     
        // single frame contains at most 1 binding for any var 
    // each frame points to enclosing env 
    // value associated with name wrt env 
        // is value given by binding of name 
        // in the first frame
        // in env that contains binding for that name 
    // if no frame in collection 
        // specifies binding for name 
        // name is 'unbound'
    


