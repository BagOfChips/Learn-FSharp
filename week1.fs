
//
// in class recursive functions
// jan 14

let rec fact n = 
    if n = 0
        then 1
    else  
        n * fact(n - 1)

// test standard factorial method
let fact4 = fact 4
    // 4 * fact 3
    // 4 * 3 * fact 2
    // 4 * 3 * 2 * fact 1
    // 4 * 3 * 2 * 1 * fact 0
// terminating case: n = 0
    // 4 * 3 * 2 * 1 * 1 = 24

//                //
// TAIL RECURSION //
//                //

let rec fastfact(n, m) = 
    if n = 0
        then m
    else fastfact(n - 1, n * m)

// tail rec 
    // only 1 rec
    // rec call outermost
let fastfact4 = fastfact(4, 1)
    // ff(4, 1)
    // ff(3, 4)
    // ff(2, 12)
    // ff(1, 24)
// terminating case: n = 0
    // ff(0, 24) -> 24

// let the recursive function be inside another function
    // less tampering with code
let iterfact n =
    let rec helper(x, y) = 
        if x = 0
            then y
        else 
            helper(x - 1, x * y)
    // call the helper for a result
    helper(n, 1)

let iterfact4 = iterfact 4

//
// russian peasant exponentiation 
//

let rec rpe(b, power) = 
    if(b = 0)   
        then 0
    elif(power = 0)
        then 1
    elif(power = 1)
        then b
    elif(power % 2 = 1)
        then b * rpe(b, power - 1)
    else 
        let temp = 
            rpe(b, power / 2) in
            temp * temp

// test rpe
let fourE8 = rpe(4, 8)
    // base cases:
        // b = 0, then b^power = 0
        // power = 0, then b^0 = 1
        // power = 1, then b^1 = b

    // if power if odd
        // do b * rpe(b, power - 1)
        // which is: b * b^(power - 1) = b^power
    
    // if power if even
        // which means power % 2 = 0
        // power / 2 -> int
        //do rpe(b, power / 2)
            // square that value
            // which is: (b^(power / 2))^2 = b^power
    
    // best case
        // power = 2^m, for arbitrary m
        // function will run in m steps = log(power)
        // since we can divide m by 2 each recursive call

    // worst case
        // if power odd
        // can never be 2 consecutive rec call on odd case
            // odd power - 1 -> even int
            // suppose every even int / 2 -> odd int
                // half rec calls: even
                // half rec calls: odd 
            // can only have log(power) number of evens
            // thus, log(power) number of odds
        // O(log(power) + log(power))
        // O(2log(power))
        // O(log(power))

//
// sqrt by guessing
// mainly to demonstrate recursion example

let delta = 0.0001

let square u: float = 
    u * u

let close guess x = 
    (abs((square guess) - x)) < delta

let update guess x = 
    (guess + x / guess) / 2.0

let rec mySqrt x guess = 
    if(close guess x) 
        then guess
    else (mySqrt x (update guess x))

// overview of mySqrt
    // want to find the sqrt of x
    // check if guess if close enough
        // guess^2 =?= x
        // if within 0.0001 -> yes
    // or else update original guess
        // rec call mySqrt with updated guesses
let sqrt10 = mySqrt 10.0 1.0

// helper functions should not be public
let rec mySqrt2(x, guess, error: float) = 
    let close(guess, x) = 
        (abs((square guess) - x)) < error

    let update(guess, x) = 
        (guess + x / guess) / 2.0
    
    if close(guess, x)
        then guess
    else mySqrt2(x, update(guess, x), error)

let newsqrt10 = mySqrt2(10.0, 1.0, 0.0001)

//       //
// LISTS //
//       //

let rec badzip(l1, l2) = 
    if l1 = []
        then l2
    else (l1.Head) :: (badzip(l2, l1.Tail))

// overview
    // if l1 is empty, return l2
    // else take the first element from l1
        // cons the "first element" recursively to the "next first item"
let l1 = [0..2..8]
let l2 = [1..2..9]

let mergedList = badzip(l1, l2)

//                  // 
// PATTERN MATCHING //
//                  //

let rec zip(l1, l2) = 
    match l1 with 
    | [] -> l2
    | x :: xs -> x :: zip(l2, xs)

// exact same code, this time using matching
let mergedListWMatching = zip(l1, l2)

//                //
// INSERTION SORT //
//                //

let rec insert n lst = 
    match lst with 
    | [] -> [n]
    | x :: xs -> 
        if n < x 
            then n :: lst
        else 
            x :: (insert n xs)

// overview
    // suppose we want to insert an element 'n' into an already sorted list
    // if the list is empty
        // then just pluck n in
    // or else take the first element of list 'x'
        // if n < x 
            // cons n with the "entire list"
            // ie. place n before x
        // else x :: (recursively call insert with the remaining list)
            // this goes through the list recursively 
                // checks if the first element of each sublist is > n
    
let rec isort lst = 
    match lst with 
    | [] -> []
    | x :: xs -> insert x (isort xs)

let unsortedList = [3; 1; 4; 1; 5; 9; 2; 6]
let sortedList = isort unsortedList    

// overview of insertion sort
    // we know insert places an element 'n' 
        // such that n precedes x in list (if x >= n)

    // isort 
        // if list is empty, return empty list
        // take the first element of list 'x'
        // insert x into (recursively call isort on rest of list xs)
    
    // I believe the algorithm breaks the original list 
        // into a list with 1 item
        // list of 1 is already sorted
        // so we can insert x into xs
            // inserting maintains sorted list
            // insert "next x" and so on...

//
// append to list
//

let rec append(l1, l2) = 
    match l1 with
    | [] -> l2
    | x :: xs -> x :: (append(xs, l2))

// there is no instant way to appending an element/list to a list
    // ie. we have to traverse to the end of the list we wish to append to
        // then point to the first element of the list we wish to add using cons

let l3 = append(l1, l2)

// we can also use the '@' operator
let l3Alt = l1 @ l2

// 
// list reversal
//

let rec rev l = 
    match l with
    | [] -> []
    | x :: xs -> rev(xs) @ [x]

let l3rev = rev l3

// recursively go through the list
    // once there are no elements left
    // append the "stack"
        // "last in, first out"

// the method above is NOT efficient however - O(n^2)
    // implement with tail recursion

let rev2 l = 
    let rec helper(l1, l2) = 
        match l1 with
        | [] -> l2
        | x :: xs -> helper(xs, x :: l2)
    helper(l, [])

// overview
    // if l1 empty -> l2
    // else recursively call helper on remaining list 'xs'
        // cons x with l2
    
    // takes first element of (sub)list
        // cons with the "last first element"

    // resulting algo - O(n)

let l3Altrev = rev2 l3

let rec sumprod l = 
    match l with
    | [] -> (0, 1)
    | x :: xs -> 
        let(s, p) = sumprod(xs)
        (x + s, x * p)

// overview
    // given list 'l'
    // compute sum and product using all elements of the list

    // terminating case for rec:
        // no more elements in sublist
        // add 0, multiply by 1 (does not change return value)

    // take the first element of list 'x' recursively
        // once we reach the last element
        // sumprod(xs) gives (s, p) = (0, 1)
        // do "last first element" 'x' + s, 'x' * p
        // this is our new (s, p), continue...

let l4 = [1; 2; 3; 4]
let sumprodl4 = sumprod l4

//
// list.map 
//

let rec mymap f l = 
    match l with 
    | [] -> []
    | (x :: xs) -> (f x) :: (mymap f xs)

// mymap applies a function to every element of a list
// take the first element of list 'l' recursively
    // call f on 'x', then cons with remaining...

// this is tail recursive as well
    // :: 'cons' fns dont have to wait for second arg 
        // just point to rest of list
    
// 
// filtering
//

let rec myfilter test lst = 
    match lst with
    | [] -> []
    | x :: xs -> 
        if(test x)
            then x :: (myfilter test xs)
        else (myfilter test xs)

// overview
    // args: function 'test', list 'lst'
    
    // recusively get the first element of lst
        // filter function test
        // if(test x)
            // ie. condition holds
            // keep x in the list using cons
        // else 
            // recursively call myfilter on sublist
            // ie. x is thrown out
    
    // return: 'filtered' list

// 
// folding
//

let rec myfold f v l = 
    match l with
    | [] -> v
    | x :: xs -> 
        myfold f (f v x) xs

// overview
    // args: function 'f', value 'v', list 'l'

    // terminating case for recursion:
        // empty list -> value v (return v at the end)

    // recursively get first element of list
        // 'next' v becomes f v x
        // recursively call myfold on sublist 'xs' with new v
            // note that f v x changes the value of v every time
            // v gets returned at the end

    // useful for things like computing summation/averages

    // lets use an example to demonstrate how this works

// example - count the total number of animals 
let data = [
    ("Cats", 4);
    ("Dogs", 5);
    ("Mice", 3);
    ("Elephants", 2)
]

let count = myfold (fun acc (nm, x) -> acc + x) 0 data
printfn "Total number of animals: %d" count

// myfold f 0 data
// mf f 4 dme
// mf f 9 me
// mf f 12 e
// mf f 14 []
    // return v = 14



