(*
    EXERCISE 11.1
*)
let xs = [2;5;7];;
let rec len xs =
    match xs with
    | [] -> 0
    | x :: xs -> 1 + len xs;;

// (i)
let rec lenc xs c =
    match xs with
    | [] -> c 0
    | _ :: xs -> lenc xs (fun r -> c (1 + r));;

lenc [2;5;7] id;;
lenc [2;5;7] (printf "the answer is '%d' \n");;

// (ii)
lenc xs (fun v -> 2*v);;
// Since the continuation function in this call takes the result of
// 1 + tailLength and doubles it we end up with the list length * 2

// (iii)
let leni xs =
    let rec lenii (xs : int list) acc =
        match xs with
        | [] -> acc
        | x :: xs -> lenii xs (acc + 1)
    lenii xs 0;;
        
leni [2;5;7];;
// The two functions are structured differently
// lenc takes a continuation as an additional parameter
// leni takes an accumulator as an additional parameter
// lenc handles the recursion in the continuation element
// leni handles the recursion in the accumulator element

(*
    EXERCISE 11.2
*)
let rec rev xs =
    match xs with
    | [] -> []
    | x :: xr -> rev xr @ [x];;
// (i)
let rec revc xs c =
    match xs with
    | [] -> c []
    | x :: xs -> revc xs (fun r -> c (r @ [x]));;
    
rev [2;5;7];;
revc [2;5;7] id;;

// (ii)
revc xs (fun v -> v @ v);;
// This returns the reversed list xs appended to itself
// i.e. [2; 5; 7] -> [7; 5; 2; 7; 5; 2]

// (iii)
let rec revi xs acc =
    match xs with
    | [] -> acc
    | x :: xs -> revi xs ([x] @ acc);;
    
revi xs [];;

(*
    EXERCISE 11.3
*)
let rec prod xs =
    match xs with
    | [] -> 1
    | x :: xs -> x * prod xs;;
    
let rec prodc xs c =
    match xs with
    | [] -> c 1
    | x :: xs -> prodc xs (fun r -> c (x * r));;
   
prodc xs id;;

(*
    EXERCISE 11.4
*)
let rec optimized_prodc xs c =
    match xs with
    | [] -> c 1
    | x :: xs -> if x <> 0 then prodc xs (fun r -> c (x * r)) else 0;;
    
let xss = [2;5;7;0];;
optimized_prodc xss id;;