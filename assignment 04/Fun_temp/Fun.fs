(* File Fun/Fun.fs
   A strict functional language with integers and first-order 
   one-argument functions * sestoft@itu.dk

   Does not support mutually recursive function bindings.

   Performs tail recursion in constant space (because F# does).
*)

module Fun

open Absyn

(* Environment operations *)

type 'v env = (string * 'v) list

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

(* A runtime value is an integer or a function closure *)

type value = 
  | Int of int
  | Closure of string * string * expr * value env       (* (f, x, fBody, fDeclEnv) *)

let rec eval (e : expr) (env : value env) : int =
    match e with 
    | CstI i -> i
    | CstB b -> if b then 1 else 0
    | Var x  ->
      match lookup env x with
      | Int i -> i 
      | _     -> failwith "eval Var"
    | Prim(ope, e1, e2) -> 
      let i1 = eval e1 env
      let i2 = eval e2 env
      match ope with
      | "*" -> i1 * i2
      | "+" -> i1 + i2
      | "-" -> i1 - i2
      | "=" -> if i1 = i2 then 1 else 0
      | "<" -> if i1 < i2 then 1 else 0
      | _   -> failwith ("unknown primitive " + ope)
    | Let(x, eRhs, letBody) -> 
      let xVal = Int(eval eRhs env)
      let bodyEnv = (x, xVal) :: env
      eval letBody bodyEnv
    | If(e1, e2, e3) -> 
      let b = eval e1 env
      if b<>0 then eval e2 env
      else eval e3 env
    | Letfun(f, params, fBody, letBody) ->
      //untested solution to letfun of string * string list * expr * expr
      let closureList = List.fold (fun a x -> (f, Closure(f, x, fBody, env)) :: a) [] params
      let bodyEnv = closureList @ env
      eval letBody bodyEnv
      
      (* Given code *)
      // let bodyEnv = (f, Closure(f, x, fBody, env)) :: env 
      // eval letBody bodyEnv

      (* our solution to letfun of string * string list * expr list * expr *)
      (* note:turns out letfun should be string * string list * expr * expr *)
      // let tuples = List.zip params fBodies

      // let makeBodyClosureList list env =
      //   match list with
      //   | [] -> list
      //   | x :: xs -> Closure(f, fst x, snd x, env) :: makeBodyClosureList xs env

      // let bodyEnvClosures = makeBodyClosureList tuples env
      
      // let makeBodyEnv closures env =
      //   match closures with
      //   | [] -> env
      //   | x :: xs -> (f, x) :: env

      // let bodyEnv = bodyEnvClosures env
      // eval letBody bodyEnv

    | Call(Var f, eArgs) ->
      let fClosure = lookup env f
      match fClosure with
      | Closure (f, x, fBody, fDeclEnv) ->
        let xVals = List.fold (fun a x -> Int(eval x env) :: a) [] eArgs
        let fBodyEnv = List.zip (List.replicate (List.length xVals) x) xVals
        // let fBodyEnv = List.zip x xVals
        let fBodyEnvv = fBodyEnv @ (f, fClosure) :: fDeclEnv
        eval fBody fBodyEnvv
      | _ -> failwith "eval call: not a function"

      // match eArgs with
      // | [] -> env
      // | a :: b :: ls ->
      //   let fClosure = lookup env a
      //   match fClosure with
      //   | Closure (f, x, fBody, fDeclEnv) ->
      //     let xVal = Int(eval b env)
      //     let fBodyEnv = (x, xVal) :: (f, fClosure) :: fDeclEnv
      //     eval fBody fBodyEnv
      //   eval ls env
      //   | _ -> failwith "eval call: not a function"

    //kalder vi eval rigtigt
    //hvordan tester vi (FunPar.fsy, string list)
    
    //hvor skal man overhovedet lave 4.4 FunPar.fsy -> AppExpr
    //Er AppExpr rigtigt defineret i AppExpr -> kan man godt [$1, $2]

    //4.5 se funlex, funpar. er det rigtigt med "true"/"false"

    // | Call(Var f, eArg) -> 
    //   let fClosure = lookup env f
    //   match fClosure with
    //   | Closure (f, x, fBody, fDeclEnv) ->
    //     let xVal = Int(eval eArg env)
    //     let fBodyEnv = (x, xVal) :: (f, fClosure) :: fDeclEnv
    //     eval fBody fBodyEnv
    //   | _ -> failwith "eval Call: not a function"
    | Call _ -> failwith "eval Call: not first-order function"

(* Evaluate in empty environment: program must have no free variables: *)

let run e = eval e [];;


(* EXAMPLES COMMENTED OUT SINCE THEY DONT ADHERE TO NEW DEFINITONS OF LETFUN AND CALL*)
(* Examples in abstract syntax *)

// let ex1 = Letfun("f1", "x", Prim("+", Var "x", CstI 1), 
//                  Call(Var "f1", CstI 12));;

// (* Example: factorial *)

// let ex2 = Letfun("fac", "x",
//                  If(Prim("=", Var "x", CstI 0),
//                     CstI 1,
//                     Prim("*", Var "x", 
//                               Call(Var "fac", 
//                                    Prim("-", Var "x", CstI 1)))),
//                  Call(Var "fac", Var "n"));;

// (* let fac10 = eval ex2 [("n", Int 10)];; *)

// (* Example: deep recursion to check for constant-space tail recursion *)

// let ex3 = Letfun("deep", "x", 
//                  If(Prim("=", Var "x", CstI 0),
//                     CstI 1,
//                     Call(Var "deep", Prim("-", Var "x", CstI 1))),
//                  Call(Var "deep", Var "count"));;
    
// let rundeep n = eval ex3 [("count", Int n)];;

// (* Example: static scope (result 14) or dynamic scope (result 25) *)

// let ex4 =
//     Let("y", CstI 11,
//         Letfun("f", "x", Prim("+", Var "x", Var "y"),
//                Let("y", CstI 22, Call(Var "f", CstI 3))));;

// (* Example: two function definitions: a comparison and Fibonacci *)

// let ex5 = 
//     Letfun("ge2", "x", Prim("<", CstI 1, Var "x"),
//            Letfun("fib", "n",
//                   If(Call(Var "ge2", Var "n"),
//                      Prim("+",
//                           Call(Var "fib", Prim("-", Var "n", CstI 1)),
//                           Call(Var "fib", Prim("-", Var "n", CstI 2))),
//                      CstI 1), Call(Var "fib", CstI 25)));;
                     
