
(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

type expr = 
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  | If of expr * expr * expr;;
    
let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;

(* Exercise 1.1 II *)
let e4 = Prim("max", Prim("min", Prim("==", Var "a", Var "b"), Var "b"), Var "a");;

(* Exercise 1.1 V *)
let e5 = If(Var "a", CstI 11, CstI 22)


(* Evaluation within an environment *)

let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim("max", e1, e2) -> if eval e1 env > eval e2 env then eval e1 env else eval e2 env 
    | Prim("min", e1, e2) -> if eval e1 env < eval e2 env then eval e1 env else eval e2 env
    | Prim("==", e1, e2) -> if eval e1 env = eval e2 env then 1 else 0
    | Prim _            -> failwith "unknown primitive"
    | If (e1, e2, e3) -> if eval e1 env <> 0 then eval e2 env else eval e3 env;;

(* Exercise 1.1 III *)
let rec eval2 e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x
    | Prim(ope, e1, e2) ->
        let i1 = eval e1 env
        let i2 = eval e2 env
        match ope with
        | "+" -> i1 + i2
        | "*" -> i1 * i2
        | "-" -> i1 - i2
        | "max" -> if i1 > i2 then i1 else i2
        | "min" -> if i1 < i2 then i1 else i2
        | "==" -> if i1 = i2 then 1 else 0
        | _ -> failwith "unknown primitive"
    | If (e1, e2, e3) -> if eval e1 env <> 0 then eval e2 env else eval e3 env;;
    

let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;

(*Exercise 1.2*)
type aexpr =
    | CstI of int
    | Var of string
    | Add of aexpr * aexpr
    | Mul of aexpr * aexpr
    | Sub of aexpr * aexpr
    
let e6 = Sub (Var "v", (Add(Var "w", Var "z")))
let e7 = Mul(CstI 2, Sub(Var "v", Add(Var "w", Var "z")))
let e8 = Add(Var "x", Add(Var "y", Add(Var "z", Var "v")))

    
let rec fmt a : string =
    match a with
    | CstI x -> $"{x}"
    | Var x -> $"{x}"
    | Add (x, y) -> $"({fmt x} + {fmt y})"
    | Mul (x, y) -> $"({fmt x} * {fmt y})"
    | Sub (x, y) -> $"({fmt x} - {fmt y})"

let rec simplify a : aexpr =
    match a with
    | Add(CstI 0, e) -> simplify e
    | Add(e, CstI 0) -> simplify e
    | Sub(e, CstI 0) -> simplify e
    | Mul(e, CstI 1) -> simplify e
    | Mul(CstI 1, e) -> simplify e
    | Mul(e, CstI 0) -> CstI 0
    | Mul(CstI 0, e) -> CstI 0
    | Sub(e1, e2) when e1 = e2 -> CstI 0
    
let rec differentiate a : aexpr =
    match a with
    | CstI x -> CstI 0
    | Var x -> CstI 1
    | Mul (CstI i, Var x) -> CstI i
    
    
    
