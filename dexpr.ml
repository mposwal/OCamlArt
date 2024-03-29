(*
 * dexpr.ml
 * Based on code by Chris Stone and Stephen Freund
 *)

module Expr : EXPR = struct

  type expr = 
      VarX
    | VarY
    | Sine     of expr
    | Cosine   of expr
    | Average  of expr * expr
    | Times    of expr * expr

  (* the mathematical constant pi *)
  let pi = 4. *. atan 1.

  (* Expression creation *)
  let buildX = VarX
  let buildY = VarY
  let buildSin e = Sine e
  let buildCos e = Cosine e
  let buildAvg (e1,e2) = Average(e1,e2)
  let buildTimes (e1,e2) = Times(e1,e2)


  (* exprToString : expr -> string *)
  let rec exprToString e = match e with
	VarX -> "x"
      | VarY -> "y"
      | Sine e -> "sin(pi*"^exprToString(e)^")"
      | Cosine e -> "cos(pi*"^exprToString(e)^")"
      | Average(e1, e2) ->
	"("^exprToString(e1)^"+"^exprToString(e2)^")/2.0"
      | Times(e1, e2) ->
	exprToString(e1)^"*"^exprToString(e2)


  (* eval : expr -> float*float -> float
     Evaluate an expression for the given (x,y) coordinate.
  *)
  let rec eval e (x,y) =
      match e with 
        VarX -> x
        | VarY -> y
        | Sine e -> sin (pi *. (eval e (x,y)) )
        | Cosine e -> cos (pi *. (eval e (x,y)) )
        | Average(e1,e2) -> ((eval e1 (x,y)) +. (eval e2 (x,y)))/. 2.0
        | Times(e1,e2) -> ((eval e1 (x,y)) *. (eval e2 (x,y)))
        
  
 
  let sampleExpr =
    buildCos(buildSin(buildTimes(buildCos(buildAvg(buildCos(
      buildX),buildTimes(buildCos (buildCos (buildAvg
        (buildTimes (buildY,buildY),buildCos (buildX)))),
        buildCos (buildTimes (buildSin (buildCos
        (buildY)),buildAvg (buildSin (buildX), buildTimes
        (buildX,buildX))))))),buildY)));

end 
(*
let myexpr =Expr.buildTimes(Expr.buildAvg(Expr.buildCos(Expr.buildSin(Expr.buildX)), Expr.buildY), Expr.buildY) *)
  
