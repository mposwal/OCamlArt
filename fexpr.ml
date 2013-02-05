(*
 * fexpr.ml
 * Based on code by Chris Stone and Steve Freund
 *)

module Expr : EXPR = struct

  type expr = float * float -> float

  (* the mathematical constant pi *)
  let pi = 4. *. atan 1.

  (* Expression construction *)       
  (*return a fun that takes a tuple that returns x*)
  let buildX                           = function (x,y) -> x
  (*return a fun that takes a tuple and returns y*)
  let buildY                           = function (x,y) -> y
  (*return a fun that takes a tuple and returns sin of e applied to that tuple *)
  let buildSin e                       = function (x,y) -> sin(pi*. (e(x,y)) )
  (*return a fun that takes a tuple and returns cos of e applied to that tuple *)
  let buildCos e                       = function (x,y) -> cos(pi*. (e(x,y)) )
  (*ret a fun that takes two e's and avg's them *)
  let buildAvg((e1:expr), (e2:expr))   = function (e1,e2) -> (e1 +. e2)/. 2.0
  (*ret a fun that takes two e's and multiplies them *)
  let buildTimes((e1:expr), (e2:expr)) = function (e1,e2) -> e1 *. e2


  (* exprToString : expr -> string *)
  let exprToString _ = "unknown"
    
  (* eval : expr -> float*float -> float *)
  let eval e (x,y) = e((x,y)) (* apply e to x and y *)
	
  let sampleExpr =
        buildCos(buildSin(buildTimes(buildCos(buildAvg(buildCos(
	buildX),buildTimes(buildCos (buildCos (buildAvg
        (buildTimes (buildY,buildY),buildCos (buildX)))),
        buildCos (buildTimes (buildSin (buildCos
        (buildY)),buildAvg (buildSin (buildX), buildTimes
        (buildX,buildX))))))),buildY)))

 end
