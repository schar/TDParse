import TDParse.Data.Ty

/-!
Printable semantic expressions.

`PExpr` is not an interpreter in its own right; it is the syntax produced by
reifying normalized semantic values.  `PBuild` threads a fresh-name counter
through that reification.
-/

namespace TDParse

inductive PExpr where
  | atom : String -> PExpr
  | var : String -> PExpr
  | app : PExpr -> PExpr -> PExpr
  | lam : String -> PExpr -> PExpr
  | bool : Bool -> PExpr
  | nat : Nat -> PExpr
  | listNat : List Nat -> PExpr
  | list : List PExpr -> PExpr
  | pair : PExpr -> PExpr -> PExpr
  | fst : PExpr -> PExpr
  | snd : PExpr -> PExpr
  | conj : PExpr -> PExpr -> PExpr
  | imp : PExpr -> PExpr -> PExpr
  | neg : PExpr -> PExpr
  | eqNat : PExpr -> PExpr -> PExpr
  | quant : String -> List Nat -> String -> PExpr -> PExpr
  | quantIn : String -> String -> PExpr -> PExpr -> PExpr
  | selectWhere : String -> PExpr -> PExpr
  | comp : List (String × PExpr) -> PExpr -> PExpr
  | op : String -> List PExpr -> PExpr

namespace PExpr

def paren (b : Bool) (s : String) : String :=
  if b then "(" ++ s ++ ")" else s

mutual
def renderNatList : List Nat -> String
  | [] => ""
  | [x] => toString x
  | x :: xs => toString x ++ "," ++ renderNatList xs

def renderList : List PExpr -> String
  | [] => ""
  | [x] => x.render 0
  | x :: xs => x.render 0 ++ ", " ++ renderList xs

def renderArgs : List PExpr -> String
  | [] => ""
  | e :: es => " " ++ e.render 11 ++ renderArgs es

def renderCompBinds : List (String × PExpr) -> String
  | [] => ""
  | [(_, restrict)] => restrict.render 0
  | (_, restrict) :: binds => restrict.render 0 ++ ", " ++ renderCompBinds binds

def render : PExpr -> Nat -> String
  | .atom s, _ => s
  | .var s, _ => s
  | .bool b, _ => if b then "true" else "false"
  | .nat n, _ => toString n
  | .listNat xs, _ => "[" ++ renderNatList xs ++ "]"
  | .list xs, _ => "[" ++ renderList xs ++ "]"
  | .pair a b, _ => "⟨" ++ a.render 0 ++ ", " ++ b.render 0 ++ "⟩"
  | .fst p, prec => paren (prec > 10) ("fst " ++ p.render 11)
  | .snd p, prec => paren (prec > 10) ("snd " ++ p.render 11)
  | .app f x, p => paren (p > 10) (f.render 10 ++ " " ++ x.render 11)
  | .lam x body, p => paren (p > 0) ("λ" ++ x ++ ". " ++ body.render 0)
  | .conj p q, prec => paren (prec > 3) (p.render 4 ++ " ∧ " ++ q.render 3)
  | .imp p q, prec => paren (prec > 2) (p.render 3 ++ " → " ++ q.render 2)
  | .neg p, prec => paren (prec > 10) ("¬" ++ p.render 11)
  | .eqNat x y, prec => paren (prec > 4) (x.render 5 ++ " = " ++ y.render 5)
  | .quant q xs x body, prec =>
      paren (prec > 0) (q ++ x ++ "∈{" ++ renderNatList xs ++ "}. " ++ body.render 0)
  | .quantIn q x restrict body, prec =>
      paren (prec > 0) (q ++ x ++ "[" ++ restrict.render 0 ++ "]. " ++ body.render 0)
  | .selectWhere x body, prec =>
      paren (prec > 0) ("ι" ++ x ++ "[" ++ body.render 0 ++ "]")
  | .comp [] body, _ => body.render 0
  | .comp binds body, _ =>
      "[" ++ body.render 0 ++ " | " ++ renderCompBinds binds ++ "]"
  | .op name args, p => paren (p > 10) (name ++ renderArgs args)
end

end PExpr

abbrev PBuild := Nat -> PExpr × Nat

namespace PBuild

def ofExpr (e : PExpr) : PBuild :=
  fun n => (e, n)

def atom (s : String) : PBuild :=
  ofExpr (PExpr.atom s)

def var (s : String) : PBuild :=
  ofExpr (PExpr.var s)

def runList (xs : List PBuild) : Nat -> List PExpr × Nat :=
  fun n =>
    let (xs, n) := xs.foldl
      (fun (acc, n) x =>
        let (e, n) := x n
        (e :: acc, n))
      ([], n)
    (xs.reverse, n)

def op (name : String) (args : List PBuild) : PBuild :=
  fun n =>
    let (args, n) := runList args n
    (PExpr.op name args, n)

def unary (mk : PExpr -> PExpr) (x : PBuild) : PBuild :=
  fun n =>
    let (xe, n) := x n
    (mk xe, n)

def binary (mk : PExpr -> PExpr -> PExpr) (x y : PBuild) : PBuild :=
  fun n =>
    let (xe, n) := x n
    let (ye, n) := y n
    (mk xe ye, n)

def app (f x : PBuild) : PBuild :=
  binary PExpr.app f x

def lam (body : String -> PBuild) : PBuild :=
  fun n =>
    let v := "x" ++ toString n
    let (be, n) := body v (n + 1)
    (PExpr.lam v be, n)

def pair (x y : PBuild) : PBuild :=
  binary PExpr.pair x y

def fst (p : PBuild) : PBuild :=
  unary PExpr.fst p

def snd (p : PBuild) : PBuild :=
  unary PExpr.snd p

def list (xs : List PBuild) : PBuild :=
  fun n =>
    let (xs, n) := runList xs n
    (PExpr.list xs, n)

def bool (b : Bool) : PBuild :=
  ofExpr (PExpr.bool b)

def nat (n : Nat) : PBuild :=
  ofExpr (PExpr.nat n)

def conj (p q : PBuild) : PBuild :=
  binary PExpr.conj p q

def imp (p q : PBuild) : PBuild :=
  binary PExpr.imp p q

def neg (p : PBuild) : PBuild :=
  unary PExpr.neg p

def eqNat (p q : PBuild) : PBuild :=
  binary PExpr.eqNat p q

end PBuild

end TDParse
