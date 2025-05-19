import TDParse.Data.Ty
import TDParse.Data.Derivation

def ModeLabel.rp : ModeLabel -> Nat -> Std.Format
  | .FA  , _ => "|FA"
  | .BA  , _ => "|BA"
  | .PM  , _ => "|PM"
  | .MR m, n => go "MR" m n
  | .ML m, n => go "ML" m n
  | .AP m, n => go "AP" m n
  | .UR m, n => go "UR" m n
  | .UL m, n => go "UL" m n
  | .CU m, n => go "CU" m n
  | .JN m, n => go "JN" m n
  | .DN m, n => go "DN" m n
  where go label m n := (if n < 1 then "" else "∘") ++ label ++ rp m 1

instance : Repr ModeLabel where
  reprPrec := ModeLabel.rp

mutual
def FX.rp : FX -> Nat -> Std.Format
  | .query e, _ => "R^" ++ e.rp max_prec
  | .spawn  , _ => "S"
  | .store o, _ => "W^" ++ o.rp max_prec
  | .scope r, _ => "C^" ++ r.rp max_prec

def Ty.rp : Ty -> Nat -> Std.Format
  | .nat     , _ => "E"
  | .bool    , _ => "T"
  | .fn a b  , n => Repr.addAppParen (a.rp (max_prec-1) ++ " ~> " ++ b.rp 0) (n+1)
  | .comp f a, n => Repr.addAppParen (f.rp 0 ++ " " ++ (a.rp max_prec)) n
end

instance : Repr FX where
  reprPrec := FX.rp

instance : Repr Ty where
  reprPrec := Ty.rp

#eval (S ((S (E ~> T)) ~> (S E)) ~> (T))

open Std.Format
def Expr.rp : Expr t -> Std.Format
  | .lex s _       => s
  | .moc ⟨m,_⟩ l r =>
      repr m ++ nest 2 (align true ++ l.rp ++ align true ++ r.rp)

instance : Repr (Expr t) where
  reprPrec e _ := Expr.rp e

def TypedExpr := (t : Ty) × Expr t
def TypedExpr.rp : TypedExpr -> Std.Format
  | ⟨t,e⟩ => ":: " ++ repr t ++ line ++ "== " ++ nest 3 (repr e)

def Exprs := List ((t : Ty) × Expr t)
def Exprs.rp : List ((t : Ty) × Expr t) -> Std.Format
  | []        => nil
  | (e :: es) => TypedExpr.rp e ++ line ++ line ++ rp es

instance : Repr Exprs where
  reprPrec l _ := Exprs.rp l

def Interps (t : Ty) := List (Expr t × t.dom)
def Interps.rp {t :Ty} [Repr t.dom] : Interps t -> Std.Format
  | [] => nil
  | ((e,d) :: es) =>
    TypedExpr.rp ⟨t,e⟩ ++ line ++ "〚〛= " ++ repr d ++ line ++ line ++ rp es

instance (priority := high) instDens {t : Ty} [Repr t.dom] : Repr (Interps t) where
  reprPrec l _ := Interps.rp l
