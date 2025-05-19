import TDParse.Combine

def synsem : Tree c ((u : Ty) × Expr u) -> List ((t : Ty) × Expr t)
  | .leaf _ ⟨u,e⟩ => [⟨u, e⟩]
  | .node _ l r => do
      let ⟨lt,le⟩ <- synsem l
      let ⟨rt,re⟩ <- synsem r
      let ⟨wt,md⟩ <- combine lt rt
      pure ⟨wt, .moc md le re⟩

def run : ((t : Ty) × Expr t) -> ((t : Ty) × Expr t × t.dom)
  | ⟨t,e⟩ => ⟨t, e, e.den⟩

def runAs : (s : Ty) -> ((t : Ty) × Expr t) -> Option (Expr s × s.dom)
  | s, ⟨t,e⟩ => if h : t = s then by subst h; exact some (e, e.den) else none
