import TDParse.Combine

def synsem : Tree c ((u : Ty) × Expr u) -> List ((t : Ty) × Expr t)
  | .leaf _ ⟨u,e⟩ => [⟨u, e⟩]
  | .node _ l r => do
      let ⟨lt,le⟩ <- synsem l
      let ⟨rt,re⟩ <- synsem r
      let ⟨wt,md⟩ <- combine lt rt
      pure ⟨wt, .moc md le re⟩

def runAs : (s : Ty) -> ((t : Ty) × Expr t) -> Option s.dom
  | s, ⟨t,e⟩ => if h : t = s then by subst h; exact some e.den else none
