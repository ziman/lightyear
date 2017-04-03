module TestLazy

import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Lightyear.Testing

%default partial
%access export

public export
data Ty = ATOM | EXPR

export
data SExpr : Ty -> Type where
  Atom : String -> SExpr ATOM
  AtomExpr : SExpr ATOM -> SExpr EXPR
  Expr : SExpr tyA -> SExpr tyB -> SExpr EXPR

Show (SExpr ty) where
  show (Atom a) = unwords ["Atom", a]
  show (Expr a b) = unwords ["Expr", show a, show b]

Show (SExpr ty) => Show (ty : Ty ** SExpr ty) where
  show (x ** pf) {ty = ATOM} = show pf
  show (x ** pf) {ty = EXPR} = show pf

atom : Parser $ SExpr ATOM
atom = do
  cs <- some $ satisfy isAlphaNum
  pure $ Atom (pack cs)
 <?> "Atom"

expr : Parser $ SExpr EXPR
expr = do
  a <- atom
  pure $ AtomExpr a
 <|>| do
   token "("
   exprA <- expr
   spaces
   exprB <- expr
   token ")"
   pure $ Expr exprA exprB
 <?> "EXPR"

tests : IO ()
tests = runTests
  [ parseTest "Atoms"          atom "Test"
  , parseTest "Numbers"        atom "1"
  , parseTest "Simple Expr 1"  expr "Test"
  , parseTest "Simple Expr 2"  expr "(id x)"
  , parseTest "Complex Expr 1" expr "(add (1 2))"
  , parseTest "Complex Expr 2" expr "(a (b (c (d (e (f (g (h (i (j (k (l (m (n (o (p (q (r (s (t (u (v (w (x (y (z NIL))))))))))))))))))))))))))"
  , parseTestNot "Complex Expr 3" expr """(a
  (b
    (c
      (d
        (e
          f
            (g
              (h
                (i
                  (j
                    (k
                      (l
                        (m
                          (n (o (p (q (r (s (t (u (v (w (x (y (z NIL))))))))))))))))))))))))))
"""
  , parseTestCmpNot "Complex Expr 4"
                    expr
                    """(a
  (b
    (c
      (d
        (e
          f
            (g
              (h
                (i
                  (j
                    (k
                      (l
                        (m
                          (n (o (p (q (r (s (t (u (v (w (x (y (z NIL))))))))))))))))))))))))))"""
                          """At 1:1:
	EXPR
At 1:1:
	Atom
At 1:1:
	a different token
At 2:3:
	EXPR
At 2:3:
	Atom
At 2:3:
	a different token
At 3:5:
	EXPR
At 3:5:
	Atom
At 3:5:
	a different token
At 4:7:
	EXPR
At 4:7:
	Atom
At 4:7:
	a different token
At 5:9:
	EXPR
At 5:9:
	Atom
At 5:9:
	a different token
At 6:12:
	token ")"
At 6:12:
	string ")"
At 6:12:
	character ')'
At 6:12:
	a different token"""
  ]
