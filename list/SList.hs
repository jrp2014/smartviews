{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module SList (List, nil, cons, foldE, app, ListView(..), view) where

data List e = Nil | Cons e (List e) | List e :++ List e
                 deriving Show

foldE n c Nil = n
foldE n c (Cons e es) = e `c` foldE n c es
foldE n c (es :++ es') = foldE (foldE n c es') c es

nil = Nil
cons = Cons

app :: List e -> List e -> List e
app = (:++)

-- Primitive recursive

primrec n c Nil = n
primrec n c (Cons e es) = c e (primrec n c es) es
primrec n c ((es :++ es') :++ es'') = primrec n c (es :++ (es' :++ es''))
primrec n c (es :++ es') = primrec (primrec n c es') c' es
                           where c' e x es = c e x (es :++ es')

-- ListView for a free monoid

data ListView e = VNil | VCons e (List e)
            deriving Show

-- view :: List e -> ListView e
-- view = primrec VNil (\e _ l -> VCons e l)

view :: List e -> ListView e
view Nil = VNil
view (Cons e es) = VCons e es
view ((es :++ es') :++ es'') = view (es :++ (es' :++ es''))
view ((view -> VNil) :++ es) = view es
view ((view -> VCons e es) :++ es') = VCons e (es :++ es')
