signature VLIST =
sig
  type 'a List

  val empty : 'a List
  val cons : 'a * 'a List -> 'a List
  val append : 'a List * 'a List -> 'a List

  datatype 'a ListView = NilV | ConsV of 'a * 'a List

  val view : 'a List -> 'a ListView

  val foldE : 'a -> ('b * 'a -> 'a) -> 'b List -> 'a
end

functor CList(Cat : CATENABLE_LIST) :> VLIST =
struct
  type 'a List = 'a Cat.Cat

  val empty = Cat.empty
  val cons = Cat.cons
  val append = Cat.++

  datatype 'a ListView = NilV | ConsV of 'a * 'a List

  fun view xs = if Cat.isEmpty xs then NilV else ConsV(Cat.head xs, Cat.tail xs)

  fun foldE n c xs = if Cat.isEmpty xs then n else c(Cat.head xs, foldE n c (Cat.tail xs))
end

structure SList :> VLIST =
struct
  datatype 'a List = SNil | SCons of 'a * 'a List | SApp of 'a List * 'a List

  val empty = SNil
  val cons = SCons
  val append = SApp

  datatype 'a ListView = NilV | ConsV of 'a * 'a List  

  fun view SNil = NilV
    | view (SCons(x, xs)) = ConsV(x, xs)
    | view (SApp(SApp(xs,ys), zs)) = view (SApp(xs, SApp(ys, zs)))
    | view (SApp(xs, ys)) = case view xs of
      NilV => view ys
      | (ConsV(x,xs)) => ConsV(x, append(xs, ys))

  fun foldE n c SNil = n
    | foldE n c (SCons(e, es)) = c(e, foldE n c es)
    | foldE n c (SApp(es, fs)) = foldE (foldE n c fs) c es
end
