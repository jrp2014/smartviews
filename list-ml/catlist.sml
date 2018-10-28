signature CATENABLE_LIST =
sig
  type 'a Cat

  val empty   : 'a Cat
  val isEmpty : 'a Cat -> bool

  val cons    : 'a * 'a Cat -> 'a Cat
  val snoc    : 'a Cat * 'a -> 'a Cat
  val ++      : 'a Cat * 'a Cat -> 'a Cat

  val head    : 'a Cat -> 'a      (* raises Empty if list is empty *)
  val tail    : 'a Cat -> 'a Cat  (* raises Empty if list is empty *)
end

infixr ++

functor CatenableList (Q : QUEUE) : CATENABLE_LIST =
struct
  datatype 'a Cat = E | C of 'a * 'a Cat susp Q.Queue

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun link (C (x, q), s) = C (x, Q.snoc (q, s))
  fun linkAll q = let val $t = Q.head q
                      val q' = Q.tail q
                  in if Q.isEmpty q' then t else link (t, $(linkAll q')) end

  fun xs ++ E = xs
    | E ++ xs = xs
    | xs ++ ys = link (xs, $ys)
  fun cons (x, xs) = C (x, Q.empty) ++ xs
  fun snoc (xs, x) = xs ++ C (x, Q.empty)

  fun head E = raise Empty
    | head (C (x, _)) = x
  fun tail E = raise Empty
    | tail (C (x, q)) = if Q.isEmpty q then E else linkAll q
end
