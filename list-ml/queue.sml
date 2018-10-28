signature QUEUE =
sig
  type 'a Queue

  val empty   : 'a Queue
  val isEmpty : 'a Queue -> bool

  val snoc    : 'a Queue * 'a -> 'a Queue
  val head    : 'a Queue -> 'a         (* raises Empty if queue is empty *)
  val tail    : 'a Queue -> 'a Queue   (* raises Empty if queue is empty *)
end

open Stream

structure RealTimeQueue : QUEUE =
struct
  type 'a Queue = 'a Stream * 'a list * 'a Stream

  val empty = ($Nil, [], $Nil)
  fun isEmpty ($Nil, _, _) = true
    | isEmpty _ = false

  fun rotate ($Nil, y :: _, a) = $(Cons (y, a))
    | rotate ($(Cons (x, xs)), y :: ys, a) =
        $(Cons (x, rotate (xs, ys, $(Cons (y, a)))))

  fun exec (f, r, $(Cons (x, s))) = (f, r, s)
    | exec (f, r, $Nil) = let val f' = rotate (f, r, $Nil) in (f', [], f') end

  fun snoc ((f, r, s), x) = exec (f, x :: r, s)

  fun head ($Nil, r, s) = raise Empty
    | head ($(Cons (x, f)), r, s) = x
  fun tail ($Nil, r, s) = raise Empty
    | tail ($(Cons (x, f)), r, s) = exec (f, r, s)
end

