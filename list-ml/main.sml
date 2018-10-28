functor Benchmark(List : VLIST) =
struct
  local open List in

  fun fromList [] = empty
    | fromList (x :: xs) = cons (x, fromList xs)

  fun toList xs = case view xs
    of NilV => []
     | ConsV(x,xs) => x :: (toList xs)

  fun reverse xs = case view xs
    of NilV => empty
     | ConsV(x,xs) => append(reverse xs, cons(x, empty))

  fun fromNumber 0 = cons (0, empty)
    | fromNumber n = cons (n, fromNumber (n - 1))

  fun test1 xs = toList (reverse (reverse (fromList xs)))

  fun test2 n = toList (reverse (reverse (fromNumber n)))

  fun foldS n c xs = case view xs
    of NilV => n
     | ConsV(e,xs) => c (e, foldS n c xs)
  end

  fun add (x, y) = x + y

  fun test3S n = foldS 0 add (fromNumber n)

  fun test3E n = List.foldE 0 add (fromNumber n)
end;

structure BSList = Benchmark(SList);
structure BCat   = Benchmark(CList(CatenableList(RealTimeQueue)));

open BSList;

val _ = let
          val args = CommandLine.arguments()
          val SOME(n) = Int.fromString(List.nth (args, 0))
          val x = test2(n)
        in
          print ("Testing n = " ^ Int.toString(n) ^ "\n")
        end;
