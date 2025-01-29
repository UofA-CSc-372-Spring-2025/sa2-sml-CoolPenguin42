(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Trevor Nemetz *)
(* Time spent on HW6:
*)

(* Collaborators and references:
*)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";


(*************************************************************************************************)
(**** Problem A ****)
fun mynull ([] : 'a list) : bool = true
  | mynull _  = false

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true

val () =
    Unit.checkExpectWith Bool.toString "mynull [1] should be false"
    (fn () => mynull [1])
    false
(*************************************************************************************************)





(*************************************************************************************************)
(**** Problem B ****)
fun firstVowel ([] : char list) : bool = false
  | firstVowel (c :: _) =
      c = #"a" orelse c = #"e" orelse c = #"i" orelse c = #"o" orelse c = #"u";

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel [] should be false"
    (fn () => firstVowel [])
    false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'b' should be false"
    (fn () => firstVowel [#"b"])
    false
(*************************************************************************************************)





(*************************************************************************************************)
(**** Problem C ****)
fun reverse (xs : 'a list) : 'a list =
  List.foldl (fn (x, acc) => x :: acc) [] xs;

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2,3] should be [3,2,1]"
  (fn () => reverse [1,2,3])
  [3,2,1]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [] should be []"
  (fn () => reverse [])
  []
(*************************************************************************************************)





(*************************************************************************************************)
(**** Problem D ****)
exception EmptyList

fun minlist (n : int list) : int = 
  case n of
    [] => raise EmptyList
  | x::xs => List.foldl Int.min x xs;

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4] should be 1"
  (fn () => minlist [1,2,3,4])
  1
(*************************************************************************************************)





(*************************************************************************************************)
(**** Problem E ****)
exception Mismatch

fun zip (xs : 'a list, ys : 'b list) : ('a * 'b) list =
  case (xs, ys) of 
    ([], []) => []
  | (x::xsprime, y::ysprime) => (x, y) :: zip (xsprime, ysprime)
  | _ => raise Mismatch;

val () =
    Unit.checkExpectWith (Unit.listString (fn (x: int, y: int) => "(" ^ Int.toString x ^ ", " ^ Int.toString y ^ ")"))
    "zip ([1,2,3], [7,8,9]) should be [(1,7), (2,8), (3,9)]"
    (fn () => zip ([1, 2, 3], [7, 8, 9]))
    [(1,7), (2,8), (3,9)]

val () =
    Unit.checkExpectWith (Unit.listString (fn (x: int, y: int) => "(" ^ Int.toString x ^ ", " ^ Int.toString y ^ ")"))
    "zip ([], []) should be []"
    (fn () => zip ([], []))
    []

val () =
    Unit.checkExnWith (Unit.listString (fn (x: int, y: int) => "(" ^ Int.toString x ^ ", " ^ Int.toString y ^ ")"))
    "zip ([1], []) should raise Mismatch"
    (fn () => zip ([1], []))
(*************************************************************************************************)





(*************************************************************************************************)
(**** Problem F ****)
fun concat ([] : 'a list list) : 'a list = [] 
  | concat ((x::xs)::xss) = x :: concat (xs :: xss)
  | concat ([] :: xss) = concat xss;

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "concat [] should be []"
  (fn () => concat [])
  []

(* val () =
  Unit.checkExpectWith (Unit.listString Unit.listString) 
  "concat [[1], [2, 3, 4], [], [5, 6]] should be [1, 2, 3, 4, 5, 6]"
  (fn () => [[1], [2, 3, 4], [], [5, 6]])
  [1, 2, 3, 4, 5, 6] *)
(*************************************************************************************************)





(*************************************************************************************************)
(**** Problem G ****)
fun isDigit (c : char) : bool = 
  case c of
    #"0" => true
  | #"1" => true
  | #"2" => true
  | #"3" => true
  | #"4" => true
  | #"5" => true
  | #"6" => true
  | #"7" => true
  | #"8" => true
  | #"9" => true
  | _    => false;

val () =
    Unit.checkExpectWith Bool.toString "isDigit 'a' should be false"
    (fn () => isDigit #"a")
    false

val () =
    Unit.checkExpectWith Bool.toString "isDigit '0' should be true"
    (fn () => isDigit #"0")
    true
(*************************************************************************************************)





(*************************************************************************************************)
(**** Problem H ****)
fun isAlpha (c : char) : bool =
  let
    val ordC = Char.ord c
  in
    (ordC >= Char.ord #"A" andalso ordC <= Char.ord #"Z") orelse
    (ordC >= Char.ord #"a" andalso ordC <= Char.ord #"z")
  end;
(*************************************************************************************************)





(*************************************************************************************************)
(**** Problem I ****)
fun svgCircle (cx : int, cy : int, r : int, fill : string) : string = 
  "<circle cx=\"" ^ Int.toString cx ^ "\" cy=\"" ^ Int.toString cy ^ "\" r=\"" ^ Int.toString r ^ "\" fill=\"" ^ fill ^ "\" />";

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";
(*************************************************************************************************)





(*************************************************************************************************)
(**** Problem J ****)
fun partition (pred : 'a -> bool) (xs : 'a list) : 'a list * 'a list =
  case xs of
    []       => ([],[])
  | (x::xs') => 
    let
      val (trueList, falseList) = partition pred xs'
    in
      if pred x then (x::trueList, falseList)
      else (trueList, x::falseList)
    end;


val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);
(*************************************************************************************************)


(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)