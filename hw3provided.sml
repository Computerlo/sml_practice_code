(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(* you can put all your code here *)

fun only_capitals str_list =
	List.filter (fn x => Char.isUpper(String.sub(x, 0))) str_list

fun longest_string1 str_list = 
	List.foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) "" str_list

fun longest_string2 str_list = 
	List.foldl (fn (x, y) => if String.size(x) >= String.size(y) then x else y) "" str_list

fun longest_string_helper f =
  fn strings => List.foldl (fn (s1, s2) =>  if f (String.size(s1), String.size(s2))
					    then s1
					    else s2) "" strings

fun longest_string3 list_str = 
	longest_string_helper (fn (x,y) => x > y) list_str

fun longest_string4 strings =
  longest_string_helper (fn (x, y) => x >= y) strings 

fun longest_capitalized str_list = 
	(longest_string1 o only_capitals) str_list

fun rev_string strings =
  (String.implode o List.rev o String.explode) strings

(*7. Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b (notice the 2 arguments are curried). The first argument should be applied to elements of the second argument in order
until the first time it returns SOME v for some v and then v is the result of the call to first_answer.
If the first argument returns NONE for all list elements, then first_answer should raise the exception
NoAnswer. Hints: Sample solution is 5 lines and does nothing fancy *)

fun first_answer 