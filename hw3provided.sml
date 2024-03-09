(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer


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

fun first_answer f = 
	fn x => case x of
			[] => raise NoAnswer
			| x::xs => case (f x) of 
						SOME x => x  
						| NONE => first_answer f xs 

fun all_answers f =
  fn list =>
     let fun all_so_far (acc, list') =
	   case list' of
	       [] => SOME []
	     | x::xs' => case (f x) of
			     NONE => NONE
			   | SOME x => all_so_far( [x] @ acc, xs')
     in
	 all_so_far([], list)
     end


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

fun count_wildcards p =
  g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
	g (fn () => 1) String.size p 

fun count_some_var (str, p) =
	g (fn () => 0) (fn x => if x = str then 1 else 0) p

(*0. Write a function check_pat that takes a pattern and returns true if and only if all the variables
appearing in the pattern are distinct from each other (i.e., use different strings). The constructor
names are not relevant. Hints: The sample solution uses two helper functions. The first takes a
pattern and returns a list of all the strings it uses for variables. Using foldl with a function that uses
@ is useful in one case. The second takes a list of strings and decides if it has repeats. List.exists may
be useful. Sample solution is 15*)

fun check_pat p =
	let fun helper1 p =
		case p of 
		Variable x => [x]
		| TupleP ps         => List.foldl (fn (ptn, i) => i @ helper1 ptn) [] ps
		| ConstructorP(_,p) => helper1 p 
		| _                 => []

		fun helper2 p =
		case p of 
		[] => false 
		| head::tail => List.exists (fn x=> head = x) tail 

	in not ((helper2 o helper1) p) 
	end 

fun match (va, ptn) =
  case (va, ptn) of
      (_, Wildcard) => SOME []
   |  (v, Variable str) => SOME [(str, v)]
   |  (Unit, UnitP) => SOME []
   | (Const v, ConstP i) =>  if v = i then SOME [] else NONE
   | (Tuple vs, TupleP ps) =>  if List.length vs = List.length ps
				then all_answers match (ListPair.zip(vs, ps))
				else NONE
   | (Constructor(s', v), ConstructorP(s'', p)) => if s' = s''
						 then match (v, p)
						 else NONE
   | (_ , _) => NONE


fun first_match va ptns = 
  SOME (first_answer (fn x => match(va, x)) ptns)
  handle NoAnswer => NONE