(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (s, xs) = 
    case xs of
	[] => NONE
      | x::xs' => if same_string(x, s)
		  then SOME xs'
		  else case all_except_option(s, xs') of
			   NONE => NONE 
			 | SOME tail => SOME (x::tail)

fun get_substitutions1 (list, s) =
  case list of
      [] => []
    | l::list' => case all_except_option(s, l) of
		    NONE => [] 
		   | SOME sublist => sublist @ get_substitutions1(list', s)

fun get_substitutions2(list, s)=
    let fun helper (list, s , acc)=
    case list of 
    []=> acc
    | l::list' => case all_except_option(s,l) of 
                    NONE => []
                    | SOME sublist => helper(list', s, sublist @ acc)
                    in helper(list, s , [])
                    end

fun similar_names(list, rcds)=
    let fun helper(list, rcds)=
    case list of 
    []=>[]
    |x::xs=> case rcds of 
            {first="f", last="l", middle="m"}=>
            [{first="x", last="l", middle="m"}]@ helper(xs, rcds)
    |_=>[]
    in
    case rcds of 
    {first="f",last="l", middle="m"}=> let val x = get_substitutions1(list, "f")
                                           in helper("f"::x, rcds)
                                           end
    |_=>[]
    end 

(* YOU may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color (card) =
  case card of
      {1=Clubs,  2=_} => Black    
    | {1=Spades, 2=_} => Black
    | _ => Red

fun card_value (card) =
  case card of
      {1=_ , 2=Num(int)} => int
    | {1=_ , 2=Ace} => 11
    | _ => 10

fun remove_card (cards, c, e) =
  let fun remove_helper(cards, c, e, removed) =
	case cards of
	    [] => if removed
		  then []
		  else raise e
	  | card::cards' => if card = c andalso removed = false
			    then remove_helper(cards', c, e, true)
			    else card::remove_helper(cards', c, e, removed)
  in
      remove_helper(cards, c, e, false)
  end

fun all_same_color (list_cards) = 
    case list_cards of
    [] => true
    | card2 :: [] => true
    | card1::card2::card' => (card_color(card1) = card_color(card2)) andalso all_same_color(card2 :: card')

fun sum_cards (list_cards)=
    let fun helper (list_cards, total) = 
        case list_cards of 
        [] => total 
        | x::xs => helper(xs, total + card_value(x))
        in 
        helper (list_cards, 0)
        end 
fun score (card_list, goal) =
  let
      val sum = sum_cards(card_list)
      val prelim_score = if sum > goal
			 then 3 * (sum - goal)
			 else (goal - sum)
				  
  in
      if all_same_color(card_list)
      then prelim_score div 2
      else prelim_score	       
  end