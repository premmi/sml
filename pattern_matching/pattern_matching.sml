(* The function compares two strings (returns true if same string) and avoids subsequent functions
   that compare strings have polymorphic types. *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* The function all_except_option, takes a string and a string list and retursn NONE if the
   string is not in the list, else returns SOME lst where lst is identical to the argument list 
   except the string is not in it. *)
fun all_except_option (s, xs) =
    let 
	fun all_except_option_acc (xs, acc) =
	    case xs of
		[] => NONE
	      | x::xs' => if (same_string(s, x))
			  then SOME(acc @ xs')
                          else all_except_option_acc(xs', x::acc)
    in
    	all_except_option_acc(xs, [])
    end
	
(* The function get_substitutions1, takes a string list list and a string s and returns a string list. 
   The result has all the strings that are in some list in substitutions that also has s, but s itself should not 
   be in the result. *)
fun get_substitutions1 (xs, s) =
    case xs of
	[] => []
      | x::xs' => case all_except_option(s, x) of
                      NONE => get_substitutions1(xs', s)
		    | SOME y => y @ get_substitutions1(xs', s)

(* The function get_substitutions2, is like get_substitutions1 except it uses a tail-recursive
   local helper function. *)
fun get_substitutions2 (xs, s) = 
    let
	fun get_substitutions_acc (xs, acc) =
	    case xs of
		[] => acc
		   | x::xs' => case all_except_option(s, x) of
				   NONE => get_substitutions_acc(xs', acc)
					| SOME y => get_substitutions_acc(xs', y @ acc)
    in
	get_substitutions_acc(xs, [])
    end

(* The function similar_names, takes a string list list of substitutions and a full name of type 
   {first:string,middle:string,last:string} and returns a list of fullnames 
   (type {first:string,middle:string,last:string} list). The result is all the full names that can be produced by 
   substituting for the first name (and only the first name) using substitutions. *)
fun similar_names (xs, {first = x, middle = y, last = z}) =    
    let        
	fun similar_names_list (substitutions, acc) =
	    case substitutions of
		[] => acc
	      | x::xs' => similar_names_list(xs', acc @ [{ first = x, middle = y, last = z }])
    in
	similar_names_list( get_substitutions1(xs, x), { first = x, middle = y, last = z }::[])
    end

(*  Num is always used with values 2, 3, ..., 10 *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove  

(* The function card_color, takes a card and returns its color (spades and clubs are black, diamonds and hearts are red). *)
fun card_color card =
    case card of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | (_, _) => Red

(* The function card_value, takes a card and returns its value (numbered cards have their
   number as the value, aces are 11, everything else is 10). *)
fun card_value card =
    case card of
	(_, Num i) => i
      | (_, Ace) => 11
      | (_, _) => 10

(* The function compares two cards (returns true if same card) and avoids subsequent functions
   that compare cards have polymorphic types. *)
fun same_card (c1 : card, c2 : card) =
    c1 = c2

(* The function remove_card, takes a list of cards cs, a card c, and an exception e. It returns a
   list that has all the elements of cs except c. If c is in the list more than once, only the first one is removed.
   If c is not in the list, an exception e is raised. *)
fun remove_card (cs, c, e) =
    let
	fun remove_card_acc (cs, acc) =
	    case cs of
		[] => raise e
	      | x::xs' => if (same_card(c, x))
			 then acc @ xs'
                         else remove_card_acc(xs', x::acc)
    in
	remove_card_acc(cs, [])
    end

(* The function all_same_color, takes a list of cards and returns true if all the cards in the
   list are the same color. *)
fun all_same_color cs =
    case cs of
      c::d::cs'  => if (card_color(c) = card_color(d))
	            then all_same_color(d::cs')
                    else false	
     | _ => true 
	   
(* The function sum_cards, takes a list of cards and returns the sum of their values. *)
fun sum_cards cs =
    let
	fun sum_cards_acc (cs, acc) =
	    case cs of
		[] => acc
	     | c::cs' => sum_cards_acc(cs', card_value(c) + acc)
    in
 	sum_cards_acc(cs, 0)
    end
		     
(* The function score_based_on_sum, takes sum of card values, a card list (the held-cards) and an int (the goal) and computes
   the score *)
fun score_based_on_sum (sum, cs, goal) =
    let
	val prelim_score = if (sum > goal) 
                           then 3 * (sum - goal)
                           else goal - sum		   
    in
	if (all_same_color(cs))
        then prelim_score div 2
        else prelim_score
    end 

(* The function score, takes a card list (the held-cards) and an int (the goal) and computes
   the score *)
fun score (cs, goal) =
    score_based_on_sum(sum_cards(cs), cs, goal)   

(* The function officiate, "runs a game." It takes a card list (the card-list) a move list
   (what the player "does" at each point), and an int (the goal) and returns the score at the end of the
   game after processing (some or all of) the moves in the move list in order.
  
   The game starts with the held-cards being the empty list.
   The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
   If the player discards some card c, play continues with the held-cards not having c and the card-list unchanged.   If c is not in the held-cards, the IllegalMove exception is raised.
   If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes the sum of 
   the held-cards to exceed the goal, the game is over (after drawing). Else play continues with a larger 
   held-cards and a smaller card-list.*)
fun officiate (cs, ms, goal) =
    let
	fun score_acc (cs, ms, hs) =
	    case(cs, ms) of
		(_, []) => score(hs, goal)
	      | ([], Draw::ms') => score(hs, goal)
	      | (c::cs', Draw::ms') => if (sum_cards(c::hs) > goal)
                                       then score(c::hs, goal)
				       else score_acc(cs', ms', c::hs)
	      | (cs, (Discard c)::ms') => score_acc(cs, ms', remove_card(hs, c, IllegalMove)) 		 
	    
    in
	score_acc(cs, ms, [])
    end	

(* The function no_of_aces, takes a card list and returns the number of aces in the list. *)
fun no_of_aces cs =
    let
	fun no_of_aces_acc (cs, acc) =
	    case cs of
		[] => acc
	      | (_, Ace)::cs' => no_of_aces_acc(cs', acc + 1)
              | c::cs' => no_of_aces_acc(cs', acc)
    in
	no_of_aces_acc(cs, 0)
    end

(* The function score_challenge and officiate_challenge are like their non-challenge counterparts except each ace can 
   have a value of 1 or 11 and score_challenge should always return the least (i.e., best) possible score. *)
fun score_challenge (cs, goal) =
    let
        val sum = sum_cards(cs)       
        fun best_score (cs, aces, min_score) =
	    case aces of
	        0 => min_score
	      | aces => let
		            val current_score = score_based_on_sum(sum - aces * 10, cs, goal)
                        in
			    if (current_score < min_score)
			    then best_score(cs, aces - 1, current_score)
                            else best_score(cs, aces - 1, min_score)
                        end
    in
	best_score(cs, no_of_aces(cs), score(cs, goal))
    end
		
	
fun officiate_challenge (cs, ms, goal) =
    let
	fun score_acc (cs, ms, hs) =
	    case(cs, ms) of
		(_, []) => score_challenge(hs, goal)
	      | ([], Draw::ms') => score_challenge(hs, goal)
	      | (c::cs', Draw::ms') => if ((sum_cards((c::hs)) - no_of_aces(c::hs) * 10) > goal)                                                          then score_challenge(c::hs, goal)
				       else score_acc(cs', ms', c::hs)
	      | (cs, (Discard c)::ms') => score_acc(cs, ms', remove_card(hs, c, IllegalMove)) 		 
	    
    in
	score_acc(cs, ms, [])
    end	

(* The function careful_player, takes a card-list and a goal and returns a move-list such that calling officiate 
   with the card-list, the goal, and the move-list has this behavior:
   The value of the held cards never exceeds the goal.
   A card is drawn whenever the goal is more than 10 greater than the value of the held cards.
   If a score of 0 is reached, there must be no more moves.
   If it is possible to discard one card, then draw one card to produce a score of 0, then this must be done. (Note   careful_player will have to look ahead to the next card, which in many card games is considered "cheating.") *)
fun careful_player (cs, goal) =
    let
	fun careful_player_acc (cs, ms, hs) =
	    case cs of
		[] => ms
	     | c::cs' => if (goal > 10 + sum_cards(hs))
			 then careful_player_acc(cs', ms @ [Draw], c::hs)
			 else
			     if (goal = sum_cards(hs))
			     then ms
			     else 
				 let
				     fun should_discard (hs, value) =
					 case hs of
					     [] => ms
					  | h::hs' => if (card_value(h) = value)
						      then ms @ [Discard h, Draw]
						      else should_discard(hs', value)
				in
				    should_discard(hs, card_value(c) + sum_cards(hs) - goal)
                                end
    in
	careful_player_acc(cs, [], [])
    end	
