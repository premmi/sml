(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test_all_except_option1 = all_except_option("string", ["string"]) = SOME []

val test_get_substitutions1_1 =  get_substitutions1([["foo"],["there"]], "foo") = []

val test_get_substitutions1_2 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],
                                                     ["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]

val test_get_substitutions2_1 =  get_substitutions1([["foo"],["there"]], "foo") = []

val test_get_substitutions2_2 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],
                                                     ["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test_similar_names = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	   [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
            {first="Freddie", last="Smith", middle="W"},{first="F", last="Smith", middle="W"}]

val test_card_color = card_color((Clubs, Num 2)) = Black

val test_card_value = card_value((Clubs, Num 2)) = 2

val test_remove_card_1 = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test_remove_card_2 = remove_card([(Hearts, Ace), (Clubs, Jack), (Spades, Num 5), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Clubs,Jack),(Spades,Num 5),(Hearts,Ace)]		          		     

val test_all_same_color_1 = all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true

val test_all_same_color_2 = all_same_color([(Hearts, Ace), (Spades, Ace)]) = false

val test_sum_cards = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4

val test_score_based_on_sum = score_based_on_sum(6,[(Hearts, Num 2),(Clubs, Num 4)],10) = 4 

val test_score = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test_officiate_1 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test_officiate_2 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42)
             = 3

val test_officiate_3 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)

val test_no_of_aces_1 = no_of_aces([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]) = 4

val test_no_of_aces_2 = no_of_aces([(Hearts, Ace),(Hearts, Num 2)]) = 1

val test_no_of_aces_3 = no_of_aces([(Hearts, Num 2),(Clubs, Num 4)]) = 0
             
             
val test_score_challenge_1 = score_challenge([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test_officiate_challenge_1 = officiate_challenge([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test_officiate_challenge_2 = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],

                       42) = 3

val test_careful_player_1 = careful_player([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], 22) = [Draw,Draw]

val test_careful_player_2 = careful_player([(Spades,Num 7),(Hearts,King),(Clubs,Ace),(Diamonds,Num 2)], 18) = [Draw,Draw,Discard (Hearts,King),Draw]
