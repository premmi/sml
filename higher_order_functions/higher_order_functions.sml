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

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(* The function only_capitals takes a string list and returns a string list that has only
   the strings in the argument that start with an uppercase letter. All strings are assumed to have at least 1
   character. *)
val only_capitals = List.filter (fn x => Char.isUpper(String.sub(x, 0))) 

(* The function longest_string1 takes a string list and returns the longest string in the
   list. If the list is empty, "" is returned. In the case of a tie, the string closest to the beginning of the
   list is returned. *)
val longest_string1 = List.foldl (fn(x, y) => if (String.size x > String.size y) then x else y) "" 

(* The function longest_string2 is exactly like longest_string1 except in the case of ties it returns the string 
   closest to the end of the list. *)
val longest_string2 = List.foldl (fn(x, y) => if (String.size x >= String.size y) then x else y) ""

(* The functions longest_string_helper, longest_string3, and longest_string4 are such that:
    . longest_string3 has the same behavior as longest_string1 and longest_string4 has the
      same behavior as longest_string2.
    . longest_string_helper has type (int * int -> bool) -> string list -> string
      (notice the currying). This function will look a lot like longest_string1 and longest_string2 
       but is more general because it takes a function as an argument.
    . If longest_string_helper is passed a function that behaves like > (so it returns true exactly
      when its first argument is stricly greater than its second), then the function returned has the same
      behavior as longest_string1.
    . longest_string3 and longest_string4 are defined with val-bindings and partial applications
      of longest_string_helper. *)

fun longest_string_helper f xs = List.foldl (fn(x, y) => if f(String.size x, String.size y) 
                                                         then x else y) "" xs

val longest_string3 = longest_string_helper (fn(x, y) => x > y)

val longest_string4 = longest_string_helper (fn(x, y) => x >= y)

(* The function longest_capitalized takes a string list and returns the longest string in the list that begins
   with an uppercase letter, or "" if there are no such strings. *)
val longest_capitalized = longest_string3  o only_capitals

(* The function rev_string takes a string and returns the string that is the same characters in
   reverse order. *)
val rev_string =  String.implode o List.rev  o String.explode

(* The function first_answer is of type ('a -> 'b option) -> 'a list -> 'b (notice the 2 argu-
   ments are curried). The first argument is applied to elements of the second argument in order
   until the first time it returns SOME v for some v and then v is the result of the call to first_answer.
   If the first argument returns NONE for all list elements, then first_answer raises the exception
   NoAnswer. *)
fun first_answer f xs = case xs of
			    [] => raise NoAnswer
			  | x::xs' => case f x of
					  SOME v => v
				        | NONE => first_answer f xs'

(* The function all_answers is of type ('a -> 'b list option) -> 'a list -> 'b list option
   (notice the 2 arguments are curried). The first argument is applied to elements of the second
   argument. If it returns NONE for any element, then the result for all_answers is NONE. Else the
   calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn and the result of
   all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together (order doesn't matter). *)
fun all_answers f xs = 
    let
	fun all_answers_acc (xs, acc) =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f x of
			      NONE => NONE
			    | SOME v => all_answers_acc(xs', v @ acc)
    in
	all_answers_acc(xs, [])
    end

(* g is used to define a function count_wildcards that takes a pattern and returns how many Wildcard
   patterns it contains. *)
val count_wildcards = g (fn _ => 1) (fn _ => 0) 

(* g is used to define a function count_wild_and_variable_lengths that takes a pattern and returns
   the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables
   in the variable patterns it contains. *)
val count_wild_and_variable_lengths = g (fn _ => 1) String.size 

(* g is used to define a function count_some_var that takes a string and a pattern (as a pair) and
   returns the number of times the string appears as a variable in the pattern. *)
fun count_some_var (s, p) = g (fn _ => 0) (fn x => if x = s then 1 else 0) p						       
(* The function check_pat takes a pattern and returns true if and only if all the variables
   appearing in the pattern are distinct from each other (i.e., use different strings). *)
fun check_pat p = 
    let
	fun extract_string_from_variables (p, acc) =
	    case p of
		Variable x => x::acc
	      | TupleP ps => List.foldl (fn (p, i) => extract_string_from_variables(p, i)) acc ps
	      | ConstructorP (_, p) => extract_string_from_variables(p, acc)
	      | _ => acc

       fun is_distinct xs = 
	   case xs of
	       [] => true
	     | x::xs' => (not (List.exists (fn y => y = x) xs'))
                         andalso is_distinct xs'
	     
    in
	 (is_distinct o extract_string_from_variables) (p, [])
    end

(* The function match takes a valu * pattern and returns a (string * valu) list option. It returns
   NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does.
   Note that if the value matches but the pattern has no patterns of the form Variable s, then the result
   is SOME []. *)
fun match (v, p) = 
    case (v, p) of
	(_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const x1, ConstP x2) => if x1 = x2 then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps 
                                 then all_answers match (ListPair.zip(vs, ps))
                                 else NONE                                
      | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2
                                                     then match(v, p)
                                                     else NONE
      |  _ => NONE
						      
(* The function first_match takes a value and a list of patterns and returns a
   (string * valu) list option, namely NONE if no pattern in the list matches or SOME lst where
   lst is the list of bindings for the first pattern in the list that matches. *)
fun first_match valu patlst = 
    SOME (first_answer (fn pat => match(valu, pat)) patlst)
    handle NoAnswer => NONE 

(* The function typecheck_patterns "type-checks" a pattern list. *)
fun typecheck_patterns (cs, ps) =
    let
	fun type_list (cs, ps, acc) = 
	    case (cs, ps) of
		(_, []) => if null acc
			   then [Datatype "none"]
			   else acc
	      | (_, p::ps') => let 
	                           fun pattern_to_type p =
				       case p of
					   Wildcard => Anything
					 | ConstP _ => IntT
					 | UnitP => UnitT
					 | TupleP ts => TupleT (List.foldl (fn (t, i) => i @ 
                                                                            [(pattern_to_type t)]) [] ts) 
					 | ConstructorP(s, pat) => let
				                                       fun const_to_datatype cs =
									   case cs of
									       [] => Datatype "none"
									     | (s1,s2,t)::cs' => if s1 = s 
                                                                                                 andalso 
                                                                                                (pattern_to_type(pat)= t orelse pattern_to_type(pat) = Anything)
                                                                                                 then Datatype s2
                                                                                                 else const_to_datatype cs'
													      
							          in
								      const_to_datatype cs
                                                                  end
					| Variable _ => Anything
                              in
				  type_list(cs, ps', acc @ [(pattern_to_type(p))])
	                      end

     fun  some_type ts = List.foldl (fn (t, i) => 
				       case (t, i) of
					   (t, Anything) => t
					 | (Anything, t) => t
					 | (TupleT t1, TupleT t2)=> let                                                                                                                    fun tup_helper (zs, acc) =
								            case zs of
										[] => TupleT acc
									      | (z1, z2)::zs' => tup_helper(zs', acc @ [some_type([z1, z2])])
                                                                    in
									tup_helper(ListPair.zipEq(t1, t2), [])
                                                                        handle UnequalLengths => TupleT [Datatype "none"]
								    end
					 | (t1, t2) => if t1 = t2
                                                       then t1
                                                       else Datatype "none") Anything ts

     fun option_type opt =
	 case opt of
	     Datatype "none" => NONE				    
	   | TupleT ts => let
	                      fun type_option_helper ts =
				  case ts of
				      [] => SOME opt
				    | t::ts' => if option_type(t) = NONE
                                                then NONE
                                                else type_option_helper(ts')
                          in
			      type_option_helper ts
                          end
			
	   | _ => SOME opt 	
  in
      (option_type  o some_type  o type_list) (cs, ps, [])
  end
    
			 
   
       
		
