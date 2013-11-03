val test_only_capitals_1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test_only_capitals_2 = only_capitals ["A","bB","C"] = ["A","C"]

val test_only_capitals_3 = only_capitals ["a","b","c"] = []

val test_longest_string1_1 = longest_string1 ["A","bc","C"] = "bc"

val test_longest_string1_2 = longest_string1 ["ab","bc","Cb"] = "ab"

val test_longest_string1_3 = longest_string1 [] = ""

val test_longest_string2_1 = longest_string2 ["A","bc","C"] = "bc"

val test_longest_string2_2 = longest_string2 ["A","bc","Cb"] = "Cb"

val test_longest_string2_3 = longest_string2 [] = ""

val test_longest_string3 = longest_string3 ["A","bc","C"] = "bc"

val test_longest_string4 = longest_string4 ["A","B","C"] = "C"

val test_longest_capitalized = longest_capitalized ["A","bc","C"] = "A";

val test_rev_string = rev_string "abc" = "cba";

val test_first_answer = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test_all_answers = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test_count_wildcards_1 = count_wildcards Wildcard = 1

val test_count_wildcards_2 = count_wildcards (TupleP [Wildcard, Variable "A", UnitP, 
                                               TupleP [Wildcard, Wildcard], ConstructorP("A", Wildcard)]) = 4

val test_count_wild_and_variable_lengths_1 = count_wild_and_variable_lengths (Variable("a")) = 1

val test_count_wild_and_variable_lengths_2 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "A", 
                                               UnitP, TupleP [Wildcard, Variable "ABC"], 
                                               ConstructorP("AB", Wildcard)]) = 7

val test_count_some_var_1 = count_some_var ("x", Variable("x")) = 1;

val test_count_some_var_2 = count_some_var ("x", TupleP [Wildcard, Variable "x", UnitP, 
                                               TupleP [Variable "a", Variable "x"], 
                                               ConstructorP("x", Variable "x")]) = 3

val test_check_pat_1 = check_pat (Variable("x")) = true

val test_check_pat_2 = check_pat (TupleP [Wildcard, Variable "x", UnitP, 
                                               TupleP [Variable "a", Variable "x"], 
                                               ConstructorP("A", Variable "b"), Variable "c"]) = false

val test_check_pat_3 = check_pat (TupleP [Wildcard, Variable "a", UnitP, 
                                               TupleP [Variable "b", Variable "c"], 
                                               ConstructorP("a", Variable "d"), Variable "e"]) = true

val test_check_pat_4 = check_pat (TupleP[ConstP 4,Wildcard,Variable "ba",TupleP[Variable "ab"]]) = true

val test_match_1 = match (Const(1), UnitP) = NONE

val test_match_2 = match (Const 1, Wildcard) = SOME []

val test_match_3 = match (Const 1, Variable "A") = SOME [("A", Const 1)]

val test_match_4 = match (Const 1, ConstP 1) = SOME []

val test_match_5 = match (Tuple [Const 1, Const 1], TupleP [Variable "B", Variable "C"]) = 
                                                SOME [("C", Const 1),("B", Const 1)] = true

val test_match_6 = match (Constructor("A", Const 1), ConstructorP("A", Variable "C")) =  SOME [("C", Const 1)]


val test_first_match = first_match Unit [UnitP] = SOME []

val test_typecheck_patterns_1 = typecheck_patterns([], [TupleP [Variable "x", Variable "y"],
                                TupleP [Wildcard, Wildcard]])
                              = SOME(TupleT [Anything, Anything]) 

val test_typecheck_patterns_2 = typecheck_patterns([], []) = NONE

val test_typecheck_patterns_3 = typecheck_patterns([], [TupleP [Wildcard, TupleP [Wildcard, Wildcard]],
                                TupleP [Wildcard, Wildcard]])
  = SOME(TupleT [Anything, TupleT [Anything, Anything]]) 

val test_typecheck_patterns_4 = typecheck_patterns([], [ConstP 5, Wildcard, ConstP 3, Variable "x"])
  = SOME(IntT)

val test_typecheck_patterns_5 = typecheck_patterns([], [ConstP 5, UnitP]) = NONE 

val test_typecheck_patterns_6 = typecheck_patterns([("c", "t", IntT)], [ConstructorP("c", ConstP 5), ConstP 5])
  = NONE 

val test_typecheck_patterns_7 = typecheck_patterns([], [TupleP [Wildcard], TupleP [Wildcard, Wildcard]])
  = NONE 

val test_typecheck_patterns_8 = typecheck_patterns([], [TupleP [ConstP 3], TupleP [UnitP]])
  = NONE

val test_typecheck_patterns_9 = typecheck_patterns([], [TupleP [Wildcard, ConstP 1], TupleP [Wildcard, TupleP [Wildcard]]])
  = NONE 

val test_typecheck_patterns_10 = typecheck_patterns([], [ConstructorP("c", Variable "x")])
  = NONE

val test_typecheck_patterns_11 = typecheck_patterns([("c", "t", TupleT[IntT, Anything])],
                           [ConstructorP("c", TupleP [ConstP 4, Variable "x"])])
  = SOME(Datatype "t")

val test_typecheck_patterns_12 = typecheck_patterns([("c1", "t1", UnitT), ("c2", "t2", UnitT)],
                           [ConstructorP("c1", UnitP), ConstructorP("c2", UnitP)])
  = NONE

val test_typecheck_patterns_13 = typecheck_patterns([("c1", "t1", UnitT), ("c2", "t2", UnitT)],
                           [ConstructorP("c1", UnitP), ConstructorP("c1", UnitP)])
  = SOME(Datatype "t1") 

val test_typecheck_patterns_14 = typecheck_patterns([("c", "t", TupleT[Anything, Anything])],
                           [ConstructorP("c", TupleP [ConstP 4, Variable "x"])])
  = NONE

val test_typecheck_patterns_15 = typecheck_patterns([("c", "t", IntT)], [
                           TupleP [Wildcard, TupleP [ConstP 3, Wildcard]],
                           TupleP [Wildcard, TupleP [Variable "x", UnitP]],
                           TupleP [ConstructorP("c", ConstP 13), Wildcard]
                           ])
  = SOME(TupleT [Datatype "t", TupleT [IntT, UnitT]]) 

val test_typecheck_patterns_16 = typecheck_patterns([("c1", "t", TupleT[IntT, Datatype "t"]), ("c2", "t", UnitT)],
  [ConstructorP("c1", TupleP [ConstP 5, ConstructorP("c2", UnitP)]), ConstructorP("c2", UnitP)])
  = SOME(Datatype "t") 

val test_typecheck_patterns_17 = typecheck_patterns([("c", "t", TupleT[IntT, Datatype "t"]), ("c", "t", UnitT)],
  [ConstructorP("c", TupleP [ConstP 5, ConstructorP("c", UnitP)]), ConstructorP("c", UnitP)])
  = SOME(Datatype "t") 

val test_typecheck_patterns_18 = typecheck_patterns([("foo1","bar1",Datatype "bar2"),("foo2","bar2",UnitT)],
                                  [ConstructorP("foo1", Variable "x")]) = SOME (Datatype "bar1")
