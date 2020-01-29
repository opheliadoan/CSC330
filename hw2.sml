(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for Part 1 here *)
fun all_except_option(str, lst) =
    case lst of
        [] => NONE
        | x::xs =>
            if same_string(x, str)
                then SOME xs
            else case all_except_option(str, xs) of
                NONE => NONE
                | SOME ys => SOME(x::ys)

fun get_substitutions1 (xs, s) =
    case xs of
        [] => []
        | (x::xs') =>
            case all_except_option(s,x) of
                NONE => get_substitutions1(xs',s)
                | SOME ys => ys @ get_substitutions1(xs',s)

fun get_substitutions2(xs, s) =
    let
        fun sub(xs, acc) =
            case xs of
                [] => acc
                | x::xs' =>
                    case all_except_option(s, x) of
                        NONE => sub(xs', acc)
                        | SOME ys => sub(xs', acc@ys)
    in sub(xs,[])
    end

fun similar_names(lst, name: {first:string, middle:string, last:string}) =
    let
        fun helper (n, xs) =
            case (n,xs) of
                (_, []) => []
                | ({first=f,middle=m,last=l}, x::xs') => {first=x,middle=m,last=l} :: helper(n,xs')
    in
        case name of
            {first=f,middle=m,last=l} => name::helper(name, get_substitutions2(lst,f))
    end

(************************************************************************)
(* Game  *)

(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw


exception IllegalMove

(* put your solutions for Part 2 here *)
fun card_color (c:card): color =
    case c of
        (Clubs,_) => Black
        |(Diamonds,_) => Red
        |(Hearts,_) => Red
        |(Spades,_) => Black

fun card_value (c:card): int =
    case c of
        (_,Jack) => 10
        |(_,Queen) => 10
        |(_,King) => 10
        |(_,Ace) => 11
        |(_,Num n) => n

fun remove_card(cs: card list, c: card, e) =
    case cs of
        [] => raise e
        | (c'::cs') => if c' = c then cs' else c'::remove_card(cs',c,e)

fun all_same_color(cs: card list) =
    case cs of
        [] => true
        |(c::[]) => true
        |(c1::c2::cs') =>
            if(card_color(c1)=card_color(c2))
                then all_same_color(c2::cs')
            else false

fun sum_cards(cs: card list) =
    let
        fun sum_helper(cs, acc) =
            case cs of
                [] => acc
                |c::cs' => sum_helper(cs', acc+card_value(c))
    in
        sum_helper(cs, 0)
    end

fun score(held_cards: card list, goal: int) =
    let
        val sum = sum_cards(held_cards)
        val same = (if all_same_color(held_cards) then 2 else 1)
    in
        if sum > goal
            then ((2*(sum-goal)) div same)
        else ((goal-sum) div same)

    end

fun officiate(cs: card list, moves: move list, goal: int) =
    let
        fun officiate_helper(cs, moves, held_cards) =
            if sum_cards(held_cards) > goal
                then score(held_cards, goal)
            else
                case moves of
                    [] => score(held_cards, goal)
                    | (Discard c::moves') => officiate_helper(cs, moves', remove_card(held_cards,c,IllegalMove))
                    | (Draw::moves') => (case cs of
                        [] => score(held_cards, goal)
                        |draw_card::cs' => officiate_helper(cs', moves', draw_card::held_cards))
    in
        officiate_helper(cs, moves, [])
    end

fun toInt (b: bool) =
    if b then 1 else 0;

fun sum(a:int, b:int) =
    a + b;

fun tests_passed(t: string, tests: bool list) =
    let
        val len = length tests
        val count = foldl sum 0 (map toInt tests)
    in
        print ("\n**Test " ^ t ^ " "^ (if count = len then "passed" else "failed")
            ^ "\n  "^ Int.toString(count) ^ " out of " ^ Int.toString(len) ^ "\n\n");
        count = len
    end;

val test1_0=all_except_option("50",["6","1","17","49","3"]) = NONE;
val test1_1=all_except_option("3",["4","9","10"]) = NONE;
val test1_2=all_except_option("3",["4","9","3","10"]) = SOME ["4","9","10"];
val test1_3=all_except_option("3",[]) = NONE;
val test1_4=all_except_option("3",["3","4","9","10"])  = SOME ["4","9","10"];
val test1_5=all_except_option("3",["4","9","10","3"]) = SOME ["4","9","10"];
val test1_6=all_except_option("3",["3"]) = SOME [];
val tests1 = [test1_0, test1_1, test1_2, test1_3, test1_4, test1_5, test1_6];

val t1 = tests_passed("1", tests1);

val test2_0=get_substitutions1([["William","Will","W"],["Thomas","Ophelia"],["Tom"]], "Will") = ["William","W"];
val test2_1=get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred")
= ["Fredrick","Freddie","F"];
val test2_2=get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff")
= ["Jeffrey","Geoff","Jeffrey"];
val test2_3=get_substitutions1([["Neo","New"],["Panzer","Tank","Sherman"],["Tank", "Container"],["Epoch","Era"]],
"Tank")
= ["Panzer","Sherman","Container"];
val test2_4=get_substitutions1([["Neo","New", "Nuovo"],["Panzer","Tank","Sherman"],["Tank", "Container"],["Epoch","Era"]],
"Neo")
= ["New","Nuovo"]

val tests2 = [test2_0,test2_1, test2_2, test2_3, test2_4];

val t2 = tests_passed("2", tests2);

val test3_0=get_substitutions2([["William","Will","W"],["Thomas","Ophelia"],["Tom"]], "Will") = ["William","W"];
val test3_1=get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred")
= ["Fredrick","Freddie","F"];
val test3_2=get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff")
= ["Jeffrey","Geoff","Jeffrey"];
val test3_3=get_substitutions2([["Neo","New"],["Panzer","Tank","Sherman"],["Tank", "Container"],["Epoch","Era"]],
"Tank")
= ["Panzer","Sherman","Container"];
val test3_4=get_substitutions1([["Neo","New", "Nuovo"],["Panzer","Tank","Sherman"],["Tank", "Container"],["Epoch","Era"]],
"Neo")
= ["New","Nuovo"];

val tests3 = [test3_0,test3_1, test3_2, test3_3, test3_4];

val t3 = tests_passed("3", tests3);

val test4_0=similar_names([
["Lia", "Phee", "Ophelia"],
["William", "Harry","Elizabeth"],
["Kate", "Meghan"]
], {first="Ophelia", middle = "Louis", last="Doan"}) =
[{first="Ophelia",last="Doan",middle="Louis"},
{first="Lia",last="Doan",middle="Louis"},
{first="Phee",last="Doan",middle="Louis"}];

val test4_1=similar_names([
["Thomas", "Neo"],
["Batman", "Hulk","Bruce"],
["Spiderman", "Peter"]
], {first="Bruce", middle = "(whoknows)", last="Wayne"}) =
[{first="Bruce",last="Wayne",middle="(whoknows)"},
{first="Batman",last="Wayne",middle="(whoknows)"},
{first="Hulk",last="Wayne",middle="(whoknows)"}];

val test4_2=similar_names([
["Fred","Fredrick"],
["Elizabeth","Betty"],
["Freddie","Fred","F"]
], {first="Fred", middle="W", last="Smith"}) =
[{first="Fred",last="Smith",middle="W"},
{first="Fredrick",last="Smith",middle="W"},
{first="Freddie",last="Smith",middle="W"},
{first="F",last="Smith",middle="W"}];

val tests4 = [test4_0, test4_1, test4_2];

val t4 = tests_passed("4", tests4);

val ClubAce = (Clubs,Ace);
val DiamondsJack = (Diamonds,Jack);
val Hearts10 = (Hearts, Num 10);
val Spades5 = (Spades,Num 5);
val Club8 = (Clubs, Num 8);

val test5_0= card_color(Club8) = Black;
val test5_1= card_color(ClubAce) = Black;
val test5_2= card_color(DiamondsJack) = Red;
val test5_3= card_color(Hearts10) = Red;
val test5_4= card_color(Spades5) = Black;

val tests5 = [test5_0, test5_1, test5_2, test5_3, test5_4];

val t5 = tests_passed("5", tests5);

val test6_0= card_value(Club8) = 8;
val test6_1= card_value(ClubAce) = 11;
val test6_2= card_value(DiamondsJack) = 10;
val test6_3= card_value(Hearts10) = 10;
val test6_4= card_value(Spades5) = 5;
val test6_5= card_value(Spades, Queen) = 10;
val test6_6= card_value(Spades, King) = 10;

val tests6 = [test6_0, test6_1, test6_2, test6_3, test6_4, test6_5, test6_6];
val t6 = tests_passed("6", tests6);


exception notFound

val cards0 = [(Hearts, Ace), (Diamonds, Num 3), (Clubs, King), (Hearts, Num 8)];
val cards1 = [(Clubs, Ace), (Diamonds, Num 10), (Spades, Num 4), (Clubs, Num 4)];
val cards2 = [];
val cards3 = [(Clubs, Ace), (Diamonds, Num 10), (Spades, Num 5), (Clubs, Num 9)];
val cards4 = [(Clubs, Ace), (Clubs, Num 10), (Clubs, Num 5), (Clubs, Num 2)];
val cards5 = [(Diamonds, Ace), (Diamonds, Num 10), (Diamonds, Queen), (Diamonds, Jack), (Diamonds,King)];

val test7_0 = remove_card(cards2, (Clubs, Num 8), notFound) = [(Diamonds,Num 10),(Spades,Num 5),(Clubs,Num 9)] handle notFound => true;
val test7_1 = remove_card(cards3, (Clubs, Ace), notFound) = [(Diamonds,Num 10),(Spades,Num 5),(Clubs,Num 9)];
val test7_2 = remove_card(cards1, (Spades, Num 4), notFound) = [(Clubs, Ace), (Diamonds, Num 10), (Clubs, Num 4)];
val test7_3 = remove_card(cards3, (Clubs, Num 9), notFound) = [(Clubs,Ace),(Diamonds,Num 10),(Spades,Num 5)];
val test7_4 = remove_card(cards5, (Diamonds, Ace), notFound) = [(Diamonds, Num 10), (Diamonds, Queen), (Diamonds, Jack), (Diamonds,King)];
(* check that exception is raised*)
val test7_5 = remove_card(cards2, (Clubs, Ace), notFound) = [(Clubs, Ace)] handle notFound => true;

val tests7 = [test7_0, test7_1, test7_2, test7_3, test7_4,test7_5];
val t7 = tests_passed("7", tests7);

val test8_0 = all_same_color(cards4) = true;
val test8_1 = all_same_color(cards1) = false;
val test8_2 = all_same_color(cards2) = true;
val test8_3 = all_same_color(cards3) = false;
val test8_4 = all_same_color(cards5) = true;
val test8_5 = all_same_color(cards5) = true;

val tests8 = [test8_0, test8_1, test8_2, test8_3, test8_4,test8_5];
val t8 = tests_passed("8", tests8);

val test9_0 = sum_cards(cards0) = 32;
val test9_1 = sum_cards(cards1) = 29;
val test9_2 = sum_cards(cards2) = 0;
val test9_3 = sum_cards(cards3) = 35;
val test9_4 = sum_cards(cards4) = 28;
val test9_5 = sum_cards(cards5) = 51;

val tests9 = [test9_0, test9_1, test9_2, test9_3, test9_4,test9_5];
val t9 = tests_passed("9", tests9);

val test10_0 = score(cards0, 9) = 23 * 2;
val test10_1 = score(cards1, 1) = 28 * 2;
val test10_2 = score(cards2, 28) = 14; (* empty list is conssidered same color *)
val test10_3 = score(cards3, 35) = 0;
val test10_4 = score(cards4, 28) = 0;
val test10_5 = score([(Spades, Num 2)], 28) = 13;
val test10_6 = score([(Diamonds, Ace), (Diamonds, Num 10)],20) = 1;

val tests10 = [test10_0, test10_1, test10_2, test10_3, test10_4, test10_5, test10_6];
val t10 = tests_passed("10", tests10);

val test11_0 = officiate(cards0, [Draw, Draw, Discard (Hearts, Ace)], 9) = 2;
val test11_1 = officiate(cards3, [], 10) = 5;
val test11_2 = officiate(cards3, [Draw], 10) = 1;
val test11_3 = officiate(cards3, [Draw], 5) = 6;
val test11_4 = officiate(cards5, [Draw, Draw], 0) = 11;
val test11_5 = officiate(cards3, [Draw, Draw], 15) = 12;
val test11_6 = officiate(cards3, [Draw, Draw, Draw], 15) = 12;
val test11_7 = officiate(cards3, [Draw, Draw, Draw, Draw], 35) = 0;
val test11_8 = officiate(cards3, [Draw, Draw, Draw, Discard (Spades, Num 5)], 15) = 12;
val test11_9 = officiate(cards5, [Draw, Draw, Draw, Discard (Spades, Num 2)], 45) = 10 handle IllegalMove => true;
val test11_10 = officiate(cards2, [Draw], 10) = 5;
val test11_11 = officiate(cards2, [Discard (Spades, Ace)], 10) = 5 handle IllegalMove => true;
val test11_12 = officiate(cards3, [Draw, Discard (Spades, Num 7)], 10) = 1;

val test11_13 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],11)=11;
val test11_14 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],22)=0;
val test11_15 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],30)=4;


val tests11 = [test11_0, test11_1, test11_2, test11_3, test11_4, test11_5,
test11_6, test11_7, test11_8, test11_9, test11_10,
test11_11, test11_12, test11_13, test11_14, test11_15];

val t11 = tests_passed("11", tests11);

val total = [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11];

val totalPassed = tests_passed("**Overall", total);





