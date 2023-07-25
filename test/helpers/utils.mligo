(* Recurssive function that can be used to apply many times the given function _f_. 
The arguments passed to _f_ is generated by the _gen_args_ function  *)
let rec repeat (type a) (count, f, gen_args, acc : nat * (a -> test_exec_result) * (nat -> a) * test_exec_result list) : test_exec_result list =
    if count <= 0n then 
        acc
    else
        let args = gen_args count in
        let res = f(args) in
        repeat(abs(count - 1n), f, gen_args, res :: acc)

let rec generic_repeat (type a b) (count, f, gen_args, acc : nat * (a -> b) * (nat -> a) * b list) : b list =
    if count <= 0n then 
        acc
    else
        let args = gen_args count in
        let res = f(args) in
        generic_repeat(abs(count - 1n), f, gen_args, res :: acc)

module List = struct

    // reverse list
    let rev (type a) (lst : a list) : a list =
        List.fold (fun (acc, i : a list * a) : a list -> i :: acc) lst ([]: a list)


    // partition a list s for a given pivot p 
    let rec partition (type a) (acc : a list * a list) (le : (a * a -> bool)) (p: a) (s: a list) : a list * a list =
        let (left, right) = acc in
        match s with
        | [] -> acc
        | hd :: tl ->
            if le(hd, p) then
                partition (hd :: left, right) le p tl
            else 
                partition (left, hd :: right) le p tl

    //  USAGE exemple
    // let test : int list = [ 5; -10; 42; 0; -32; 11; 13; -8] in
    // let sorted_test : int list = Utils.List.quicksort test (fun (a,b: int * int) -> (a <= b)) in
    // let () = Test.log(sorted_test) in
    let rec quicksort (type a) (lst : a list) (cmp: (a * a -> bool)) : a list =
        match lst with
        | [] -> []
        | hd::tl ->
            let (left, right) = partition (([]: a list), ([]: a list)) cmp hd tl in
            List.fold (fun (acc, i : a list * a) : a list -> i :: acc) (rev (quicksort left cmp)) (hd :: (quicksort right cmp))
  


    // compare 2 list
    let rec equal (type a) (lst1, lst2, eq_func: a list * a list * (a * a -> bool)) : bool =
        if (List.length lst1 = List.length lst2) then
            match (lst1, lst2) with
            | ([], []) -> true
            | ([], _) -> false
            | (_, []) -> false
            | (x::xs, y::ys) -> 
                if eq_func(x, y) then
                    equal(xs, ys, eq_func)
                else 
                    false
        else 
            false

    // removes duplicates 
    let rec nub_aux (type a) (lst, acc : a list * (a set * a list)) : (a set * a list) = 
        match lst with 
        | hd::tl -> 
            if Set.mem hd acc.0 then 
                nub_aux(tl, (acc.0, hd::acc.1))
            else
                nub_aux(tl, (Set.add hd acc.0, hd::acc.1))
        | [] -> acc 

    // removes duplicates 
    let nub (type a) (lst : a list) : a list = 
        let (_, result) = nub_aux(lst, ((Set.empty : a set), ([] : a list))) in
        rev result

    // Standard truncated zipwith
    // l1 = [a; b; c; d]
    // l2 = [1; 2; 3 ]
    // result = [f(a,1); f(b,2); f(c,3)]
    let rec zipWith (type a b c) (f: ((a * b) -> c)) (lst1 : a list) (lst2: b list) : c list =
        match (lst1, lst2) with
        | ([], _) -> []
        | (_, []) -> []
        | (x::xs, y::ys) -> (f (x, y)) :: zipWith f xs ys

    // traverse list and apply f to elements two by two
    // l1 = [a; b; c; d]
    // result = [f(a,b); f(b,c); f(c,d)]
    let rec mapAdjacent (type a b) (lst, f: a list * (a * a -> b)) : b list =
        match lst with
        | [] -> []
        | [_hd] -> []
        | _hd::tl -> zipWith f lst tl

    // custom implementation of mapAdjacent
    let rec mapAdjacent_aux (type a b) (lst, last, compare, acc: a list * a option * (a * a -> b) * b list) : b list =
        match lst with
        | hd::tl -> 
            if Option.is_none last then
                mapAdjacent_aux(tl, Some(hd), compare, acc)
            else
                let result = compare(Option.unopt last, hd) in
                mapAdjacent_aux(tl, Some(hd), compare, result :: acc)
        | [] -> acc
    
    let mapAdjacent_ (type a b) (lst, compare: a list * (a * a -> b)) =
        mapAdjacent_aux(lst, (None: a option), compare, ([]: b list))

end

// exemple of usage
    // with a mapAdjacent_ (home made)
    // let compare_index(a, b : (Cfmm.tick_index * Cfmm.tick_state) * (Cfmm.tick_index * Cfmm.tick_state)) : bool = a.0.i < b.0.i in
    // let all_comparison = Utils.List.mapAdjacent_(all_ticks, compare_index) in
    // let _ : unit = List.iter (fun (i:bool) -> assert (i)) all_comparison in

    // with a zipWith
    // let all_ticks_minus_first = Option.unopt (List.tail_opt all_ticks) in
    // let compare_tick_index(a, b : (Cfmm.tick_index * Cfmm.tick_state) * (Cfmm.tick_index * Cfmm.tick_state)) : bool = a.0.i < b.0.i in
    // let all_comparison = Utils.List.zipWith compare_tick_index all_ticks all_ticks_minus_first in
    // let _ : unit = List.iter (fun (i:bool) -> assert (i)) all_comparison in

    // with a mapAdjacent (who is using zipWith)
    // let compare_index(a, b : (Cfmm.tick_index * Cfmm.tick_state) * (Cfmm.tick_index * Cfmm.tick_state)) : bool = a.0.i < b.0.i in
    // let all_comparison = Utils.List.mapAdjacent(all_ticks, compare_index) in
    // let _ : unit = List.iter (fun (i:bool) -> assert (i)) all_comparison in