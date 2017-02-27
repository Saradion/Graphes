#use "aux.ml"

let rec forward l p =
    match l with
    | [] -> p;
    | h::t -> if ((List.hd p) == (E.dst h)) then (forward t p) else (E.dst h)::p;;


