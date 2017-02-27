#use "exemples_graphes.ml"

let est_connexe g =
    (List.length (Components.scc_list g)) == 1

let degree g v =
    (out_degree g v) + (in_degree g v)

let est_degre_pair g v =
    ((degree g v) land 2) == 0

let nb_impair g v =
    fold_vertex (fun v n -> if (est_degre_pair g v) then n else n+1) g 0

let est_semi_eulerien g =
    (est_connexe g) && ((nb_impair g) == 2)
