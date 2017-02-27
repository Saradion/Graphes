#use "aux.ml"

let succ_close g s =
    let rec aux graph vset acc =
        let succs = (succ_set graph vset) in
            let acc = (VertexSet.union acc vset) in
                if (VertexSet.is_empty (VertexSet.diff succs acc)) then
                    acc
                else
                    aux graph succs acc
     in
         aux g s VertexSet.empty

let pred_close g s =
    let rec aux graph vset acc =
        let preds = (pred_set graph vset) in
            let acc = (VertexSet.union acc vset) in
                if (VertexSet.is_empty (VertexSet.diff preds acc)) then
                    acc
                else
                    aux graph preds acc
    in
        aux g s VertexSet.empty

let comps g =
    fold_vertex (fun v ss -> (VSSet.add (VertexSet.inter (succ_close g (VertexSet.singleton v)) (pred_close g (VertexSet.singleton v))) ss)) g VSSet.empty

let est_connexe g =
    let vss = (comps g) in
        (VSSet.cardinal vss == 1) && ((VertexSet.cardinal (flatten vss)) == nb_vertex g) 

let est_degre_pair g v =
    (((out_degree g v) + (in_degree g v)) land 2) == 0

let est_eulerien g =
    (est_connexe g) && (for_all_vertices (est_degre_pair g) g)

let nb_impaire g =
    fold_vertex (fun v n -> if (est_degre_pair g v) then n else n+1) g 0

let est_semi_eulerien g =
    (est_connexe g) && ((nb_impaire g) == 2)
