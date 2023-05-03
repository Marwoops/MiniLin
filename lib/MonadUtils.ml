module Make(M : MonadSigs.MONAD) = struct
  open M

  let rec fold_right_mon f xs acc =
    match xs with
    | [] ->
       return acc
    | x :: xs ->
       let* acc = fold_right_mon f xs acc in
       f x acc

  let rec fold_right2_mon
            (f : 'a -> 'b -> 'c -> 'c t)
            (xs : 'a list) (ys : 'b list) (acc : 'c) : 'c t =
    match xs, ys with
    | [], [] ->
       return acc
    | x :: xs, y :: ys ->
       let* acc = fold_right2_mon f xs ys acc in
       f x y acc
    | _, [] | [], _ ->
       invalid_arg "fold_right2_mon"

  let rec for_all_mon (f : 'a -> bool t) (xs : 'a list) : bool t =
    match xs with
    | [] -> return true
    | x :: xs ->
        let* b1 = f x in
        let* b2 = for_all_mon f xs in
        return (b1 && b2)
end
