module Make(M : MonadSigs.MONAD) : sig
  val fold_right_mon
      : ('a -> 'b -> 'b M.t) -> 'a list -> 'b -> 'b M.t
  val fold_right2_mon
      : ('a -> 'b -> 'c -> 'c M.t) -> 'a list -> 'b list -> 'c -> 'c M.t
  val for_all_mon : ('a -> bool M.t) -> 'a list -> bool M.t
end
