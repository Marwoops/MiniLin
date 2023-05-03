type t

(** [equal x y] returns true if x and y are equal.*)
val equal : t -> t -> bool

(** [compare  x y] returns 0 if [x] is equal to [y] , a negative integer if [x] is less than [y] , 
    and a positive integer if [x] is greater than [y] .*)
val compare : t -> t -> int

val ( + ) : t -> t -> t
val ( - ) : t -> t -> t

val ( = ) : t -> t -> bool
val ( < ) : t -> t -> bool

(** [of_int k] returns [k]'s representation as a [t]. *)
val of_int : int -> t
