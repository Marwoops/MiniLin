module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
end

module type Comonad = sig
  type 'a t
  val get : 'a t -> 'a
  val cobind : 'a t -> ('a t -> 'b) -> 'b t
end
