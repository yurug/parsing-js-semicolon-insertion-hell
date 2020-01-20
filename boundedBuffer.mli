(** Implement an impure bounded buffer. *)
type 'a t

exception InvalidAccess

exception EmptyBoundedBuffer

val make : int -> 'a t

val get : 'a t -> int -> 'a

val push : 'a t -> 'a -> unit

val limit : 'a t -> int

val size : 'a t -> int
