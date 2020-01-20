(** This mutable data structure represents the [n] last elements of
    a stream. For efficiency reasons, we implement it using an array
    with moving bounds. *)

type 'a t = {
    mutable start   : int;
    (* The most ancient element is located at [start]. *)

    mutable size    : int;
    (* The number of elements in the queue. *)

            limit   : int;

    mutable buffer  : 'a array;
    (* This array contains the sequence of buffered elements.  It must
       be regarded as a circular sequence: the element next to the
       final element is 0 and the element just before 0 is the final
       element. *)
}

let make limit =
  {
    start  = 0;
    size   = 0;
    buffer = [||];
    limit;
  }

let limit b =
  Array.length b.buffer

let next b i =
  (i + 1) mod (limit b)

let size b =
  b.size

exception EmptyBoundedBuffer

let pop b =
  if size b = 0 then raise EmptyBoundedBuffer;
  b.start <- next b b.start;
  b.size <- b.size - 1

exception InvalidAccess

let idx b i =
  (b.start + b.size - i - 1) mod (limit b)

let get b i =
  let n = size b in
  if n = 0 then raise EmptyBoundedBuffer;
  if i >= n then raise InvalidAccess;
  b.buffer.(idx b i)

let push b x =
  if b.buffer = [||] then b.buffer <- Array.make b.limit x;
  if size b = limit b then pop b;
  b.size <- b.size + 1;
  b.buffer.(idx b 0) <- x
