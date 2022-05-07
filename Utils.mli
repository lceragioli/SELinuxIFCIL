(* module StringList :
sig
  val length : string list -> int
  val compare_lengths : string list -> 'b list -> int
  val compare_length_with : string list -> int -> int
  val cons : string -> string list -> string list
  val hd : string list -> string
  val tl : string list -> string list
  val nth : string list -> int -> string
  val nth_opt : string list -> int -> string option
  val rev : string list -> string list
  val init : int -> (int -> string) -> string list
  val append : string list -> string list -> string list
  val rev_append : string list -> string list -> string list
  val concat : string list list -> string list
  val flatten : string list list -> string list
  val equal : (string -> string -> bool) -> string list -> string list -> bool
  val compare : string list -> string list -> int
  val iter : (string -> unit) -> string list -> unit
  val iteri : (int -> string -> unit) -> string list -> unit
  val map : (string -> 'b) -> string list -> 'b list
  val mapi : (int -> string -> 'b) -> string list -> 'b list
  val rev_map : (string -> 'b) -> string list -> 'b list
  val filter_map : (string -> 'b option) -> string list -> 'b list
  val concat_map : (string -> 'b list) -> string list -> 'b list
  val fold_left_map : (string -> 'b -> string * 'c) -> string -> 'b list -> string * 'c list
  val fold_left : (string -> 'b -> string) -> string -> 'b list -> string
  val fold_right : (string -> 'b -> 'b) -> string list -> 'b -> 'b
  val iter2 : (string -> 'b -> unit) -> string list -> 'b list -> unit
  val map2 : (string -> 'b -> 'c) -> string list -> 'b list -> 'c list
  val rev_map2 : (string -> 'b -> 'c) -> string list -> 'b list -> 'c list
  val fold_left2 : (string -> 'b -> 'c -> string) -> string -> 'b list -> 'c list -> string
  val fold_right2 : (string -> 'b -> 'c -> 'c) -> string list -> 'b list -> 'c -> 'c
  val for_all : (string -> bool) -> string list -> bool
  val exists : (string -> bool) -> string list -> bool
  val for_all2 : (string -> 'b -> bool) -> string list -> 'b list -> bool
  val exists2 : (string -> 'b -> bool) -> string list -> 'b list -> bool
  val mem : string -> string list -> bool
  val memq : string -> string list -> bool
  val find : (string -> bool) -> string list -> string
  val find_opt : (string -> bool) -> string list -> string option
  val find_map : (string -> 'b option) -> string list -> 'b option
  val filter : (string -> bool) -> string list -> string list
  val find_all : (string -> bool) -> string list -> string list
  val filteri : (int -> string -> bool) -> string list -> string list
  val partition : (string -> bool) -> string list -> string list * string list
  val partition_map :
    (string -> ('b, 'c) Either.t) -> string list -> 'b list * 'c list
  val assoc : string -> (string * 'b) list -> 'b
  val assoc_opt : string -> (string * 'b) list -> 'b option
  val assq : string -> (string * 'b) list -> 'b
  val assq_opt : string -> (string * 'b) list -> 'b option
  val mem_assoc : string -> (string * 'b) list -> bool
  val mem_assq : string -> (string * 'b) list -> bool
  val remove_assoc : string -> (string * 'b) list -> (string * 'b) list
  val remove_assq : string -> (string * 'b) list -> (string * 'b) list
  val split : (string * 'b) list -> string list * 'b list
  val combine : string list -> 'b list -> (string * 'b) list
  val sort : (string -> string -> int) -> string list -> string list
  val stable_sort : (string -> string -> int) -> string list -> string list
  val fast_sort : (string -> string -> int) -> string list -> string list
  val sort_uniq : (string -> string -> int) -> string list -> string list
  val merge : (string -> string -> int) -> string list -> string list -> string list
  val to_seq : string list -> string Seq.t
  val of_seq : string Seq.t -> string list
  type t = string list
end *)

module OrderPair:
  functor (Ord1 : Stdlib__set.OrderedType) (Ord2 : Stdlib__set.OrderedType) ->
    sig
      type t = Ord1.t * Ord2.t
      val compare : Ord1.t * Ord2.t -> Ord1.t * Ord2.t -> int
    end

module OrderList:
  functor (Ord : Stdlib__set.OrderedType) ->
    sig
      val length : 'a list -> int
      val compare_lengths : 'a list -> 'b list -> int
      val compare_length_with : 'a list -> int -> int
      val cons : 'a -> 'a list -> 'a list
      val hd : 'a list -> 'a
      val tl : 'a list -> 'a list
      val nth : 'a list -> int -> 'a
      val nth_opt : 'a list -> int -> 'a option
      val rev : 'a list -> 'a list
      val init : int -> (int -> 'a) -> 'a list
      val append : 'a list -> 'a list -> 'a list
      val rev_append : 'a list -> 'a list -> 'a list
      val concat : 'a list list -> 'a list
      val flatten : 'a list list -> 'a list
      val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
      val iter : ('a -> unit) -> 'a list -> unit
      val iteri : (int -> 'a -> unit) -> 'a list -> unit
      val map : ('a -> 'b) -> 'a list -> 'b list
      val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
      val rev_map : ('a -> 'b) -> 'a list -> 'b list
      val filter_map : ('a -> 'b option) -> 'a list -> 'b list
      val concat_map : ('a -> 'b list) -> 'a list -> 'b list
      val fold_left_map :
        ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
      val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
      val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
      val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
      val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
      val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
      val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
      val fold_right2 :
        ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
      val for_all : ('a -> bool) -> 'a list -> bool
      val exists : ('a -> bool) -> 'a list -> bool
      val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
      val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
      val mem : 'a -> 'a list -> bool
      val memq : 'a -> 'a list -> bool
      val find : ('a -> bool) -> 'a list -> 'a
      val find_opt : ('a -> bool) -> 'a list -> 'a option
      val find_map : ('a -> 'b option) -> 'a list -> 'b option
      val filter : ('a -> bool) -> 'a list -> 'a list
      val find_all : ('a -> bool) -> 'a list -> 'a list
      val filteri : (int -> 'a -> bool) -> 'a list -> 'a list
      val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
      val partition_map :
        ('a -> ('b, 'c) Either.t) -> 'a list -> 'b list * 'c list
      val assoc : 'a -> ('a * 'b) list -> 'b
      val assoc_opt : 'a -> ('a * 'b) list -> 'b option
      val assq : 'a -> ('a * 'b) list -> 'b
      val assq_opt : 'a -> ('a * 'b) list -> 'b option
      val mem_assoc : 'a -> ('a * 'b) list -> bool
      val mem_assq : 'a -> ('a * 'b) list -> bool
      val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
      val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
      val split : ('a * 'b) list -> 'a list * 'b list
      val combine : 'a list -> 'b list -> ('a * 'b) list
      val sort : ('a -> 'a -> int) -> 'a list -> 'a list
      val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
      val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
      val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list
      val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
      val to_seq : 'a list -> 'a Seq.t
      val of_seq : 'a Seq.t -> 'a list
      type t = Ord.t list
      val compare : Ord.t list -> Ord.t list -> int
    end

module SS :
  sig
    type elt = String.t
    type t = Stdlib__set.Make(String).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
val ( << ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val listminus : 'a list -> 'a list -> 'a list option
val replace : string -> string -> string -> string
val remove_space : string -> string
val slist_to_ss : SS.elt list -> SS.t
val tl_flatten : 'a list list -> 'a list
val last_and_list : 'a list -> 'a * 'a list
val list_find_map : ('a -> 'b option) -> 'a list -> 'b option
val prefix_list : 'a list -> 'a list list
val id : 'a -> 'a
val breake : 'a list -> ('a list * 'a list) list
val times : ('a * 'b) list -> ('c * 'd) list -> (('a * 'c) * ('b * 'd)) list
val times_list : 'a list -> 'b list -> ('a * 'b) list
val remove_duplicate : 'a list -> 'a list
val make_pairs :
  'a list -> 'b list -> (('a list * 'b list) * ('a list * 'b list)) list
val list_start_with : 'a list -> 'a list -> bool
