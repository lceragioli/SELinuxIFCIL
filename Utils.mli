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
