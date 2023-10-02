open IFCILsyntax

module Block :
  sig
    type t = 
      { 
        rules: statement list;
        abstract: bool;
        nested: ns list;
      }
    val emptyframe : t
    val union : t -> t -> t
  end

type commands = Block.t Utils.SLM.t 

val from_config: statement list -> commands
val print : commands -> unit

