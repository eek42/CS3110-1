open Definitions
open Constants
open Util
open State

exception GameOver of color

exception Fainted of (color * string)

type md = At | D | S | Ac

val get_mult: md -> int -> float
  
val changeMod: state -> string -> md -> int -> unit

val calc_speed: state -> string -> float

val sprint: state -> int
  
val validAttack: state -> color -> string -> bool

val attackHappens: state -> color -> string -> attk -> bool
   
val calcCrit: attk -> float

val calcSTAB: state -> string -> attk -> float

val calcWeakness: state -> string -> attk -> float

val calcAttack: state -> string -> attk -> float

val calcDef: state -> string -> attk -> float

val calcEffect: state -> attk -> string -> string -> unit