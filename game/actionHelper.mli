open Definitions
open Constants
open Util
open State
open StateUtil
open AttackUtil

(*validate (r,b) evaluates to (ra,ba)
 * Checks that the given commands are actions and returns them as:
 *  Some(Action(a)) for actions
 *  None for other commands
 *)
val validate: (command * command) -> (action option * action option)

(*draftr g c
 * Selects the first remaining steammon on the global list and adds it to
 * the steammon list for team c
 *)
val draftr: state -> color -> unit

(*draft g c s
 * Finds steammon s on the global list and adds it to the steammon list
 * for team c, if it is an invalid steammon calls (draftr g c)
 *)
val draft: state -> color -> string -> unit

(*addInvSt g c i
 * Fills the item list for c with the standard item values
 *)
val addInvSt : state -> color -> unit

(*addInv g c i
 * If the total cost of inventory i is less than cINITIAL_CASH changes the
 * item list for c to i, otherwise calls (addInvSt g c)
 *)
val addInv: state -> color -> inventory -> unit

(*order g r b evaluates to
 * 1: Red team goes first
 * 2: Blue team goes first
 * 3: Red is the only team with an action to take
 * 4: Blue is the only team with an action to take
 * 0: Neither team responded with a valid action
 *)
val order: state -> action option -> action option -> int

val item_to_int: item -> int

val selectStart: state -> color -> string -> unit
  
val switchSmon: state -> color -> string -> unit

val handleItem: state -> color -> item -> string -> unit

val handleAttack: state -> color -> string -> unit

(*Determines the validity of the action in the situation, applies it*)
val hA: state -> color -> action option -> unit

val turnEndStuff: state -> unit