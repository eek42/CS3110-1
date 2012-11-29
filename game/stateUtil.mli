open Definitions
open Util
open State

(* 
 * Create an attack from a string of the format:
 *  attack_name element PP power accuracy crit_chance effect_num effect_chance
 *)
val create_attack: string -> attack

(* 
 * Create a Steammon from a string of the format:
 *  Name Max_hp Type1 Type2 Attk1 Attk2 Attk3 Attk4 attk spl_attk def spl_def speed
 *)
val create_mon: string -> attack Table.t -> steammon

(* 
 * Choose a player to have first pick
 *)
val choose_c: unit -> color

(*
 * Create an empty game
 *)
val create_game: unit -> state

(* 
 * Create an attack from a State.attk
 *)
val datafy_attk: attk -> attack

(* 
 * Create a steammon from a State.smon
 *)
val datafy_mon: smon -> steammon

(* 
 * Create a State.attk from an attack
 *)
val atk_from_data: attack -> attk

(* 
 * Create a State.smon from a steammon
 *)
val mon_from_data: steammon -> smon

