open Definitions
open Util

    type attk = {
      sname: string;
      selement: steamtype;
      smax_pp: int;
      spp_remaining: int ref;
      spower : int;
      saccuracy: int;
      scrit_chance: int;
      seffect: attack_effect
    }

    type smon = {
      sspecies: string;
      scurr_hp: int ref;
      smax_hp: int;
      sfirst_type: steamtype option;
      ssecond_type: steamtype option;
      sattks: (string * attk) list;
      sattack: int;
      sspl_attack: int;
      sdefense: int;
      sspl_defense: int;
      sspeed: int;
      sstatus: status list ref;
      smods: modifier ref
    }
	
    type state = {
      red_mons: string list ref;
      blue_mons: string list ref;
	  mons: (string * smon) list ref;
      red_items: int array ref;
      blue_items: int array ref;
    }
	
	type step = Initialization
      | Draft
      | Inventory
      | Battle 
	
	exception Runtime of string 
	
	exception Mon_not_found of string
	
	let (phase : step ref) = ref Initialization 

    let (prev_reqs : (command option * command option) ref) = ref (None, None) 

    let (mon : steammon Table.t ref) = ref (Table.create 1) 

    let (gAttacks : attack list ref) = ref [] 
	
	let get_smon (st:state) (stmon:string) : smon = 
	  try (List.assoc stmon !(st.mons))
	  with Not_found -> raise (Mon_not_found (stmon))
	let get_team_list (st:state) (c:color) : string list ref =
	  if c=Red then (st.red_mons) else (st.blue_mons)
	
	let get_max_hp (st:state) (stmon:string) : int = (get_smon st stmon).smax_hp
	let get_curr_hp (st:state) (stmon:string) : int ref = (get_smon st stmon).scurr_hp
    let get_types (st:state) (stmon:string) : steamtype list = 
	  let m = (get_smon st stmon) in
	  match (m.sfirst_type, m.ssecond_type) with
        (Some(x),Some(y)) -> x::[y]
        | (Some(x),None) -> [x]
        | _ -> []
	let get_attk (st:state) (stmon:string) (atk:string) : attk =
	  List.assoc atk ((get_smon st stmon).sattks)
	let get_attks (st:state) (stmon:string) : string list =
	  let m = (get_smon st stmon) in
	  List.fold_right (fun (a,b) l -> a::l) m.sattks []
	let get_attack (st:state) (stmon:string) : int = (get_smon st stmon).sattack
	let get_spl_attack (st:state) (stmon:string) : int = (get_smon st stmon).sspl_attack
	let get_def (st:state) (stmon:string) : int = (get_smon st stmon).sdefense
	let get_spl_def (st:state) (stmon:string) : int = (get_smon st stmon).sspl_defense
	let get_speed (st:state) (stmon:string) : int = (get_smon st stmon).sspeed
	let get_status (st:state) (stmon:string) : status list ref = (get_smon st stmon).sstatus
    let get_modifiers (st:state) (stmon:string) : modifier ref = (get_smon st stmon).smods
	
	let get_elem st stmon atk = 
	  let m = List.assoc stmon !(st.mons) in
	  (List.assoc atk m.sattks).selement
    let get_power st stmon atk =
	  let m = List.assoc stmon !(st.mons) in
	  (List.assoc atk m.sattks).spower
	let get_pp st stmon atk =
	  let m = List.assoc stmon !(st.mons) in
	  (List.assoc atk m.sattks).spp_remaining
	let get_max_pp st stmon atk =
	  let m = List.assoc stmon !(st.mons) in
	  (List.assoc atk m.sattks).smax_pp
    let get_acc st stmon atk =
	  let m = List.assoc stmon !(st.mons) in
	  (List.assoc atk m.sattks).saccuracy
    let get_crit_ch st stmon atk =
	  let m = List.assoc stmon !(st.mons) in
	  (List.assoc atk m.sattks).scrit_chance
	let get_effect st stmon atk =
	  let m = List.assoc stmon !(st.mons) in
	  (List.assoc atk m.sattks).seffect
	  
    let get_starter (st:state) (c:color) : string =
	  let lst = if c=Red then !(st.red_mons) else !(st.blue_mons) in
	  match lst with
	  [] -> raise (Runtime "No Steammon found; no starter")
	  |h::t -> h
	let is_fainted (st:state) (stmon:string) : bool = 
	  0 = !(get_curr_hp st stmon)
	let num_left (st:state) (c:color) : int =
	  let f acc s = if (is_fainted st s) then acc else acc+1 in
	  let counter lst = List.fold_left f 0 lst in
	  if c=Red then counter !(st.red_mons) else counter !(st.blue_mons)
	let get_color (g:state) (m:string) : color =
	  if List.mem m !(g.red_mons) then Red
	  else if List.mem m !(g.blue_mons) then Blue
	  else raise (Runtime "This pokemon is not on either team")
