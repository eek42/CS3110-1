open Team
open Definitions
open Constants

(* Attention Student:
 * You only need to modify the handle_request function. Do not change its arguments.
 * You should change all of the inside and write many helper functions if you
 * want to have a good bot.
 *)
let _ = Random.self_init ()

let handle_request c r =
  match r with
    | StarterRequest(gs)->
        let (a1,b1) = gs in
        let my_team = if c = Red then a1 else b1 in
        let (mons, pack) = my_team in
        let pick = try List.find(fun x -> x.curr_hp > 0) mons with _ -> (List.hd mons) in
          SelectStarter(pick.species)
    | PickRequest(_, _, _, sp) ->
        (match sp with
         | h::t ->
             let length = List.length sp in
             let my_pick = List.nth sp (Random.int length) in
               print_endline ("picking " ^ my_pick.species);
               PickSteammon(my_pick.species)
         | [] -> failwith "no steammon to pick!")
    | ActionRequest (gr) ->
        let (a1, b1) = gr in
        let my_team = if c = Red then a1 else b1 in
        let (mons, [a;b;c;d;e;f;g;h]) = my_team in
		if (Random.int 100)>=10 then begin
        (match mons with
        | h::t ->
		    let attkLst = [h.first_attack;h.second_attack;h.third_attack;h.fourth_attack] in
			let attkArr = Array.of_list attkLst in
			let rec tryAttk i = 
			  if (attkArr.(i)).pp_remaining > 0 then
			    let _ = print_endline (h.species ^ "used " ^ ((attkArr.(i)).name)) in
                UseAttack((attkArr.(i)).name)
			  else
			    tryAttk (i+1 mod 4) in
			if h.curr_hp < (h.max_hp / 2) && b > 0 then UseItem(MaxPotion,h.species) else
			tryAttk (Random.int 4)
        | _ -> failwith "WHAT IN THE NAME OF ZARDOZ HAPPENED HERE") end
		else
		  let i = Random.int 100 in
		  if i <10 then SwitchSteammon((List.hd(List.tl mons)).species)
		  else if i<20 then UseItem(MaxPotion,(List.hd mons).species)
		  else if i<30 then UseItem(Revive,(List.hd(List.tl mons)).species)
		  else if i<40 then UseItem(FullHeal,(List.hd mons).species)
		  else if i<50 then UseItem(XAttack,"Mankey")
		  else if i<60 then UseItem(XDefense,"Nidoqueen")
		  else if i<70 then UseItem(XAccuracy,"Nidoqueen")
		  else if i<80 then UseItem(XSpeed,"Nidoqueen")
		  else UseItem(Ether,(List.hd mons).species)
	 | PickInventoryRequest (gr) -> PickInventory(
					[cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
	 				 cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XACCURACY;cNUM_XSPEED])
let () = run_bot handle_request
