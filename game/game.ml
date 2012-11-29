open Definitions
open Util
open Constants
open Netgraphics
open State
open StateUtil
open AttackUtil
open ActionHelper

(* You have to implement this. Change it from int to your own state type*)
type game = state 

let game_datafication (g : game) : game_status_data =
  let mons = !(g.mons) in
  let rs = List.map (fun s -> datafy_mon (List.assoc s mons)) !(g.red_mons) in
  let bs = List.map (fun s -> datafy_mon (List.assoc s mons)) !(g.blue_mons) in
  ((rs , Array.to_list !(g.red_items)) , (bs, Array.to_list !(g.blue_items)))
	
let game_from_data (((rs, ri) , (bs, bi)) : game_status_data) : game =
  let names lst = List.map (fun s -> s.species) lst in
  let monl = List.map (fun s -> (s.species, mon_from_data s)) (rs@bs) in
  {red_mons = ref (names rs);
   blue_mons = ref (names bs);
   mons = ref monl;
   red_items = ref (Array.of_list ri);
   blue_items = ref (Array.of_list bi)}

let init_game () =
  send_update InitGraphics;
  let atkList = match read_lines "attack.txt" with
    | h::t -> t
    | _ -> raise (Runtime "No attacks, or missing attack format comment") in
  let attks = Table.create (List.length atkList) in (*hashtable of attks*)
  let atkAdd str =
    let atk = create_attack str in 
	Table.add attks atk.name atk in
  List.iter atkAdd atkList; (*create attks and add them to hashtable*)
  let monList = read_lines "steammon.txt" in
 (if (List.length monList) < (2*cNUM_PICKS) then 
    raise(Runtime "Too few Steammon Provided")
  else ());
  mon := Table.create (List.length monList); (*hashtable of smon*)
  let monAdd str =
    let variant_of_mon = create_mon str attks in
	Table.add (!mon) (variant_of_mon.species) (variant_of_mon) in
  List.iter monAdd monList; (*create smon and add them to the hashtable*)
  let g = create_game () in
  gAttacks := hash_to_list attks;
  let steammon = hash_to_list (!mon) in
  let starter = choose_c () in
  prev_reqs := if starter=Red 
     then (Some(Request(PickRequest(Red, game_datafication g,!gAttacks,steammon))),None)
     else (None,Some(Request(PickRequest(Blue, game_datafication g,!gAttacks,steammon))));
  phase := Draft;
  (g, starter, !gAttacks, steammon)

  
let handle_step (g : game) (ra: command) (ba : command) : game_output =
  let (nra, nba) = validate (ra, ba) in
  match !phase with
  |Initialization -> raise (Runtime "Game not initialized")
  |Draft -> ( (*DRAFT PHASE - only PickSteammon actions will be fielded*)
    let col = ref Red in
	( match !prev_reqs with (*Draft steammon*)
      |(Some(Request(PickRequest(_))), None) -> (*Red's turn*)
	      (col := Red;
	       match nra with
           | Some(PickSteammon(s)) -> draft g Red s;
           | _ -> draftr g Red;)									
	  |(None, Some(Request(PickRequest(_)))) -> (*Blue's Turn*)
	      (col := Blue;
	      match nba with
          | Some(PickSteammon(s)) -> draft g Blue s;
          | _ -> draftr g Blue;)
	  | _ -> raise (Runtime "No previous PickRequest in Draft Phase") );
	let gsd = game_datafication g in
	let l = List.length (!(g.mons)) in
	let rr = ref None in
	let br = ref None in
   (if l = 2*cNUM_PICKS (*Both teams are full, move on to Inventory phase*)
	then (
	  rr := Some(Request(PickInventoryRequest(gsd)));
	  br := Some(Request(PickInventoryRequest(gsd)));
      phase := Inventory	  )
    else ( (*Not full, send PickRequest*)
     (if not( (l=2) || (l=4) ) then col := (invert_color (!col)) else () );
      match !col with
		Red -> rr := Some(Request(PickRequest(Red, gsd, !gAttacks, hash_to_list !mon)));
		| Blue -> br := Some(Request(PickRequest(Blue, gsd, !gAttacks, hash_to_list !mon))); ));
	prev_reqs := (!rr,!br); (*Set previouse requests to outgoing requests*)
	(None, gsd, !rr, !br) 
	)	
  |Inventory -> ( (*INVENTORY PHASE - ony PickInventory actions will be fielded*)
   (match (nra,nba) with (*Set inventories*)
	 |(Some(PickInventory(i)),Some(PickInventory(j))) -> addInv g Red i; addInv g Blue j
	 |(Some(PickInventory(i)),None) -> addInv g Red i; addInvSt g Blue
	 |(None,Some(PickInventory(j))) -> addInvSt g Red; addInv g Blue j
	 | _ -> addInvSt g Red; addInvSt g Blue );
	 let gsd = game_datafication g in
	 phase := Battle; (*Move on to Battle phase*)
	 prev_reqs := (Some(Request(StarterRequest(gsd))), Some(Request(StarterRequest(gsd))));
	 (None, gsd, fst (!prev_reqs), snd (!prev_reqs))
     )
  |Battle -> (
    let ord = order g nra nba in
	try (
	 (if ord = 1 then 
	    (Netgraphics.add_update(SetFirstAttacker(Red)); 
		 hA g Red nra; hA g Blue nba) (*Red first*)
	  else if ord = 2 then 
	    (Netgraphics.add_update (SetFirstAttacker(Blue)); 
		 hA g Blue nba; hA g Red nra) (*Blue first*)
	  else if ord = 3 then 
	    (Netgraphics.add_update (SetFirstAttacker(Red)); 
		 hA g Red nra) (*Blue non-response*)
	  else if ord = 4 then 
	    (Netgraphics.add_update (SetFirstAttacker(Blue)); 
		 hA g Blue nba)(*Red non-response*)
	  else () ); (*No response*)
	  turnEndStuff g;
	  let gsd = game_datafication g in
	  let r = Some(Request(ActionRequest(gsd))) in
	  prev_reqs := (r,r);
	  (None, gsd, r, r))
    with 
	  GameOver(c) -> (Some(Winner(c)), game_datafication g, None, None)
	  |Fainted(c,s) -> let m = get_smon g s in (*find fainted mon*)
	    Netgraphics.add_update (Message(s^" fainted!"));
	    let u = (s,0,get_max_hp g s,c) in
	    Netgraphics.add_update (UpdateSteammon(u));
	    m.scurr_hp := 0; (*make sure it's dead*)
        m.sstatus := []; (*Remove all statuses*)
		m.smods := {attack_mod=0; speed_mod=0; 
		            defense_mod=0; accuracy_mod=0}; (*Remove all modifiers*)
		if (num_left g c)<= 0 then 
		  (Some(Winner(invert_color c)), game_datafication g, None, None)
		else
	 	  let rr = ref None in
	      let br = ref None in
		  let gsd = game_datafication g in
	     (if c=Red then rr := Some(Request(StarterRequest(gsd))) (*Red starter*)
		  else br := Some(Request(StarterRequest(gsd))) ); (*Blue starter*)
		  prev_reqs := (!rr, !br);
		  (None, gsd, !rr, !br)
    ) 
