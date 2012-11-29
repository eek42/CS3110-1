open Definitions
open Constants
open Util
open State 
open StateUtil
open AttackUtil

let validate (a,b) =
  match (a,b) with
  (Action(x),Action(y)) -> (Some(x), Some(y))
  | ( Action(x), _ ) -> (Some(x),None)
  | ( _ , Action(y)) -> (None, Some(y))
  | _ -> (None,None) 
  
let draftr g c =
  let monses = hash_to_list !mon in
  let m = List.hd monses in
  Table.remove (!mon) m.species;
  let u = (m.species, m.curr_hp, m.max_hp,c) in
  Netgraphics.add_update (UpdateSteammon(u));
 (if c = Red 
  then g.red_mons := (m.species)::(!(g.red_mons)) 
  else g.blue_mons := (m.species)::(!(g.blue_mons)));
  g.mons := (m.species, mon_from_data m)::(!(g.mons)) 

let draft g c s =
  try (
    let m = Table.find (!mon) s in
    Table.remove (!mon) m.species;
	let u = (m.species, m.curr_hp, m.max_hp,c) in
	Netgraphics.add_update (UpdateSteammon(u));
   (if c = Red 
    then g.red_mons := (m.species)::(!(g.red_mons))
    else g.blue_mons := (m.species)::(!(g.blue_mons)));
    g.mons := (m.species, mon_from_data m)::(!(g.mons)) )
  with Not_found -> draftr g c 

let addInvSt g c =
  let i = [cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL
           ;cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XSPEED;cNUM_XACCURACY] in
  if c=Red then 
    g.red_items := (Array.of_list i) 
  else 
    g.blue_items := (Array.of_list i)

let addInv g c i =
  match i with
  |e::m::r::fh::xat::xd::xs::xac::[] ->
    let cost = ((cCOST_ETHER*e)+(cCOST_MAXPOTION*m)
	+(cCOST_FULLHEAL*fh)+(cCOST_REVIVE*r)+(cCOST_XATTACK*xat)
	+(cCOST_XDEFEND*xd)+(cCOST_XSPEED*xs)+(cCOST_XACCURACY*xac)) in
	if (cost > cINITIAL_CASH) then addInvSt g c
	else if c=Red then g.red_items := (Array.of_list i) 
	else g.blue_items := (Array.of_list i)
  | _ -> addInvSt g c  
   
let order g ra ba =
  match (ra, ba) with
  |(None,None) -> 0
  |(_,None) -> 3
  |(None,_) -> 4
  |(_,Some(SwitchSteammon(_))) -> 2
  |(Some(UseAttack(_)),Some(UseItem(_))) -> 2
  |(Some(UseAttack(_)),Some(UseAttack(_))) -> sprint g
  |_ -> 1
  
let item_to_int (i : item) : int =
  match i with
  Ether -> 0
  |MaxPotion -> 1
  |Revive -> 2
  |FullHeal -> 3
  |XAttack -> 4
  |XDefense -> 5
  |XSpeed -> 6
  |XAccuracy -> 7

let selectStart (g:state) (c:color) (s:string) : unit =
  let lst = get_team_list g c in
  if (List.mem s !lst) && not(is_fainted g s) then
   (Netgraphics.add_update (SetChosenSteammon(s));
    Netgraphics.add_update (Message((color_to_string c)^" sent out "^s^"!"));
    lst := s::(List.filter (fun n -> not(n=s)) !lst))
  else (
    let lst2 = List.filter (fun x -> not(is_fainted g x)) !lst in
    let m = List.hd lst2 in
	lst := m::(List.filter (fun n -> not(n=m)) !lst);
    Netgraphics.add_update (SetChosenSteammon(m));
    Netgraphics.add_update (Message((color_to_string c)^" sent out "^m^"!")))
  
let switchSmon (g:state) (c:color) (s:string) : unit = 
  let lst = get_team_list g c in
  if (List.mem s !lst) && not(is_fainted g s) && not(s=(List.hd !lst)) then
   (let old = get_smon g (List.hd !lst) in
	Netgraphics.add_update (Message((color_to_string c)^" withdrew "^old.sspecies^" and..."));
	Netgraphics.add_update (Message((color_to_string c)^" sent out "^s^"!"));
	Netgraphics.add_update (SetChosenSteammon(s));
	old.smods := {attack_mod=0; speed_mod=0; defense_mod=0; accuracy_mod=0};
	old.sstatus := List.filter (fun x -> not(x=Confused)) !(old.sstatus);
    lst := s::(List.filter (fun n -> not(n=s)) !lst))
  else
    ()

let handleItem (g:state) (c:color) (i:item) (s:string) =
  let items = if c=Red then !(g.red_items) else !(g.blue_items) in
  if items.(item_to_int i) <= 0 then 
    ()
  else (
    let lst = get_team_list g c in
    items.(item_to_int i) <- items.(item_to_int i) - 1;
    match i with
    Ether ->
	  if (List.mem s !lst) then
	   (Netgraphics.add_update (Message((color_to_string c)^" used Ether!"));
	    let atks = get_attks g s in
		let f a = (get_pp g s a):= min (!(get_pp g s a)+5) (get_max_pp g s a) in
		List.iter f atks)
	  else
	    Netgraphics.add_update (Message((color_to_string c)^" tried to use an item illegally..."))
    |MaxPotion ->
	  if (List.mem s !lst) && not(is_fainted g s) then
	   (let max = (get_max_hp g s) in
	    let curr = (get_curr_hp g s) in
	    Netgraphics.add_update (Message((color_to_string c)^" used MaxPotion on "^s^"!"));
	    Netgraphics.add_update (PositiveEffect("MaxPotion",c,(max-(!curr))));
	    Netgraphics.add_update (UpdateSteammon(s,max,max,c));
	    curr := max)
	  else 
	    Netgraphics.add_update (Message((color_to_string c)^" tried to use an item illegally..."))
    |Revive -> 
	  if (List.mem s !lst) && (is_fainted g s) then 
	   (let max = get_max_hp g s in
	    Netgraphics.add_update (Message((color_to_string c)^" used Revive on "^s^"!"));
	    Netgraphics.add_update (UpdateSteammon(s,max/2,max,c));
	    (get_curr_hp g s) := max / 2 )
	  else 
	    Netgraphics.add_update (Message((color_to_string c)^" tried to use an item illegally..."))
    |FullHeal -> 
	  if (List.mem s !lst) then
       (Netgraphics.add_update (Message((color_to_string c)^" used FullHeal on "^s^"!"));
	    Netgraphics.add_update (SetStatusEffects(s,[]));
	    (get_status g s) := [] )
	  else
	    Netgraphics.add_update (Message((color_to_string c)^" tried to use an item illegally..."))
    |XAttack -> 
	   Netgraphics.add_update (Message((color_to_string c)^" used XAttack on "^(get_starter g c)^"!"));
	   changeMod g (get_starter g c) At 1
    |XDefense -> 
	   Netgraphics.add_update (Message((color_to_string c)^" used XDefense on "^(get_starter g c)^"!"));
	   changeMod g (get_starter g c) D 1
    |XSpeed -> 
	   Netgraphics.add_update (Message((color_to_string c)^" used XSpeed on "^(get_starter g c)^"!"));
	   changeMod g (get_starter g c) S 1
    |XAccuracy -> 
	   Netgraphics.add_update (Message((color_to_string c)^" used XAccuracy on "^(get_starter g c)^"!"));
	   changeMod g (get_starter g c) Ac 1 )

let handleAttack (g:state) (c:color) (s:string) =
  if not(validAttack g c s) then
    (Netgraphics.add_update (Message((color_to_string c)^" tried to use an illegal attack...")))
  else (
    let m = get_starter g c in (*attacking pokemon*)
	let oppM = get_starter g (invert_color c) in 
    let attack = get_attk g (get_starter g c) s in (*attack used*)
	(get_pp g m s) := !(get_pp g m s) - 1; (*decrement pp*)
	if not(attackHappens g c m attack) then () (*deal with possible nonattack*)
	else ( (*calculate attack amount terms, then calculate damage*)
	  Netgraphics.add_update (Message(m^" used "^s^"!"));
	  if (Random.int 100) < attack.saccuracy then
	   (let crit = calcCrit attack in
	    let stab = calcSTAB g m attack in
	    let sm = calcWeakness g oppM attack in
	    let p = float_of_int (attack.spower) in
	    let a = calcAttack g m attack in
	    let d = calcDef g oppM attack in
	    let damage = int_of_float ((p *. a *. crit *. stab *. sm) /. d) in (*calc damage*)
	    let hp = get_curr_hp g oppM in
	    let new_hp = !hp - damage in
	    if new_hp<=0 then raise (Fainted ((invert_color c),oppM)) (*check faint*)
	    else (
	      Netgraphics.add_update (NegativeEffect(s, invert_color c, damage));
	      let u = (oppM, new_hp, get_max_hp g oppM, invert_color c) in
	      Netgraphics.add_update (UpdateSteammon(u));
	      hp := new_hp; (*change hp*)
		  calcEffect g attack m oppM)) (*carry out attack effects*)
	  else (Netgraphics.add_update (Message(m^"'s attack missed!")))
	) 
  )

(*Determines the validity of the action in the situation, applies it*)
let hA (g:state) (c:color) (a: action option) =
  let last = if c=Red then fst !prev_reqs else snd !prev_reqs in
  match (last,a) with
  (Some(Request(StarterRequest(_))),Some(SelectStarter(s))) -> selectStart g c s
  |(Some(Request(ActionRequest(_))),Some(SwitchSteammon(s))) -> switchSmon g c s
  |(Some(Request(ActionRequest(_))),Some(UseItem(i,s))) -> handleItem g c i s
  |(Some(Request(ActionRequest(_))),Some(UseAttack(s))) -> handleAttack g c s
  |_ -> () 
  
let turnEndStuff (g:state) =
  let checkPoison m = List.mem Poisoned !(get_status g m) in
  let lst = List.filter checkPoison (!(g.red_mons)@(!(g.blue_mons))) in
  let pDamage m = 
      let c = get_color g m in
      let hp = get_curr_hp g m in
	  Netgraphics.add_update (Message(m^"'s hurt by poison!"));
	  let damage = int_of_float (cPOISON_DAMAGE *. (float_of_int (get_max_hp g m))) in
	  let new_hp = !hp - damage in
	  if new_hp<=0 then (
        if m=(get_starter g c) then raise (Fainted (c,m)) (*check faint*)
		else (
		  Netgraphics.add_update (Message(m^" fainted!"));
	      let u = (m,0,get_max_hp g m,c) in
	      Netgraphics.add_update (UpdateSteammon(u));
		  (get_status g m) := []; (*Remove all statuses*)
		  (get_modifiers g m) := {attack_mod=0; speed_mod=0; 
		                  defense_mod=0; accuracy_mod=0};
          if (num_left g c) <= 0 then raise (GameOver(invert_color c)) else ()))
	  else (
        Netgraphics.add_update (NegativeEffect(" ", c, damage));
	    let u = (m, new_hp, get_max_hp g m, c) in
	    Netgraphics.add_update (UpdateSteammon(u));	  
	    hp := new_hp) in (*change hp*)
  List.iter pDamage lst;
  let checkFrozen m = List.mem Frozen !(get_status g m) in
  let lst2 = List.filter checkFrozen (!(g.red_mons)@(!(g.blue_mons))) in
  let qThaw m = 
    if (Random.int 100)>=cDEFROST_CHANCE then ()
    else (
	  Netgraphics.add_update (Message(m^" defrosted!"));
      let stat = get_status g m in
      stat := List.filter (fun x -> not(x=Frozen)) !stat;
      Netgraphics.add_update (SetStatusEffects(m,!stat))) in
  List.iter qThaw lst2 