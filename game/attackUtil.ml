open Definitions
open Constants
open Util
open State

exception GameOver of color

exception Fainted of (color * string)

type md = At | D | S | Ac

let get_mult (stat : md) (i : int) : float =
  match stat with
  | At when i=1 -> cATTACK_UP1
  | At when i=2 -> cATTACK_UP2
  | At when i>=3 -> cATTACK_UP3
  | At when i=(-1) -> cATTACK_DOWN1
  | At when i=(-2) -> cATTACK_DOWN2
  | At when i<=(-3) -> cATTACK_DOWN3
  | D when i=1 -> cDEFENSE_UP1
  | D when i=2 -> cDEFENSE_UP2
  | D when i>=3 -> cDEFENSE_UP3
  | D when i=(-1) -> cATTACK_DOWN1
  | D when i=(-2) -> cATTACK_DOWN2
  | D when i<=(-3) -> cATTACK_DOWN3
  | S when i=1 -> cSPEED_UP1
  | S when i=2 -> cSPEED_UP2
  | S when i>=3 -> cSPEED_UP3
  | S when i=(-1) -> cATTACK_DOWN1
  | S when i=(-2) -> cATTACK_DOWN2
  | S when i<=(-3) -> cATTACK_DOWN3
  | Ac when i=1 -> cACCURACY_UP1
  | Ac when i=2 -> cACCURACY_UP2
  | Ac when i>=3 -> cACCURACY_UP3
  | Ac when i=(-1) -> cATTACK_DOWN1
  | Ac when i=(-2) -> cATTACK_DOWN2
  | Ac when i<=(-3) -> cATTACK_DOWN3
  | _ -> 1.
  
let changeMod (g:state) (m:string) (n : md) (i : int) =
  let mods = (get_modifiers g m) in
  match n with
  |At -> 
    mods := {attack_mod = max (-3) (min 3 ((!mods).attack_mod + i));
	         speed_mod = (!mods).speed_mod;
	         defense_mod = (!mods).defense_mod;
	         accuracy_mod = (!mods).accuracy_mod}
  |D -> 
	mods := {attack_mod = (!mods).attack_mod;
	         speed_mod = (!mods).speed_mod;
		     defense_mod = max (-3) (min 3 ((!mods).defense_mod + i));
		     accuracy_mod = (!mods).accuracy_mod}
  |S ->
	mods := {attack_mod = (!mods).attack_mod;
             speed_mod = max (-3) (min 3 ((!mods).speed_mod + i));
	         defense_mod = (!mods).defense_mod;
	         accuracy_mod = (!mods).accuracy_mod}
  |Ac ->
	mods := {attack_mod = (!mods).attack_mod;
	         speed_mod = (!mods).speed_mod;
	         defense_mod = (!mods).defense_mod;
	         accuracy_mod = max (-3) (min 3 ((!mods).accuracy_mod + i))}

let calc_speed (g:state) (s:string) : float =
  let p= if List.mem Paralyzed !(get_status g s) then cPARALYSIS_SLOW else 1 in
  let m = get_mult S ((!(get_modifiers g s)).speed_mod) in
  (m *. (float_of_int (get_speed g s))) /. (float_of_int p)

let sprint g =
  try (
  let redS = calc_speed g (get_starter g Red) in
  let blueS = calc_speed g (get_starter g Blue) in
  if redS > blueS then 1
  else if redS < blueS then 2
  else (Random.int 2) + 1 )
  with Mon_not_found(s) -> (Random.int 2) + 1
  
let validAttack (g:state) (c:color) (s:string) : bool =
  let m = get_starter g c in
  (*attack exists for starter and has pp remaining*)
  ((List.mem s (get_attks g m)) && (!(get_pp g m s)>0))

let attackHappens (g:state) (c:color) (m:string) (attack:attk) : bool = 
  let stat = get_status g m in
  let confound = 
    if List.mem Confused !stat then (*confused*)
	  (Netgraphics.add_update (Message(m^" is confused!"));
      (if (Random.int 100)<cSNAP_OUT_OF_CONFUSION then (*snapped out*)
	    (Netgraphics.add_update (Message(m^" snapped out of confusion!"));
		stat := List.filter (fun x -> not(x=Confused)) !stat; 
		Netgraphics.add_update (SetStatusEffects(m,!stat)); false)
       else if (Random.int 100)>=cSELF_ATTACK_CHANCE then (*no self-attack*)
         false
       else ((*self-attack*)
	     Netgraphics.add_update (Message("It hurt itself in its confusion!"));
   	     let a = (float_of_int (get_attack g m)) *. (get_mult At ((!(get_modifiers g m)).attack_mod)) in
	     let d = (float_of_int (get_def g m)) *. (get_mult D ((!(get_modifiers g m)).defense_mod)) in
         let damage = int_of_float (((float_of_int cSELF_ATTACK_POWER) *. a) /. d) in
	     let hp = get_curr_hp g m in
	     let new_hp = !hp - damage in
	     if new_hp<=0 then raise (Fainted (c,m)) (*check faint*)
	     else (
		   Netgraphics.add_update (NegativeEffect(" ", c, damage));
	       let u = (m, new_hp, get_max_hp g m, c) in
	       Netgraphics.add_update (UpdateSteammon(u));
	       hp := new_hp; (*change hp*)
           true ) ) ) )
    else (*not confused*)
	  false in
  if confound then
    false
  else (
    match List.filter (fun x -> not(x=Confused)) !stat with
    |h::t::[] -> raise (Runtime "status invariants not satisfied")
    |Frozen::[] -> Netgraphics.add_update (Message(m^" is frozen solid!")); false
    |Asleep::[] -> 
	  Netgraphics.add_update (Message(m^" is asleep."));
      if (Random.int 100)<cWAKE_UP_CHANCE then
       (Netgraphics.add_update (Message(m^" woke up!"));
		stat := List.filter (fun x -> not(x=Asleep)) !stat;
        Netgraphics.add_update (SetStatusEffects(m,!stat));	true)
      else
        false 
    |Paralyzed::[] ->
      Netgraphics.add_update (Message(m^" is paralyzed and may be unable to move!"));
      if (Random.int 100) >= cPARALYSIS_CHANCE then true
	  else (Netgraphics.add_update (Message(m^" is paralyzed! It can't move!")); false)
    |_ -> true
   )
   
let calcCrit (attack:attk) : float =
  if (Random.int 100)<(attack.scrit_chance) then 
   (Netgraphics.add_update (Message("Critical hit!"));
	cCRIT_MULTIPLIER)
  else 
    1.

let calcSTAB (g:state) (m:string) (attack:attk) : float = 
  if List.mem (attack.selement) (get_types g m) then cSTAB_BONUS else 1.

let calcWeakness (g:state) (oppM:string) (attack:attk) : float =
  let eff =
    match get_types g oppM with
    x::[] -> weakness attack.selement x
    |h::t::[] -> (weakness attack.selement h) *. (weakness attack.selement t)
    |_ -> raise (Runtime "type invariants not satisfied") in
 (if eff<1. then 
    (Netgraphics.add_update (Message("It's not very effective...")))
  else if eff>1. then
    (Netgraphics.add_update (Message("It's super effective!")))
  else if eff=0. then
    (Netgraphics.add_update (Message("It doesn't effect "^oppM^"!")))
  else ());
  eff

let calcAttack (g:state) (m:string) (attack:attk) : float =
  let a = 
    match attack.selement with
    Electric | Fire | Water | Psychic | Ghost -> get_spl_attack g m
	|_ -> get_attack g m in
  let m = get_mult At ((!(get_modifiers g m)).attack_mod) in
  m *. (float_of_int a)

let calcDef (g:state) (oppM:string) (attack:attk) : float =
  let d = 
    match attack.selement with
    Electric | Fire | Water | Psychic | Ghost -> get_spl_def g oppM
	|_ -> get_def g oppM in
  let m = get_mult D ((!(get_modifiers g oppM)).defense_mod) in
  m *. (float_of_int d)

let calcEffect (g:state) (attack:attk) (m:string) (oppM:string) : unit =
  let os = get_status g oppM in
  let changeSt sl s =
   (sl :=
      match !sl with
	  [] -> [s]
      |x::[] when x=Confused && not(s=Confused) -> s::!sl
	  |x::[] when not(x=Confused) && s=Confused -> s::!sl
	  |_ -> !sl);
    Netgraphics.add_update (SetStatusEffects(oppM,!sl)) in
  match attack.seffect with
  |(Nada,_) -> ()
  |(k,s) ->
    if (Random.int 100)<s then (
	  match k with
      | Nada -> ()	  
	  | Poisons -> changeSt os Poisoned
      | Confuses -> changeSt os Confused
      | Sleeps -> changeSt os Asleep
      | Paralyzes -> changeSt os Paralyzed
      | Freezes -> changeSt os Frozen
      | SelfAttackUp1 -> changeMod g m At 1
      | SelfDefenseUp1 -> changeMod g m D 1
      | SelfSpeedUp1 -> changeMod g m S 1
      | SelfAccuracyUp1 -> changeMod g m Ac 1
      | OpponentAttackDown1 -> changeMod g oppM At (-1)
      | OpponentDefenseDown1 -> changeMod g oppM D (-1)
      | OpponentSpeedDown1 -> changeMod g oppM S (-1)
      | OpponentAccuracyDown1 -> changeMod g oppM Ac (-1) )
	else
	  ()
