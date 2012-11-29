open Definitions
open Util
open State

let create_attack (str:string) : attack =
  let lst = wordify str in
  match lst with
  nm::el::pp::pow::acc::crit::effn::effc::[] ->
    {name = nm;
     element = type_of_string el;
     max_pp = int_of_string pp;
     pp_remaining = int_of_string pp;
     power = int_of_string pow;
     accuracy = int_of_string acc;
     crit_chance = int_of_string crit;
     effect = (effect_of_num (int_of_string effn), int_of_string effc)}
  | _ -> raise (Runtime "Bad attack initialization text")
  
let create_mon (str:string) (attks: attack Table.t) : steammon  =
  let lst = wordify str in
  match lst with
  nm::hp::t1::t2::a1::a2::a3::a4::a::spa::d::spd::sp::[] ->
    let tpe (a : string) = match a with
	    "Nothing" -> None
		| _ -> Some (type_of_string a) in
   (try (
     {species = nm;
      curr_hp = int_of_string hp;
      max_hp = int_of_string hp;
      first_type = tpe t1;
      second_type = tpe t2;
      first_attack = Table.find attks a1 ;
      second_attack = Table.find attks a2;
      third_attack = Table.find attks a3;
      fourth_attack = Table.find attks a4;
      attack = int_of_string a;
      spl_attack = int_of_string spa;
      defense = int_of_string d;
      spl_defense = int_of_string spd;
      speed = int_of_string sp;
      status = [];
      mods = {attack_mod=0; speed_mod=0; defense_mod=0; accuracy_mod=0}} )
	 with Not_found -> raise(Runtime "Steammon contains non-existent attack")
	)
  | _ -> raise (Runtime "Bad Steammon initialization text")

let choose_c () = 
  if (Random.float 1.) <= 0.5 then Red else Blue

let create_game () =
    {
      red_mons = ref [];
      blue_mons = ref [];
      mons = ref [];
      red_items = ref (Array.create 8 0);
      blue_items = ref (Array.create 8 0);
    }

let datafy_attk (atk : attk) : attack =
  {name = atk.sname;
   element = atk.selement;
   max_pp = atk.smax_pp;
   pp_remaining = !(atk.spp_remaining);
   power = atk.spower;
   accuracy = atk.saccuracy;
   crit_chance = atk.scrit_chance;
   effect = atk.seffect}
	
let datafy_mon (mon : smon) : steammon  =
  let a = match mon.sattks with
    b::c::d::e::[] -> (datafy_attk (snd b), datafy_attk (snd c), 
	                    datafy_attk (snd d), datafy_attk (snd e)) 
    | _ -> raise (Runtime "game data conversion error: bad smon attack list")
  in
  let (b,c,d,e) = a in
  {species = mon.sspecies;
   curr_hp = !(mon.scurr_hp);
   max_hp = mon.smax_hp;
   first_type = mon.sfirst_type;
   second_type = mon.ssecond_type;
   first_attack = b;
   second_attack = c;
   third_attack = d;
   fourth_attack = e;
   attack = mon.sattack;
     spl_attack = mon.sspl_attack;
   defense = mon.sdefense;
     spl_defense = mon.sspl_defense;
   speed = mon.sspeed;
   status = !(mon.sstatus);
   mods = !(mon.smods)}  

let atk_from_data (atk : attack) : attk =
  {sname = atk.name;
   selement = atk.element;
   smax_pp = atk.max_pp;
   spp_remaining = ref atk.pp_remaining;
   spower = atk.power;
   saccuracy = atk.accuracy;
   scrit_chance = atk.crit_chance;
   seffect = atk.effect}

let mon_from_data (mon : steammon) : smon =
  let attks = [(mon.first_attack.name, atk_from_data (mon.first_attack));
               (mon.second_attack.name, atk_from_data (mon.second_attack));
			   (mon.third_attack.name, atk_from_data (mon.third_attack));
			   (mon.fourth_attack.name, atk_from_data (mon.fourth_attack))] in
  {sspecies = mon.species;
   scurr_hp = ref mon.curr_hp;
   smax_hp = mon.max_hp;
   sfirst_type = mon.first_type;
   ssecond_type = mon.second_type;
   sattks = attks;
   sattack = mon.attack;
   sspl_attack = mon.spl_attack;
   sdefense = mon.defense;
   sspl_defense = mon.spl_defense;
   sspeed = mon.speed;
   sstatus = ref mon.status;
   smods = ref mon.mods}
   