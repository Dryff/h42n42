open Js_of_ocaml
open Creet
module Html = Dom_html

(* Spell circle effect type *)
type spell_circle = {
  x: float;        (* Center X position *)
  y: float;        (* Center Y position *)
  radius: float;   (* Circle radius *)
  duration: float; (* Duration remaining in seconds *)
}

(* Global game state variables *)
let game_over = ref false
let creets = ref []
let dragging = ref None
let offset_x = ref 0.
let offset_y = ref 0.
let last_time = ref 0.
let last_spawn = ref 0.
let last_speed_increase = ref 0.
let start_time = ref 0.
let elapsed_time = ref 0.
let spell_cooldown = ref 0.  (* Add spell cooldown timer *)
let spell_active = ref false  (* Is spell currently active *)
let spell_circle = ref { x = 0.; y = 0.; radius = 0.; duration = 0.; } (* Spell circle effect *)
let is_paused = ref false (* Track if the game is paused *)

(* Game configuration *)
let canvas_width = 800
let canvas_height = 600
let spawn_interval_low = 2
let spawn_interval_high = 6
let spawn_interval = ref (float_of_int spawn_interval_low +. Random.float (float_of_int spawn_interval_high -. float_of_int spawn_interval_low)) (* Between 2-6 seconds *)
let speed_increase_interval = 0.2
let speed_increase_factor = 0.01

(* Hospital configuration *)
let hospital_width = 170.
let hospital_height = 170.
let hospital_spacing = 200.
let initial_hospital_x = 10.
let num_hospitals = 4

(* Reset the game to initial state *)
let reset_game () =
  (* Reset all game state *)
  game_over := false;
  creets := [];
  dragging := None;
  offset_x := 0.;
  offset_y := 0.;
  Creet.global_speed := 1.0;
  
  (* Reset timers *)
  let current_time = Js.to_float (Js.Unsafe.js_expr "new Date().getTime()") /. 1000. in
  last_time := current_time;
  last_spawn := current_time;
  last_speed_increase := current_time;
  start_time := current_time;
  elapsed_time := 0.;
  
  (* Create initial creet *)
  let creet1 = Entity.create_axis_aligned_creet "HealthyCreet.png" 300. 300. in
  creets := [creet1]

(* Spawn a new creet *)
let spawn_creet () =
  (* Only spawn if the game is not paused *)
  if not !is_paused then begin
    let new_creet = Entity.create_random_healthy_creet canvas_width canvas_height in
    creets := new_creet :: !creets
  end

(* Check if all creets are unhealthy (game over condition) *)
let check_all_creets_health () =
  if Entity.all_creets_unhealthy !creets then
    game_over := true
    
(* Handle toggle pause button click *)
let toggle_pause () =
  is_paused := not !is_paused;
  Ui.update_pause_button_state !is_paused;
  
  if !is_paused then
    Printf.printf "Game paused by user\n"
  else
    Printf.printf "Game resumed by user\n"

(* Cast healing spell function *)
let cast_healing_spell () =
  if !spell_cooldown <= 0. && not !game_over then begin
    (* Create spell circle effect in the middle of the canvas *)
    let center_x = float_of_int canvas_width /. 2.0 in
    let center_y = float_of_int canvas_height /. 2.0 in
    let offset_range = 150.0 in (* Maximum offset from center *)
    
    (* Generate random position within the middle area *)
    let random_x = center_x +. (Random.float (2.0 *. offset_range) -. offset_range) in
    let random_y = center_y +. (Random.float (2.0 *. offset_range) -. offset_range) in
    let circle_radius = 200. in
    
    spell_circle := { 
      x = random_x; 
      y = random_y; 
      radius = circle_radius;
      duration = 2.0; (* Show for 2 seconds *)
    };
    
    (* Check which creets are inside the circle and convert only those *)
    creets := List.map (fun creet -> 
      (* Calculate distance from creet to circle center *)
      let creet_val : Creet.creet = creet in
  
  (* Calculate distance from creet to circle center *)
  let dx = creet_val.x -. !spell_circle.x in
  let dy = creet_val.y -. !spell_circle.y in
  let distance = sqrt(dx *. dx +. dy *. dy) in
      
      (* If creet is inside the circle and is unhealthy, heal it *)
      if distance <= !spell_circle.radius && 
        (creet_val.status = Creet.Mean || creet_val.status = Creet.Contaminated || creet_val.status = Creet.Berserker) then begin
        let healed_creet = creet in
        Creet.change_status healed_creet Creet.Healthy;
        healed_creet
      end else
        creet
    ) !creets;
    
    (* Set cooldown to 5 seconds *)
    spell_cooldown := 5.;
    
    Printf.printf "Healing spell cast! Cooldown: 5 seconds\n";
  end

(* Update spell cooldown *)
let update_spell_cooldown dt =
  if !spell_cooldown > 0. then begin
    spell_cooldown := !spell_cooldown -. dt;
    
    (* Update button appearance based on cooldown *)
    match !Ui.spell_button with
    | Some btn -> 
        let cooldown_text = if !spell_cooldown > 0. then
                             Printf.sprintf "Spell (%d)" (int_of_float (ceil !spell_cooldown))
                           else
                             "Cast Spell" 
        in
        Js.Unsafe.set btn "textContent" (Js.some (Js.string cooldown_text));
        
        (* Disable button during cooldown *)
        if !spell_cooldown > 0. then begin
          Js.Unsafe.set btn "disabled" Js._true;
          let style = Js.Unsafe.get btn "style" in
          Js.Unsafe.set style "backgroundColor" (Js.string "#999999");
        end else begin
          Js.Unsafe.set btn "disabled" Js._false;
          let style = Js.Unsafe.get btn "style" in
          Js.Unsafe.set style "backgroundColor" (Js.string "#7B68EE");
        end
    | None -> ()
  end;
  
  (* Update spell circle effect duration *)
  if !spell_circle.duration > 0. then
    spell_circle := { !spell_circle with duration = !spell_circle.duration -. dt }
