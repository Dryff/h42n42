open Js_of_ocaml
open Lwt.Syntax

(* Movement state for each creet *)
type movement_state = {
  mutable is_running: bool;
  mutable last_update: float;
}

(* Store movement threads for each creet *)
let movement_threads = ref []
let movement_states = Hashtbl.create 32

let sleep duration =
  let promise, resolver = Lwt.wait () in
  let callback = Js.wrap_callback (fun () -> Lwt.wakeup resolver ()) in
  ignore (Dom_html.window##setTimeout callback (Js.number_of_float (duration *. 1000.0)));
  promise

(* Individual creet movement thread *)
let rec creet_movement_thread creet canvas_width canvas_height all_creets =
  let state = try 
    Hashtbl.find movement_states creet 
  with Not_found -> 
    let current_time = Js.to_float (Js.date##now) /. 1000. in
    let new_state = { is_running = true; last_update = current_time } in
    Hashtbl.add movement_states creet new_state;
    new_state
  in
  
  if state.is_running && not !Gamestate.game_over && not !Gamestate.is_paused then begin
    (* Use FIXED 30 FPS timestep - no more variable dt *)
    let fixed_dt = 0.01 in 
    
    (* Always update with consistent timestep *)
    Creet.update_creet creet canvas_width canvas_height fixed_dt !all_creets;
    
  end;

  (* Sleep for exactly 33.33ms (30 FPS) *)
  let* () = sleep 0.01 in
  
  (* Continue the thread if still running *)
  if state.is_running then
    creet_movement_thread creet canvas_width canvas_height all_creets
  else
    Lwt.return ()

(* Start movement thread for a new creet *)
let start_creet_movement creet canvas_width canvas_height all_creets =
  let thread = creet_movement_thread creet canvas_width canvas_height all_creets in
  movement_threads := thread :: !movement_threads;
  Lwt.async (fun () -> thread)

(* Stop movement thread for a creet *)
let stop_creet_movement creet =
  try
    let state = Hashtbl.find movement_states creet in
    state.is_running <- false;
    Hashtbl.remove movement_states creet
  with Not_found -> ()

(* Stop all movement threads *)
let stop_all_movements () =
  Hashtbl.iter (fun _ state -> state.is_running <- false) movement_states;
  Hashtbl.clear movement_states;
  movement_threads := []

(* Start movements for all existing creets *)
let start_all_movements canvas_width canvas_height all_creets =
  List.iter (fun creet ->
    start_creet_movement creet canvas_width canvas_height all_creets
  ) !all_creets

(* Spawn a mean creet *)
let spawn_mean_creet () =
  if not !Gamestate.is_paused && not !Gamestate.game_over then begin
    let new_creet = Entity.create_random_healthy_creet Gamestate.canvas_width Gamestate.canvas_height in
    Creet.change_status new_creet Creet.Mean;
    Gamestate.creets := new_creet :: !Gamestate.creets;
    start_creet_movement new_creet Gamestate.canvas_width Gamestate.canvas_height Gamestate.creets;
    Printf.printf "Mean creet spawned\n"
  end

(* Spawn a berserker creet *)
let spawn_berserker_creet () =
  if not !Gamestate.is_paused && not !Gamestate.game_over then begin
    let new_creet = Entity.create_random_healthy_creet Gamestate.canvas_width Gamestate.canvas_height in
    Creet.change_status new_creet Creet.Berserker;
    Gamestate.creets := new_creet :: !Gamestate.creets;
    start_creet_movement new_creet Gamestate.canvas_width Gamestate.canvas_height Gamestate.creets;
    Printf.printf "Berserker creet spawned\n"
  end

(* Spawn a healthy creet *)
let spawn_healthy_creet () =
  if not !Gamestate.is_paused && not !Gamestate.game_over then begin
    let new_creet = Entity.create_random_healthy_creet Gamestate.canvas_width Gamestate.canvas_height in
    Gamestate.creets := new_creet :: !Gamestate.creets;
    start_creet_movement new_creet Gamestate.canvas_width Gamestate.canvas_height Gamestate.creets;
    Printf.printf "Healthy creet spawned\n"
  end

(* Spawn a contaminated creet *)
let spawn_contaminated_creet () =
  if not !Gamestate.is_paused && not !Gamestate.game_over then begin
    let new_creet = Entity.create_random_healthy_creet Gamestate.canvas_width Gamestate.canvas_height in
    Creet.change_status new_creet Creet.Contaminated;
    Gamestate.creets := new_creet :: !Gamestate.creets;
    start_creet_movement new_creet Gamestate.canvas_width Gamestate.canvas_height Gamestate.creets;
    Printf.printf "Contaminated creet spawned\n"
  end