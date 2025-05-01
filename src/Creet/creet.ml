open Js_of_ocaml

(* Global game speed control *)
let global_speed = ref 1.0

(* Creet types and properties for the game *)
type creet_type = Patient | Doctor | Virus

type creet = {
  id: int;
  mutable creet_type: creet_type;
  mutable x: float;
  mutable y: float;
  mutable vx: float;
  mutable vy: float;
  mutable width: float;
  mutable height: float;
  mutable health: float;
  mutable target_x: float option;
  mutable target_y: float option;
  mutable being_dragged: bool;
  mutable infected: bool;
  mutable infection_time: float;
  mutable recovery_time: float;
  mutable alive: bool;
  mutable rotation: float;
  (* For debug/display purposes *)
  mutable show_path: bool;
}

(* Update a creet's position and behavior *)
let update_creet creet canvas_width canvas_height dt all_creets =
  if not creet.being_dragged && creet.alive then begin
    (* Apply global speed to movement *)
    let speed_adjusted_dt = dt *. !global_speed in
    
    (* Movement based on current velocity *)
    creet.x <- creet.x +. creet.vx *. speed_adjusted_dt;
    creet.y <- creet.y +. creet.vy *. speed_adjusted_dt;
    
    (* Rest of the update logic *)
    (* ...existing code... *)
  end