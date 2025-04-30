open Js_of_ocaml
open Creet
module Html = Dom_html

(* Constants for physics calculations *)
let river_height = 30.
let hospital_height = 30.
let creet_collision_radius = 50.

(* Assign a new random direction to a creet *)
let assign_new_direction creet =
  let base_speed = 10. in
  if Random.bool () then begin
    creet.dx <- (if Random.bool () then base_speed else -.base_speed);
    creet.dy <- 0.
  end else begin
    creet.dx <- 0.;
    creet.dy <- (if Random.bool () then base_speed else -.base_speed)
  end;
  creet.last_direction_change <- Js.to_float (Js.date##now) /. 1000.

(* Find the nearest healthy creet *)
let find_nearest_healthy_creet creet all_creets =
  let nearest_creet = ref None in
  let min_distance = ref max_float in
  
  List.iter (fun other ->
    if other != creet && other.status = Healthy && not other.is_dragged then
      let dx = creet.x -. other.x in
      let dy = creet.y -. other.y in
      let distance = sqrt(dx *. dx +. dy *. dy) in
      if distance < !min_distance then begin
        min_distance := distance;
        nearest_creet := Some other;
      end
  ) all_creets;
  
  !nearest_creet

(* Check for river (top of screen) contact *)
let check_river_contact creet =
  creet.y -. Creet.creet_hitbox_size <= river_height && 
  creet.status = Healthy && 
  not creet.is_dragged

(* Calculate distance between two creets *)
let distance_between creet1 creet2 =
  let dx = creet1.x -. creet2.x in
  let dy = creet1.y -. creet2.y in
  sqrt(dx *. dx +. dy *. dy)

(* Check if two creets are colliding *)
let are_colliding creet1 creet2 =
  if creet1 != creet2 then
    let collision_distance = Creet.creet_hitbox_size *. (creet1.size_factor +. creet2.size_factor) /. 2.0 in
    distance_between creet1 creet2 < collision_distance
  else
    false

(* Handle boundary collisions and keep creet inside canvas *)
let handle_boundary_collisions creet canvas_width canvas_height =
  let hitbox = Creet.creet_hitbox_size in
  (* Bounce off left and right borders *)
  if creet.x < hitbox /. 2. || creet.x > (float_of_int canvas_width) -. (hitbox /. 2.) then begin
    creet.dx <- -.creet.dx;
    creet.x <- max (hitbox /. 2.) (min ((float_of_int canvas_width) -. (hitbox /. 2.)) creet.x);
  end;
  (* Bounce off top and bottom borders *)
  if creet.y < hitbox /. 2. || creet.y > (float_of_int canvas_height) -. (hitbox /. 2.) then begin
    creet.dy <- -.creet.dy;
    creet.y <- max (hitbox /. 2.) (min ((float_of_int canvas_height) -. (hitbox /. 2.)) creet.y);
  end

(* Update the size of a berserker creet *)
let update_berserker_size creet dt =
  if creet.status = Berserker then begin
    (* Gradually increase size up to 4x over time *)
    let max_size = 4.0 in
    let growth_rate = 0.1 *. dt in (* Adjust growth rate as needed *)
    creet.size_factor <- min max_size (creet.size_factor +. growth_rate);
  end

(* Update creet position based on velocity *)
let update_position creet dt =
  if not creet.is_dragged then begin
    creet.x <- creet.x +. creet.dx *. dt *. 10. *. !global_speed *. creet.speed_factor;
    creet.y <- creet.y +. creet.dy *. dt *. 10. *. !global_speed *. creet.speed_factor;
  end

(* Handle Mean creet hunting behavior *)
let handle_mean_creet_hunting creet all_creets =
  if creet.status = Mean && not creet.is_dragged then begin
    match find_nearest_healthy_creet creet all_creets with
    | Some target_creet ->
        let dx = target_creet.x -. creet.x in
        let dy = target_creet.y -. creet.y in
        let distance = sqrt(dx *. dx +. dy *. dy) in
        
        (* Only change direction if target is somewhat far away *)
        if distance > 20.0 || (creet.dx = 0. && creet.dy = 0.) then begin
          (* Focus on either horizontal or vertical movement based on which is larger *)
          if abs_float dx > abs_float dy then begin
            creet.dx <- (if dx > 0. then 10. else -10.);
            creet.dy <- 0.;
          end else begin
            creet.dx <- 0.;
            creet.dy <- (if dy > 0. then 10. else -10.);
          end;
          
          (* Reset direction change timer *)
          creet.last_direction_change <- Js.to_float (Js.date##now) /. 1000.;
        end
    | None ->
        (* If no healthy creet found, behave like normal *)
        if creet.dx = 0. && creet.dy = 0. then begin
          assign_new_direction creet
        end
  end

(* Update creet direction periodically *)
let update_direction creet =
  let current_time = Js.to_float (Js.date##now) /. 1000. in
  
  (* Normal behavior for non-Mean creets or when not being dragged *)
  if creet.status <> Mean && not creet.is_dragged then begin
    (* If creet was just released from being dragged, immediately give it a new direction *)
    if creet.dx = 0. && creet.dy = 0. then begin
      assign_new_direction creet
    end
    (* Otherwise, check if it's time for a regular direction change *)
    else if current_time -. creet.last_direction_change > (Random.float 3.) +. 0.5 then begin
      assign_new_direction creet
    end
  end

(* Update creet physics *)
let update_creet_physics creet canvas_width canvas_height dt all_creets =
  (* Update direction for mean creets (hunting behavior) *)
  handle_mean_creet_hunting creet all_creets;
  
  (* Update direction for normal creets *)
  update_direction creet;
  
  (* Update berserker size *)
  update_berserker_size creet dt;
  
  (* Update position based on velocity *)
  update_position creet dt;
  
  (* Handle boundary collisions *)
  handle_boundary_collisions creet canvas_width canvas_height