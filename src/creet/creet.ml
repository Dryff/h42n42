open Js_of_ocaml
module Html = Dom_html

(* status status type *)
type health_status = Healthy | Contaminated | Berserker | Mean

type creet = {
  mutable x: float; (* X position *)
  mutable y: float; (* Y position *)
  mutable dx: float; (* X velocity - will be non-zero or dy will be non-zero, but not both *)
  mutable dy: float; (* Y velocity - will be non-zero or dx will be non-zero, but not both *)
  mutable status: health_status;
  mutable speed_factor: float; (* Speed factor for the creet *)
  mutable is_dragged: bool;
  mutable size_factor: float;
  mutable last_direction_change: float; (* Per-creet direction change timer *)
  image: Html.imageElement Js.t (* Image element *)
}

(* Create a new creet - default to healthy status *)
let create_creet img_src x y dx dy =
  let img = Html.createImg Html.document in
  img##.src := Js.string img_src;
  { 
    x; y; dx; dy; 
    status = Healthy; 
    image = img; 
    speed_factor = 1.0; 
    is_dragged = false; 
    size_factor = 1.0; 
    last_direction_change = Js.to_float (Js.date##now) /. 1000.
  }

(* Update creet's image based on status status *)
let update_creet_image creet =
  match creet.status with
  | Healthy -> creet.image##.src := Js.string "HealthyCreet.png"
  | Contaminated -> creet.image##.src := Js.string "ContaminatedCreet.png"
  | Berserker -> creet.image##.src := Js.string "BerserkerCreet.png"
  | Mean -> creet.image##.src := Js.string "MeanCreet.png"

(* Global speed factor *)
let global_speed = ref 1.0

(* Global hitbox size for creets and static elements *)
let creet_hitbox_size = 65.

(* Change creet status status and update associated properties *)
let change_status creet new_status =
  let final_status = 
    if new_status = Contaminated then
      let roll = Random.int 100 in
      if roll < 10 then Berserker
      else if roll < 20 then Mean
      else Contaminated
    else new_status
  in
  creet.status <- final_status;
  (* Adjust speed factor based on status status *)
  creet.speed_factor <- (match final_status with
    | Healthy -> 1.0             
    | Mean -> 1.2 (* Mean creets are faster to catch healthy ones *)
    | _ -> 0.85 
  );
  (* Adjust size_factor: Mean creets are 15% smaller *)
  creet.size_factor <- (match final_status with
    | Mean -> 0.85
    | _ -> creet.size_factor
  );
  if new_status = Healthy then
    creet.size_factor <- 1.0;
  update_creet_image creet

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

(* Update creet position, handle bouncing, and check for river/hospital contact *)
let update_creet creet canvas_width canvas_height dt all_creets =
  (* Check if it's time to change direction *)
  let current_time = Js.to_float (Js.date##now) /. 1000. in
  
  (* Special behavior for Mean creets - chase healthy creets *)
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
          creet.last_direction_change <- current_time;
        end
    | None ->
        (* If no healthy creet found, behave like normal *)
        if creet.dx = 0. && creet.dy = 0. then begin
          assign_new_direction creet
        end else if current_time -. creet.last_direction_change > (Random.float 3.) +. 0.5 then begin
          assign_new_direction creet
        end
  end else begin
    (* Normal behavior for other creets *)
    (* If creet was just released from being dragged, immediately give it a new direction *)
    if not creet.is_dragged && creet.dx = 0. && creet.dy = 0. then begin
      assign_new_direction creet
    end
    (* Otherwise, check if it's time for a regular direction change *)
    else if not creet.is_dragged && current_time -. creet.last_direction_change > (Random.float 3.) +. 0.5 then begin
      assign_new_direction creet
    end
  end;
  
  (* Update position based on velocity, applying the global speed factor *)
  if creet.is_dragged then begin
    (* If dragged, set dx and dy to zero *)
    creet.dx <- 0.;
    creet.dy <- 0.;
  end else begin
    (* Update position based on velocity *)
    creet.x <- creet.x +. creet.dx *. dt *. 10. *. !global_speed *. creet.speed_factor;
    creet.y <- creet.y +. creet.dy *. dt *. 10. *. !global_speed *. creet.speed_factor;
  end;
  
  if creet.status = Berserker then begin
    (* Gradually increase size up to 4x over time *)
    let max_size = 4.0 in
    let growth_rate = 0.1 *. dt in (* Adjust growth rate as needed *)
    creet.size_factor <- min max_size (creet.size_factor +. growth_rate);
  end;

  (* Check if creet is touching the river (top area) and change status if needed *)
  let river_height = 20. in
  if creet.y -. creet_hitbox_size <= river_height && creet.status = Healthy && not creet.is_dragged then begin
    change_status creet Contaminated;
  end;    
  
  (* Hospital is at the bottom, check for collision with any hospital *)
  let hospital_y = float_of_int canvas_height -. 20. in
  if creet.y +. creet_hitbox_size >= hospital_y && creet.status <> Healthy && creet.is_dragged then begin
    change_status creet Healthy;
  end;  

  (* Bounce off left and right borders *)
  let half_hitbox = creet_hitbox_size /. 2. in
  if creet.x < half_hitbox || creet.x > (float_of_int canvas_width) -. half_hitbox then begin
    creet.dx <- -.creet.dx;
    creet.x <- max half_hitbox (min ((float_of_int canvas_width) -. half_hitbox) creet.x);
  end;
  
  (* Bounce off top and bottom borders, accounting for image height *)
  if creet.y < half_hitbox || creet.y > (float_of_int canvas_height) -. half_hitbox then begin
    creet.dy <- -.creet.dy;
    creet.y <- max half_hitbox (min ((float_of_int canvas_height) -. half_hitbox) creet.y);
  end

(* Check for collisions between creets *)
let check_collisions creet all_creets =
  (* Only process contamination from contaminated to healthy *)
  if creet.status = Healthy then
    List.iter (fun other ->
      (* Skip self-comparison *)
      if creet != other then
        let dx = creet.x -. other.x in
        let dy = creet.y -. other.y in
        let distance = sqrt(dx *. dx +. dy *. dy) in
        let collision_radius = creet_hitbox_size *. creet.size_factor in
        
        (* If touching a contaminated creet, apply 2% chance of contamination *)
        if other.status <> Healthy && distance < collision_radius 
           && not creet.is_dragged && Random.int 100 < 2 then begin
          change_status creet Contaminated;
        end;
    ) all_creets
