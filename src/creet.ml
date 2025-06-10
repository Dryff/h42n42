open Js_of_ocaml
module Html = Dom_html

(* status status type *)
type health_status = Healthy | Contaminated | Berserker | Mean

type creet = {
  mutable x: float; 
  mutable y: float; 
  mutable dx: float; 
  mutable dy: float; 
  mutable status: health_status;
  mutable speed_factor: float; 
  mutable is_dragged: bool;
  mutable size_factor: float;
  mutable last_direction_change: float; (* Per-creet direction change timer *)
  image: Html.imageElement Js.t;
  mutable dom_element: Html.divElement Js.t option;
}

(* Create a new healthy creet *)
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
    last_direction_change = Js.to_float (Js.date##now) /. 1000.;
    dom_element = None;
  }

(* Update creet's image based on status status *)
let update_creet_image creet =
  match creet.status with
  | Healthy -> creet.image##.src := Js.string "HealthyCreet.png"
  | Contaminated -> creet.image##.src := Js.string "ContaminatedCreet.png"
  | Berserker -> creet.image##.src := Js.string "BerserkerCreet.png"
  | Mean -> creet.image##.src := Js.string "MeanCreet.png"


let global_speed = ref 1.0
let creet_hitbox_size = 65.
let mean_spawn_rate = 10
let berserker_spawn_rate = 10

let change_status creet new_status =
  let final_status = 
    if new_status = Contaminated then
      let roll = Random.int 100 in
      if roll < berserker_spawn_rate then Berserker
      else if roll < berserker_spawn_rate + mean_spawn_rate then Mean
      else Contaminated
    else new_status
  in
  creet.status <- final_status;
  creet.speed_factor <- (match final_status with
    | Healthy -> 1.0
    | Mean -> 1.1
    | _ -> 0.85 
  );
  creet.size_factor <- (match final_status with
    | Mean -> 0.85
    | _ -> creet.size_factor
  );
  if new_status = Healthy then
    creet.size_factor <- 1.0;
  update_creet_image creet

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
  let current_time = Js.to_float (Js.date##now) /. 1000. in
  
  (* Chasing healthy creets *)
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
    (* If creet was just released from being dragged, immediately give it a new direction *)
    if not creet.is_dragged && creet.dx = 0. && creet.dy = 0. then begin
      assign_new_direction creet
    end
    (* Otherwise, check if it's time for a regular direction change *)
    else if not creet.is_dragged && current_time -. creet.last_direction_change > (Random.float 3.) +. 0.5 then begin
      assign_new_direction creet
    end
  end;
  
  if creet.is_dragged then begin
    creet.dx <- 0.;
    creet.dy <- 0.;
  end else begin
      creet.x <- creet.x +. creet.dx *. dt *. 10. *. !global_speed *. creet.speed_factor;
    creet.y <- creet.y +. creet.dy *. dt *. 10. *. !global_speed *. creet.speed_factor;
  end;
  
  if creet.status = Berserker then begin
    (* Gradually increase size up to 4x over time *)
    let max_size = 4.0 in
    let growth_rate = 0.1 *. dt in
    creet.size_factor <- min max_size (creet.size_factor +. growth_rate);
  end;

  (* Check for collision with river *)
  let river_height = 20. in
  let scaled_hitbox = (creet_hitbox_size *. creet.size_factor) /. 2. in
  if creet.y -. scaled_hitbox <= river_height && creet.status = Healthy && not creet.is_dragged then begin
    change_status creet Contaminated;
  end;

  (* Check for collision with hospital *)
  let hospital_y = float_of_int canvas_height -. 20. in
  if creet.y +. scaled_hitbox >= hospital_y && creet.status <> Healthy && creet.is_dragged then begin
    change_status creet Healthy;
  end;  

  (* Bounce off left and right borders *)
  let half_hitbox = scaled_hitbox in
  if creet.x < half_hitbox || creet.x > (float_of_int canvas_width) -. half_hitbox then begin
    creet.dx <- -.creet.dx;
    creet.x <- max half_hitbox (min ((float_of_int canvas_width) -. half_hitbox) creet.x);
  end;
  
  (* Bounce off top and bottom borders, accounting for scaled size *)
  if creet.y < half_hitbox || creet.y > (float_of_int canvas_height) -. half_hitbox then begin
    creet.dy <- -.creet.dy;
    creet.y <- max half_hitbox (min ((float_of_int canvas_height) -. half_hitbox) creet.y);
  end

(* Quadtree for collision detection *)
type quad_tree = {
  bounds_x: float; bounds_y: float; bounds_w: float; bounds_h: float;
  stored_creets: creet list;
  sub_nodes: quad_tree list;
}

let make_quad_tree x y w h = {
  bounds_x = x; bounds_y = y; bounds_w = w; bounds_h = h;
  stored_creets = [];
  sub_nodes = [];
}

let is_point_inside tree px py =
  px >= tree.bounds_x && px < tree.bounds_x +. tree.bounds_w && 
  py >= tree.bounds_y && py < tree.bounds_y +. tree.bounds_h

let rec add_creet_to_tree tree creet_item =
  if not (is_point_inside tree creet_item.x creet_item.y) then 
    tree
  else if List.length tree.stored_creets < 4 && tree.sub_nodes = [] then
    { tree with stored_creets = creet_item :: tree.stored_creets }
  else
    let half_width = tree.bounds_w /. 2.0 in
    let half_height = tree.bounds_h /. 2.0 in
    let quadrants = if tree.sub_nodes = [] then [
      make_quad_tree tree.bounds_x tree.bounds_y half_width half_height;
      make_quad_tree (tree.bounds_x +. half_width) tree.bounds_y half_width half_height;
      make_quad_tree tree.bounds_x (tree.bounds_y +. half_height) half_width half_height;
      make_quad_tree (tree.bounds_x +. half_width) (tree.bounds_y +. half_height) half_width half_height;
    ] else tree.sub_nodes in
    let new_quadrants = List.map (fun quad -> add_creet_to_tree quad creet_item) quadrants in
    { tree with sub_nodes = new_quadrants; stored_creets = [] }

let rec find_nearby_creets tree center_x center_y search_radius =
  let circle_touches_rectangle rx ry rw rh cx cy r =
    let nearest_x = max rx (min cx (rx +. rw)) in
    let nearest_y = max ry (min cy (ry +. rh)) in
    let dist_x = cx -. nearest_x in
    let dist_y = cy -. nearest_y in
    (dist_x *. dist_x +. dist_y *. dist_y) <= (r *. r)
  in
  
  if not (circle_touches_rectangle tree.bounds_x tree.bounds_y tree.bounds_w tree.bounds_h center_x center_y search_radius) then 
    []
  else
    let local_matches = List.filter (fun creet_obj ->
      let dx = creet_obj.x -. center_x in
      let dy = creet_obj.y -. center_y in
      (dx *. dx +. dy *. dy) <= (search_radius *. search_radius)
    ) tree.stored_creets in
    
    let child_matches = List.fold_left (fun acc child_node ->
      (find_nearby_creets child_node center_x center_y search_radius) @ acc
    ) [] tree.sub_nodes in
    
    local_matches @ child_matches

(* Check for collisions between creets using spatial tree *)
let check_collisions creet all_creets =
  if creet.status = Healthy && not creet.is_dragged then
    let collision_radius = creet_hitbox_size *. creet.size_factor in
    
    (* Build spatial tree with contaminated creets *)
    let contaminated_creets = List.filter (fun c -> c.status <> Healthy) all_creets in
    let quad_tree = List.fold_left (fun acc_tree c -> add_creet_to_tree acc_tree c) 
                      (make_quad_tree 0.0 0.0 800.0 600.0) contaminated_creets in
    
    (* Query nearby contaminated creets *)
    let nearby_creets = find_nearby_creets quad_tree creet.x creet.y collision_radius in
    
    let has_contamination = List.exists (fun other ->
      creet != other &&
      let creet_half_size = (creet_hitbox_size *. creet.size_factor) /. 2. in
      let other_half_size = (creet_hitbox_size *. other.size_factor) /. 2. in
      (* Check if rectangles overlap *)
      abs_float (creet.x -. other.x) < (creet_half_size +. other_half_size) &&
      abs_float (creet.y -. other.y) < (creet_half_size +. other_half_size) &&
      Random.int 100 < 2
    ) nearby_creets in
    
    if has_contamination then change_status creet Contaminated
