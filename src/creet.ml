open Js_of_ocaml
open Tyxml.Html
module Html = Dom_html

(* status status type *)
type health_status = Healthy | Contaminated | Berserker | Mean | Dead
let time_to_die = 30.0 

(* Helper function to extract specific element type from TyXML *)
let extract_element doc tyxml_element selector =
  let html_string = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) tyxml_element in
  let temp_container = Html.createDiv doc in
  temp_container##.innerHTML := Js.string html_string;
  let element = Js.Opt.get 
    (temp_container##querySelector (Js.string selector))
    (fun () -> failwith ("Element not found: " ^ selector)) in
  Js.Unsafe.coerce element

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
  mutable contamination_timer: float; (* Timer for contamination effect *)
  image: Html.imageElement Js.t;
  mutable dom_element: Html.divElement Js.t option;
}

(* Create a new healthy creet using TyXML for validation *)
let create_creet img_src x y dx dy =
  (* Create image element using TyXML for static validation *)
  let img_tyxml = img 
    ~src:img_src 
    ~alt:"Creet sprite" 
    ~a:[
      a_style "display: block; max-width: 100%; height: auto;";
      a_class ["creet-image"]
    ] () in
  
  let img_elem = extract_element Html.document img_tyxml "img" in
  
  { 
    x; y; dx; dy; 
    status = Healthy; 
    image = img_elem; 
    speed_factor = 1.0; 
    is_dragged = false; 
    size_factor = 1.0; 
    last_direction_change = Js.to_float (Js.date##now) /. 1000.;
    dom_element = None;
    contamination_timer = 0.0;
  }

(* Update creet's image based on status using TyXML validation *)
let update_creet_image creet =
  (* Create new validated image elements for each status *)
  let new_img_tyxml = match creet.status with
  | Healthy -> img 
      ~src:"HealthyCreet.png" 
      ~alt:"Healthy Creet" 
      ~a:[a_class ["creet-image"; "healthy"]] ()
  | Contaminated -> img 
      ~src:"ContaminatedCreet.png" 
      ~alt:"Contaminated Creet" 
      ~a:[a_class ["creet-image"; "contaminated"]] ()
  | Berserker -> img 
      ~src:"BerserkerCreet.png" 
      ~alt:"Berserker Creet" 
      ~a:[a_class ["creet-image"; "berserker"]] ()
  | Mean -> img 
      ~src:"MeanCreet.png" 
      ~alt:"Mean Creet" 
      ~a:[a_class ["creet-image"; "mean"]] ()
  | Dead -> img 
      ~src:"DeadCreet.png" 
      ~alt:"Dead Creet" 
      ~a:[a_class ["creet-image"; "dead"]] ()
  in
  
  (* Extract the validated image element and update the existing one *)
  let temp_img = extract_element Html.document new_img_tyxml "img" in
  creet.image##.src := temp_img##.src;
  creet.image##.alt := temp_img##.alt;
  creet.image##.className := temp_img##.className

let global_speed = ref 1.0
let creet_hitbox_size = 65.
let mean_spawn_rate = 10
let berserker_spawn_rate = 10

let change_status creet new_status =
  let current_time = Js.to_float (Js.date##now) /. 1000. in
  (* Pick status *)
  let final_status = 
    if new_status = Contaminated then
      let roll = Random.int 100 in
      if roll < berserker_spawn_rate then Berserker
      else if roll < berserker_spawn_rate + mean_spawn_rate then Mean
      else Contaminated
    else new_status
  in
  creet.status <- final_status;

  (* Track contamination time *)
  if final_status = Contaminated || final_status = Berserker || final_status = Mean then
    creet.contamination_timer <- current_time
  else if final_status = Healthy then
    creet.contamination_timer <- 0.0;

  creet.speed_factor <- (match final_status with
    | Healthy -> 1.0
    | Mean -> 1.2
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
  let base_speed = 20. in
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

let should_creet_die creet current_time =
  (creet.status = Contaminated || creet.status = Berserker || creet.status = Mean) &&
  creet.contamination_timer > 0.0 &&
  (current_time -. creet.contamination_timer) >= time_to_die

(* Update creet position, handle bouncing, and check for river/hospital contact *)
let update_creet creet canvas_width canvas_height dt all_creets =
  let current_time = Js.to_float (Js.date##now) /. 1000. in

  if should_creet_die creet current_time then begin
    creet.status <- Dead;
  end;

  (* Handling mean creet behavior and regular creets direction changes *)
  if creet.status = Mean && not creet.is_dragged then begin
    match find_nearest_healthy_creet creet all_creets with
    | Some target_creet ->
        let dx = target_creet.x -. creet.x in
        let dy = target_creet.y -. creet.y in
        let distance = sqrt(dx *. dx +. dy *. dy) in
        
        (* Only change direction if target is far away *)
        if distance > 20.0 || (creet.dx = 0. && creet.dy = 0.) then begin
          (* Focus on either horizontal or vertical movement based on which is larger *)
          if abs_float dx > abs_float dy then begin
            creet.dx <- (if dx > 0. then 20. else -20.);
            creet.dy <- 0.;
          end else begin
            creet.dx <- 0.;
            creet.dy <- (if dy > 0. then 20. else -20.);
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
  
  (* Bounce off top and bottom borders *)
  if creet.y < half_hitbox || creet.y > (float_of_int canvas_height) -. half_hitbox then begin
    creet.dy <- -.creet.dy;
    creet.y <- max half_hitbox (min ((float_of_int canvas_height) -. half_hitbox) creet.y);
  end

  
(* Quadtree*)
type quad_tree = {
  bounds_x: float; bounds_y: float; bounds_w: float; bounds_h: float; 
  stored_creets: creet list;  
  sub_nodes: quad_tree list;  
}

(* Create empty quadtree covering given rectangle *)
let make_quad_tree x y w h = {
  bounds_x = x; bounds_y = y; bounds_w = w; bounds_h = h;
  stored_creets = [];
  sub_nodes = [];
}

(* Check if point is inside this quadtree's bounds *)
let is_point_inside tree px py =
  px >= tree.bounds_x && px < tree.bounds_x +. tree.bounds_w && 
  py >= tree.bounds_y && py < tree.bounds_y +. tree.bounds_h

(* Add creet to quadtree, subdividing if needed *)
let rec add_creet_to_tree tree creet_item =
  (* Skip if creet is outside bounds *)
  if not (is_point_inside tree creet_item.x creet_item.y) then 
    tree
  (* Store in current node if space available and no subdivisions *)
  else if List.length tree.stored_creets < 4 && tree.sub_nodes = [] then
    { tree with stored_creets = creet_item :: tree.stored_creets }
  (* Need to subdivide: create 4 child quadrants *)
  else
    let half_width = tree.bounds_w /. 2.0 in
    let half_height = tree.bounds_h /. 2.0 in
    (* Create 4 quadrants: top-left, top-right, bottom-left, bottom-right *)
    let quadrants = if tree.sub_nodes = [] then [
      make_quad_tree tree.bounds_x tree.bounds_y half_width half_height;                                    (* Top-left *)
      make_quad_tree (tree.bounds_x +. half_width) tree.bounds_y half_width half_height;                   (* Top-right *)
      make_quad_tree tree.bounds_x (tree.bounds_y +. half_height) half_width half_height;                  (* Bottom-left *)
      make_quad_tree (tree.bounds_x +. half_width) (tree.bounds_y +. half_height) half_width half_height;  (* Bottom-right *)
    ] else tree.sub_nodes in
    (* Add creet to appropriate child quadrant *)
    let new_quadrants = List.map (fun quad -> add_creet_to_tree quad creet_item) quadrants in
    { tree with sub_nodes = new_quadrants; stored_creets = [] }

(* Find all creets within search radius of a center point *)
let rec find_nearby_creets tree center_x center_y search_radius =
   (* Helper func: Does a circle touch a rectangle? *)
  let circle_touches_rectangle rx ry rw rh cx cy r =
    let nearest_x = max rx (min cx (rx +. rw)) in  
    let nearest_y = max ry (min cy (ry +. rh)) in
    let dist_x = cx -. nearest_x in
    let dist_y = cy -. nearest_y in
    (dist_x *. dist_x +. dist_y *. dist_y) <= (r *. r)  
  in
  
  (* Skip if search circle doesn't touch this quadrant *)
  if not (circle_touches_rectangle tree.bounds_x tree.bounds_y tree.bounds_w tree.bounds_h center_x center_y search_radius) then 
    []
  else
    (* Check creets stored in current node *)
    let local_matches = List.filter (fun creet_obj ->
      let dx = creet_obj.x -. center_x in
      let dy = creet_obj.y -. center_y in
      (dx *. dx +. dy *. dy) <= (search_radius *. search_radius)  
    ) tree.stored_creets in
    
    (* Recursively check child quadrants *)
    let child_matches = List.fold_left (fun acc child_node ->
      (find_nearby_creets child_node center_x center_y search_radius) @ acc
    ) [] tree.sub_nodes in
    
    (* Combine results *)
    local_matches @ child_matches  

(* Main collision detection: check if healthy creet touches contaminated ones *)
let check_collisions creet all_creets =
  if creet.status = Healthy && not creet.is_dragged then
    let collision_radius = creet_hitbox_size *. creet.size_factor in
    
    (* Step 1: Build quadtree with only contaminated creets for efficiency *)
    let contaminated_creets = List.filter (fun c -> c.status <> Healthy) all_creets in
    let quad_tree = List.fold_left (fun acc_tree c -> add_creet_to_tree acc_tree c) 
                      (make_quad_tree 0.0 0.0 800.0 600.0) contaminated_creets in
    
    (* Step 2: Query quadtree for nearby contaminated creets *)
    let nearby_creets = find_nearby_creets quad_tree creet.x creet.y collision_radius in
    
    (* Step 3: Check collision with nearby creets only *)
    let has_contamination = List.exists (fun other ->
      creet != other &&
      let creet_half_size = (creet_hitbox_size *. creet.size_factor) /. 2. in
      let other_half_size = (creet_hitbox_size *. other.size_factor) /. 2. in
      abs_float (creet.x -. other.x) < (creet_half_size +. other_half_size) &&
      abs_float (creet.y -. other.y) < (creet_half_size +. other_half_size) &&
      Random.int 100 < 2
    ) nearby_creets in
    
    if has_contamination then change_status creet Contaminated