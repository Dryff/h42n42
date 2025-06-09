open Js_of_ocaml
open Creet
module Html = Dom_html

let create_axis_aligned_creet img_src x y =
  (* Choose to move either horizontally or vertically, not both *)
  if Random.bool() then
    (* Horizontal movement *)
    let base_speed = 10. in
    let dx = if Random.bool() then base_speed else -.base_speed in
    create_creet img_src x y dx 0.
  else
    (* Vertical movement *)
    let base_speed = 10. in
    let dy = if Random.bool() then base_speed else -.base_speed in
    create_creet img_src x y 0. dy

(* Create a creet at a random position *)
let create_random_creet img_src canvas_width canvas_height =
  let x = Random.float (float_of_int canvas_width) in
  let y = Random.float (float_of_int canvas_height) in
  create_axis_aligned_creet img_src x y

(* Create a healthy creet at a random position *)
let create_random_healthy_creet canvas_width canvas_height =
  create_random_creet "HealthyCreet.png" canvas_width canvas_height

(* Check if a point is within a creet's hitbox *)
let is_point_inside_creet creet x y =
  let scaled_width = Creet.creet_hitbox_size *. creet.size_factor in
  let scaled_height = Creet.creet_hitbox_size *. creet.size_factor in
  
  x >= creet.x -. (scaled_width /. 2.) && 
  x <= creet.x +. (scaled_width /. 2.) &&
  y >= creet.y -. (scaled_height /. 2.) && 
  y <= creet.y +. (scaled_height /. 2.)

(* Find a creet at the given position *)
let find_creet_at_position creets x y =
  let found = ref None in
  List.iter (fun creet ->
    if is_point_inside_creet creet x y then
      found := Some creet
  ) creets;
  !found

(* Check if all creets are unhealthy (game over condition) *)
let all_creets_unhealthy creets =
  List.for_all (fun creet -> creet.status <> Healthy) creets && 
  List.length creets > 0

(* Create initial set of creets *)
let create_initial_creets count canvas_width canvas_height =
  let rec create_creets n acc =
    if n <= 0 then acc
    else 
      let new_creet = create_random_healthy_creet canvas_width canvas_height in
      create_creets (n-1) (new_creet :: acc)
  in
  create_creets count []