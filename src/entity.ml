open Js_of_ocaml
open Creet
module Html = Dom_html

let create_axis_aligned_creet img_src x y =
  (* Choose to move either horizontally or vertically, not both *)
  if Random.bool() then
    (* Horizontal movement *)
    let base_speed = 20. in
    let dx = if Random.bool() then base_speed else -.base_speed in
    create_creet img_src x y dx 0.
  else
    (* Vertical movement *)
    let base_speed = 20. in
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