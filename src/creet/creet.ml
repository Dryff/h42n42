open Js_of_ocaml
module Html = Dom_html

(* Creet type definition *)
type creet = {
  mutable x: float; (* X position *)
  mutable y: float; (* Y position *)
  mutable dx: float; (* X velocity - will be non-zero or dy will be non-zero, but not both *)
  mutable dy: float; (* Y velocity - will be non-zero or dx will be non-zero, but not both *)
  image: Html.imageElement Js.t (* Image element *)
}

(* Create a new creet *)
let create_creet img_src x y dx dy =
  let img = Html.createImg Html.document in
  img##.src := Js.string img_src;
  { x; y; dx; dy; image = img }

(* Update creet position and handle bouncing *)
let last_direction_change = ref (Js.to_float (Js.date##now) /. 1000.)

(* Global speed factor *)
let global_speed = ref 1.0

(* Keep the original interface but use the global speed factor internally *)
let update_creet creet canvas_width canvas_height dt =
  (* Check if it's time to change direction *)
  let current_time = Js.to_float (Js.date##now) /. 1000. in
  
  if current_time -. !last_direction_change > (Random.float 5.) +. 1. then begin
    (* Randomly choose new direction: either horizontal or vertical *)
    let base_speed = 10. in
    if Random.bool () then begin
      creet.dx <- (if Random.bool () then base_speed else -.base_speed);
      creet.dy <- 0.
    end else begin
      creet.dx <- 0.;
      creet.dy <- (if Random.bool () then base_speed else -.base_speed)
    end;
    last_direction_change := current_time
  end;
  
  (* Update position based on velocity, applying the global speed factor *)
  creet.x <- creet.x +. creet.dx *. dt *. 10. *. !global_speed;
  creet.y <- creet.y +. creet.dy *. dt *. 10. *. !global_speed;
  
  let half_width = 30. in
  let half_height = 30. in
  
  (* Bounce off left and right borders *)
  if creet.x < half_width || creet.x > (float_of_int canvas_width) -. half_width then begin
    creet.dx <- -.creet.dx;
    (* Ensure creet stays within bounds *)
    creet.x <- max half_width (min ((float_of_int canvas_width) -. half_width) creet.x);
  end;
  
  (* Bounce off top and bottom borders, accounting for image height *)
  if creet.y < half_height || creet.y > (float_of_int canvas_height) -. half_height then begin
    creet.dy <- -.creet.dy;
    (* Ensure creet stays within bounds *)
    creet.y <- max half_height (min ((float_of_int canvas_height) -. half_height) creet.y);
  end

(* Draw a creet with coordinate display and origin marker *)
let draw_creet context creet =
  let float_to_js f = Js.number_of_float f in
  let width = 70. in (* Width of image *)
  let height = 70. in (* Height of image *)
  
  (* Calculate the top-left position for drawing the image
   by offsetting from the center position *)
  let draw_x = creet.x -. (width /. 2.) in
  let draw_y = creet.y -. (height /. 2.) in
  
  (* Draw the creet image, offset so x,y is the center *)
  context##drawImage_withSize
    creet.image
    (float_to_js draw_x)
    (float_to_js draw_y)
    (float_to_js width)
    (float_to_js height)