open Js_of_ocaml
open Creet
module Html = Dom_html

type t = Creet.creet

type spell_circle = {
  x: float;        
  y: float;        
  radius: float;   
  duration: float; 
}

let draw_creet context creet =
  let float_to_js f = Js.number_of_float f in
  let base_size = Creet.creet_hitbox_size in
  let scale_factor = 1.2 in
  let size = base_size *. creet.size_factor *. scale_factor in
  let image_draw_x = creet.x -. (size /. 2.) in
  let image_draw_y = creet.y -. (size /. 2.) in
  let hitbox_width = base_size *. creet.size_factor in
  let hitbox_height = base_size *. creet.size_factor in
  let hitbox_draw_x = creet.x -. (hitbox_width /. 2.) in
  let hitbox_draw_y = creet.y -. (hitbox_height /. 2.) in
  context##drawImage_withSize
    creet.image
    (float_to_js image_draw_x)
    (float_to_js image_draw_y)
    (float_to_js size)
    (float_to_js size);
  if !Input.debug_mode then begin
    let hitbox_color = match creet.status with
      | Mean -> "#FF0000"      (* Red *)
      | Berserker -> "#FF69B4" (* Pink *)
      | Contaminated -> "#FF8C00" (* Dark Orange *)
      | Healthy -> "#00FF00"   (* Green *)
    in
    context##.strokeStyle := Js.string hitbox_color;
    context##.lineWidth := Js.number_of_float 2.;
    context##strokeRect
      (float_to_js hitbox_draw_x)
      (float_to_js hitbox_draw_y)
      (float_to_js hitbox_width)
      (float_to_js hitbox_height)
  end

(* GAME OVER SCREEN *)
let display_game_over context canvas_width canvas_height elapsed creets =
  context##.fillStyle := Js.string "rgba(0, 0, 0, 0.7)";
  context##fillRect
    (Js.number_of_float 0.)
    (Js.number_of_float 0.)
    (Js.number_of_float (float_of_int canvas_width))
    (Js.number_of_float (float_of_int canvas_height));
  
  let center_y = (float_of_int canvas_height /. 2.) -. (float_of_int canvas_height *. 0.15) in
  
  (* Game Over text *)
  context##.fillStyle := Js.string "#FF0000"; 
  context##.font := Js.string "bold 48px Arial";
  context##.textAlign := Js.string "center";
  context##fillText 
    (Js.string "GAME OVER")
    (Js.number_of_float (float_of_int canvas_width /. 2.))
    (Js.number_of_float center_y);
  
  (* Subtext *)
  context##.fillStyle := Js.string "#FFFFFF";
  context##.font := Js.string "24px Arial";
  context##fillText 
    (Js.string "All creets are infected!")
    (Js.number_of_float (float_of_int canvas_width /. 2.))
    (Js.number_of_float (center_y +. 50.));
  
  (* Display time survived *)
  context##.fillStyle := Js.string "#FFFF00";
  context##.font := Js.string "28px Arial";
  let minutes = int_of_float (elapsed /. 60.) in
  let seconds = int_of_float (elapsed -. (float_of_int minutes *. 60.)) in
  let time_text = Printf.sprintf "Time Survived: %02d:%02d" minutes seconds in
  context##fillText 
    (Js.string time_text)
    (Js.number_of_float (float_of_int canvas_width /. 2.))
    (Js.number_of_float (center_y +. 100.));

  (* Calculate and display score (creets × seconds) *)
  context##.fillStyle := Js.string "#00FFFF";
  context##.font := Js.string "32px Arial";
  let total_seconds = int_of_float elapsed in
  let creet_count = List.length creets in
  let final_score = creet_count * total_seconds in
  
  let score_text = Printf.sprintf "Score: %d " 
    final_score in
    
  context##fillText
    (Js.string score_text)
    (Js.number_of_float (float_of_int canvas_width /. 2.))
    (Js.number_of_float (center_y +. 140.));
  
  (* Draw replay button *)
  let button_width = 120. in
  let button_height = 40. in
  let button_x = (float_of_int canvas_width /. 2.) -. (button_width /. 2.) in
  let button_y = center_y +. 190. in
  context##.fillStyle := Js.string "#4CAF50";
  context##fillRect
    (Js.number_of_float button_x)
    (Js.number_of_float button_y)
    (Js.number_of_float button_width)
    (Js.number_of_float button_height);
  context##.fillStyle := Js.string "#FFFFFF"; (* White text *)
  context##.font := Js.string "20px Arial";
  context##fillText 
    (Js.string "Replay")
    (Js.number_of_float (float_of_int canvas_width /. 2.))
    (Js.number_of_float (button_y +. 25.));
  context##.textAlign := Js.string "left"

(* Function to check if a click is within the replay button *)
let is_click_on_replay_button x y canvas_width canvas_height =
  let button_width = 120. in
  let button_height = 40. in
  let center_y = (float_of_int canvas_height /. 2.) -. (float_of_int canvas_height *. 0.15) in
  let button_x = (float_of_int canvas_width /. 2.) -. (button_width /. 2.) in
  let button_y = center_y +. 190. in

  x >= button_x && x <= button_x +. button_width &&
  y >= button_y && y <= button_y +. button_height

(* Draw background and static elements *)
let draw_background_elements context doc canvas_width canvas_height (hospital_width, hospital_height, hospital_spacing, initial_hospital_x, num_hospitals) creets spawn_interval_low spawn_interval_high =
  let hospital_y = float_of_int canvas_height -. hospital_height -. -100. in
  
  (* Load and draw background image *)
  let bg_img = Html.createImg doc in
  bg_img##.src := Js.string "background.jpeg";
  context##drawImage_withSize
    bg_img
    (Js.number_of_float 0.)
    (Js.number_of_float 0.)
    (Js.number_of_float (float_of_int canvas_width))
    (Js.number_of_float (float_of_int canvas_height));

  (* Load and draw river image *)
  let river_img = Html.createImg doc in
  river_img##.src := Js.string "river_background.png";
  context##drawImage_withSize
    river_img
    (Js.number_of_float 0.)
    (Js.number_of_float 0.)
    (Js.number_of_float (float_of_int canvas_width))
    (Js.number_of_float 50.);
  if !Input.debug_mode then (
    context##.strokeStyle := Js.string "#aa00FF"; (* Blue for river *)
    context##.lineWidth := Js.number_of_float 2.;
    context##strokeRect
      (Js.number_of_float 0.)
      (Js.number_of_float 0.)
      (Js.number_of_float (float_of_int canvas_width))
      (Js.number_of_float 50.)
  );

  (* Draw hospitals houses *)
  for i = 0 to num_hospitals - 1 do
    let hospital_x = initial_hospital_x +. (float_of_int i *. hospital_spacing) in
    let hospital_img = Html.createImg doc in
    hospital_img##.src := Js.string "hospital.png";
    context##drawImage_withSize
      hospital_img
      (Js.number_of_float hospital_x)
      (Js.number_of_float hospital_y)
      (Js.number_of_float hospital_width)
      (Js.number_of_float hospital_height);
    if !Input.debug_mode then (
      context##.strokeStyle := Js.string "#0095ff";
      context##.lineWidth := Js.number_of_float 2.;
      context##strokeRect
        (Js.number_of_float hospital_x)
        (Js.number_of_float hospital_y)
        (Js.number_of_float hospital_width)
        (Js.number_of_float hospital_height)
    )
  done;
  
  (* DEBUG MODE TEXT *)
  if !Input.debug_mode then begin
    context##.fillStyle := Js.string "#000000";
    context##.font := Js.string "16px Arial";
    let speed_text = Printf.sprintf "Speed: %d%%" (int_of_float (!Creet.global_speed *. 100.)) in
    context##fillText 
      (Js.string speed_text)
      (Js.number_of_float 10.)
      (Js.number_of_float 80.);
    let creet_count_text = Printf.sprintf "Creets: %d" (List.length creets) in
    context##fillText 
      (Js.string creet_count_text)
      (Js.number_of_float 10.)
      (Js.number_of_float 105.);
    context##fillText 
      (Js.string (Printf.sprintf "Spawn Rate: %.0fs - %.0fs" spawn_interval_low spawn_interval_high))
      (Js.number_of_float 10.)
      (Js.number_of_float 130.);
      
  end

let draw_spell_circle context circle =
  (* Save current context state *)
  context##save;
  
  (* Fading effect *)
  let opacity = 0.4 *. (circle.duration /. 3.0) in
  context##.fillStyle := Js.string (Printf.sprintf "rgba(64, 224, 208, %.2f)" opacity);
  context##.strokeStyle := Js.string (Printf.sprintf "rgba(64, 224, 208, %.2f)" (opacity +. 0.1));
  context##.lineWidth := Js.number_of_float 3.;
  
  (* Draw the circle with fill *)
  context##beginPath;
  context##arc 
    (Js.number_of_float circle.x) 
    (Js.number_of_float circle.y) 
    (Js.number_of_float circle.radius) 
    (Js.number_of_float 0.) 
    (Js.number_of_float (2. *. 3.14159)) 
    Js._false;
  context##fill;
  context##stroke;
  
  (* Restore context state *)
  context##restore

(* PAUSE SCREEN *)
let display_pause_overlay context canvas_width canvas_height =
  context##.fillStyle := Js.string "rgba(0, 0, 0, 0.5)";
  context##fillRect
    (Js.number_of_float 0.)
    (Js.number_of_float 0.)
    (Js.number_of_float (float_of_int canvas_width))
    (Js.number_of_float (float_of_int canvas_height));
  context##.fillStyle := Js.string "#FFFFFF"; (* White text *)
  context##.font := Js.string "bold 48px Arial";
  context##.textAlign := Js.string "center";
  context##fillText 
    (Js.string "PAUSED")
    (Js.number_of_float (float_of_int canvas_width /. 2.))
    (Js.number_of_float (float_of_int canvas_height /. 2.));
  context##.font := Js.string "24px Arial";
  context##fillText 
    (Js.string "Click on the button to resume")
    (Js.number_of_float (float_of_int canvas_width /. 2.))
    (Js.number_of_float (float_of_int canvas_height /. 2. +. 50.));
  context##.textAlign := Js.string "left"

let render context doc canvas creets elapsed game_over is_paused spell_circles spawn_interval_low spawn_interval_high hospital_config =
  let canvas_width = canvas##.width in
  let canvas_height = canvas##.height in
  
  context##clearRect
    (Js.number_of_float 0.)
    (Js.number_of_float 0.)
    (Js.number_of_float (float_of_int canvas_width))
    (Js.number_of_float (float_of_int canvas_height));
  
  (* Draw background elements and hospitals *)
  draw_background_elements context doc canvas_width canvas_height hospital_config creets spawn_interval_low spawn_interval_high;

  (* Draw creets & spell circles *)
  List.iter (draw_creet context) creets;
  List.iter (draw_spell_circle context) spell_circles;
  
  (* Display game over screen if needed *)
  if game_over then
    display_game_over context canvas_width canvas_height elapsed creets
  else if is_paused then
    display_pause_overlay context canvas_width canvas_height