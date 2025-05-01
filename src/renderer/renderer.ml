open Js_of_ocaml
open Creet
module Html = Dom_html

type t = Creet.creet

(* Spell circle effect type *)
type spell_circle = {
  x: float;        (* Center X position *)
  y: float;        (* Center Y position *)
  radius: float;   (* Circle radius *)
  duration: float; (* Duration remaining in seconds *)
}

(* Draw a creet on the canvas *)
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
  if !Input.show_hitboxes then begin
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

(* Display game over screen *)
let display_game_over context canvas_width canvas_height elapsed creets =
  (* Semi-transparent overlay *)
  context##.fillStyle := Js.string "rgba(0, 0, 0, 0.7)";
  context##fillRect
    (Js.number_of_float 0.)
    (Js.number_of_float 0.)
    (Js.number_of_float (float_of_int canvas_width))
    (Js.number_of_float (float_of_int canvas_height));
  
  (* Game Over text *)
  context##.fillStyle := Js.string "#FF0000"; (* Red text *)
  context##.font := Js.string "bold 48px Arial";
  context##.textAlign := Js.string "center";
  context##fillText 
    (Js.string "GAME OVER")
    (Js.number_of_float (float_of_int canvas_width /. 2.))
    (Js.number_of_float (float_of_int canvas_height /. 2.));
  
  (* Subtext *)
  context##.fillStyle := Js.string "#FFFFFF"; (* White text *)
  context##.font := Js.string "24px Arial";
  context##fillText 
    (Js.string "All creets are infected!")
    (Js.number_of_float (float_of_int canvas_width /. 2.))
    (Js.number_of_float (float_of_int canvas_height /. 2. +. 50.));
  
  (* Display time survived *)
  context##.fillStyle := Js.string "#FFFF00"; (* Yellow text *)
  context##.font := Js.string "28px Arial";
  let minutes = int_of_float (elapsed /. 60.) in
  let seconds = int_of_float (elapsed -. (float_of_int minutes *. 60.)) in
  let time_text = Printf.sprintf "Time Survived: %02d:%02d" minutes seconds in
  context##fillText 
    (Js.string time_text)
    (Js.number_of_float (float_of_int canvas_width /. 2.))
    (Js.number_of_float (float_of_int canvas_height /. 2. +. 100.));

  (* Display score calculation (creets × seconds) *)
  context##.fillStyle := Js.string "#00FFFF";  (* Cyan text *)
  context##.font := Js.string "32px Arial";
  
  (* Calculate the score by multiplying number of creets by elapsed seconds *)
  let total_seconds = int_of_float elapsed in
  let creet_count = List.length creets in
  let final_score = creet_count * total_seconds in
  
  let score_text = Printf.sprintf "Score: %d " 
    final_score in
    
  context##fillText
    (Js.string score_text)
    (Js.number_of_float (float_of_int canvas_width /. 2.))
    (Js.number_of_float (float_of_int canvas_height /. 2. +. 140.));
  
  (* Draw replay button *)
  let button_width = 120. in
  let button_height = 40. in
  let button_x = (float_of_int canvas_width /. 2.) -. (button_width /. 2.) in
  let button_y = (float_of_int canvas_height /. 2.) +. 150. in
  
  (* Draw button background *)
  context##.fillStyle := Js.string "#4CAF50"; (* Green button *)
  context##fillRect
    (Js.number_of_float button_x)
    (Js.number_of_float button_y)
    (Js.number_of_float button_width)
    (Js.number_of_float button_height);
  
  (* Draw button text *)
  context##.fillStyle := Js.string "#FFFFFF"; (* White text *)
  context##.font := Js.string "20px Arial";
  context##fillText 
    (Js.string "Replay")
    (Js.number_of_float (float_of_int canvas_width /. 2.))
    (Js.number_of_float (button_y +. 25.));
  
  (* Reset text alignment for other text *)
  context##.textAlign := Js.string "left"

(* Function to check if a click is within the replay button *)
let is_click_on_replay_button x y canvas_width canvas_height =
  let button_width = 120. in
  let button_height = 40. in
  let button_x = (float_of_int canvas_width /. 2.) -. (button_width /. 2.) in
  let button_y = (float_of_int canvas_height /. 2.) +. 150. in
  
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
  if !Input.show_hitboxes then (
    context##.strokeStyle := Js.string "#aa00FF"; (* Blue for river *)
    context##.lineWidth := Js.number_of_float 2.;
    context##strokeRect
      (Js.number_of_float 0.)
      (Js.number_of_float 0.)
      (Js.number_of_float (float_of_int canvas_width))
      (Js.number_of_float 50.)
  );

  (* Draw hospitals at regular intervals *)
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
    if !Input.show_hitboxes then (
      context##.strokeStyle := Js.string "#0095ff";
      context##.lineWidth := Js.number_of_float 2.;
      context##strokeRect
        (Js.number_of_float hospital_x)
        (Js.number_of_float hospital_y)
        (Js.number_of_float hospital_width)
        (Js.number_of_float hospital_height)
    )
  done;
  
  if !Input.show_hitboxes then begin
    (* Display current speed as text *)
    context##.fillStyle := Js.string "#000000"; (* Black text *)
    context##.font := Js.string "16px Arial";
    let speed_text = Printf.sprintf "Speed: %d%%" (int_of_float (!Creet.global_speed *. 100.)) in
    context##fillText 
      (Js.string speed_text)
      (Js.number_of_float 10.)
      (Js.number_of_float 80.);
      
    (* Display creet count *)
    let creet_count_text = Printf.sprintf "Creets: %d" (List.length creets) in
    context##fillText 
      (Js.string creet_count_text)
      (Js.number_of_float 10.)
      (Js.number_of_float 105.);
      
    (* Display spawn interval *)
    context##fillText 
      (Js.string (Printf.sprintf "Spawn Interval: %.0fs - %.0fs" spawn_interval_low spawn_interval_high))
      (Js.number_of_float 10.)
      (Js.number_of_float 130.);
      
    (* Display Mean and Berserker spawn rates *)
    context##fillText 
      (Js.string (Printf.sprintf "Mean Spawn Rate: %d%%" Creet.mean_spawn_rate))
      (Js.number_of_float 10.)
      (Js.number_of_float 155.);
      
    context##fillText 
      (Js.string (Printf.sprintf "Berserker Spawn Rate: %d%%" Creet.berserker_spawn_rate))
      (Js.number_of_float 10.)
      (Js.number_of_float 180.);
  end

(* Draw the spell circle effect *)
let draw_spell_circle context circle =
  (* Save current context state *)
  context##save;
  
  (* Calculate opacity based on remaining duration - fade from 0.4 to 0 over the duration *)
  let opacity = 0.4 *. (circle.duration /. 3.0) in
  
  (* Configure the circle appearance with fading opacity *)
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

(* Display pause overlay when game is paused *)
let display_pause_overlay context canvas_width canvas_height =
  (* Semi-transparent overlay *)
  context##.fillStyle := Js.string "rgba(0, 0, 0, 0.5)";
  context##fillRect
    (Js.number_of_float 0.)
    (Js.number_of_float 0.)
    (Js.number_of_float (float_of_int canvas_width))
    (Js.number_of_float (float_of_int canvas_height));
  
  (* Pause message *)
  context##.fillStyle := Js.string "#FFFFFF"; (* White text *)
  context##.font := Js.string "bold 48px Arial";
  context##.textAlign := Js.string "center";
  context##fillText 
    (Js.string "PAUSED")
    (Js.number_of_float (float_of_int canvas_width /. 2.))
    (Js.number_of_float (float_of_int canvas_height /. 2.));
  
  (* Pause instructions *)
  context##.font := Js.string "24px Arial";
  context##fillText 
    (Js.string "Click on the button to resume")
    (Js.number_of_float (float_of_int canvas_width /. 2.))
    (Js.number_of_float (float_of_int canvas_height /. 2. +. 50.));
  
  (* Reset text alignment for other text *)
  context##.textAlign := Js.string "left"

let render context doc canvas creets elapsed game_over spell_circles spawn_interval_low spawn_interval_high hospital_config =
  let canvas_width = canvas##.width in
  let canvas_height = canvas##.height in
  
  (* Clear canvas *)
  context##clearRect
    (Js.number_of_float 0.)
    (Js.number_of_float 0.)
    (Js.number_of_float (float_of_int canvas_width))
    (Js.number_of_float (float_of_int canvas_height));
  
  (* Draw background elements and hospitals *)
  draw_background_elements context doc canvas_width canvas_height hospital_config creets spawn_interval_low spawn_interval_high;

  (* Draw all creets *)
  List.iter (draw_creet context) creets;

  (* Draw all spell circles *)
  List.iter (draw_spell_circle context) spell_circles;
  
  (* Display game over screen if needed *)
  if game_over then
    display_game_over context canvas_width canvas_height elapsed creets