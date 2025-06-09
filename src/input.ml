open Js_of_ocaml
module Html = Dom_html

(* Toggle flag for showing hitboxes and debug information *)
let show_hitboxes = ref false

(* Handle keydown events for debugging features *)
let keydown_handler ev =
  let key_code = ev##.keyCode in
  
  (* 'H' key toggles hitbox display *)
  if key_code = 72 then begin
    show_hitboxes := not !show_hitboxes;
    if !show_hitboxes then
      Printf.printf "Hitboxes enabled. Mean spawn rate: %d%%, Berserker spawn rate: %d%%\n" 
        Creet.mean_spawn_rate Creet.berserker_spawn_rate
    else
      Printf.printf "Hitboxes disabled\n";
    Js._true
  end else
    Js._false