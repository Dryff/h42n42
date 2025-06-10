open Js_of_ocaml
module Html = Dom_html

(* Toggle flag for showing hitboxes and debug information *)
let debug_mode = ref false

(* Handle keydown events for debugging features *)
let keydown_handler ev =
  let key_code = ev##.keyCode in
  
  if key_code = 72 then begin
    debug_mode := not !debug_mode;
    Js._true
  end else
    Js._false