open Js_of_ocaml
open Creet
module Html = Dom_html

let mousedown_handler ~game_over ~creets ~dragging ~offset_x ~offset_y canvas ev =
  if !game_over then Js._false
  else begin
    let mouse_x = Js.to_float ev##.clientX in
    let mouse_y = Js.to_float ev##.clientY in
    let canvas_rect = canvas##getBoundingClientRect in
    let canvas_x = mouse_x -. Js.to_float canvas_rect##.left in
    let canvas_y = mouse_y -. Js.to_float canvas_rect##.top in
    match Entity.find_creet_at_position !creets canvas_x canvas_y with
    | Some creet ->
        offset_x := creet.x -. canvas_x;
        offset_y := creet.y -. canvas_y;
        creet.is_dragged <- true;
        dragging := Some creet;
        Js._true
    | None -> Js._false
  end

let mousemove_handler ~dragging ~canvas_width ~canvas_height ~offset_x ~offset_y canvas ev =
  match !dragging with
  | Some creet ->
      let mouse_x = Js.to_float ev##.clientX in
      let mouse_y = Js.to_float ev##.clientY in
      let canvas_rect = canvas##getBoundingClientRect in
      let canvas_x = mouse_x -. Js.to_float canvas_rect##.left in
      let canvas_y = mouse_y -. Js.to_float canvas_rect##.top in
      let half_width = 35. in
      let half_height = 35. in
      creet.x <- min (float_of_int canvas_width -. half_width)
                  (max half_width (canvas_x +. !offset_x));
      creet.y <- min (float_of_int canvas_height -. half_height)
                  (max half_height (canvas_y +. !offset_y));
      Js._true
  | None -> Js._false

let mouseup_handler ~dragging () =
  (match !dragging with
   | Some creet ->
       creet.is_dragged <- false;
       creet.last_direction_change <- 0.
   | None -> ());
  dragging := None;
  Js._true

let show_hitboxes = ref false

let keydown_handler (ev : Dom_html.keyboardEvent Js.t) =
  let key = Js.to_string (Js.Optdef.get ev##.key (fun () -> Js.string "")) in
  if key = "h" || key = "H" then (
    show_hitboxes := not !show_hitboxes;
    Js._true
  ) else Js._false