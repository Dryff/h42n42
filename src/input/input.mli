open Js_of_ocaml

val show_hitboxes : bool ref
val keydown_handler : Dom_html.keyboardEvent Js.t -> bool Js.t

val mousedown_handler :
  game_over:bool ref ->
  creets:Creet.creet list ref ->
  dragging:Creet.creet option ref ->
  offset_x:float ref ->
  offset_y:float ref ->
  Dom_html.canvasElement Js.t ->
  Dom_html.mouseEvent Js.t ->
  bool Js.t

val mousemove_handler :
  dragging:Creet.creet option ref ->
  canvas_width:int ->
  canvas_height:int ->
  offset_x:float ref ->
  offset_y:float ref ->
  Dom_html.canvasElement Js.t ->
  Dom_html.mouseEvent Js.t ->
  bool Js.t

val mouseup_handler :
  dragging:Creet.creet option ref ->
  unit ->
  bool Js.t