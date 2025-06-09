open Js_of_ocaml
module Html = Dom_html

type t = Creet.creet

(* Spell circle effect type *)
type spell_circle = {
  x: float;        (* Center X position *)
  y: float;        (* Center Y position *)
  radius: float;   (* Circle radius *)
  duration: float; (* Duration remaining in seconds *)
}

val draw_creet : Dom_html.canvasRenderingContext2D Js.t -> Creet.creet -> unit

val display_game_over : Dom_html.canvasRenderingContext2D Js.t -> int -> int -> float -> Creet.creet list -> unit

val display_pause_overlay : Dom_html.canvasRenderingContext2D Js.t -> int -> int -> unit

val is_click_on_replay_button : float -> float -> int -> int -> bool

val draw_background_elements : 
  Dom_html.canvasRenderingContext2D Js.t -> 
  Dom_html.document Js.t -> 
  int -> 
  int -> 
  (float * float * float * float * int) -> 
  Creet.creet list -> 
  float ->
  float ->
  unit

val draw_spell_circle : Dom_html.canvasRenderingContext2D Js.t -> spell_circle -> unit

val render : 
  Dom_html.canvasRenderingContext2D Js.t -> 
  Dom_html.document Js.t -> 
  Dom_html.canvasElement Js.t -> 
  Creet.creet list -> 
  float -> 
  bool -> 
  bool ->
  spell_circle list -> 
  float -> 
  float -> 
  (float * float * float * float * int) ->
  unit