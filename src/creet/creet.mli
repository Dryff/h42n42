open Js_of_ocaml
module Html = Dom_html

(* Creet type definition *)
type creet = {
  mutable x: float; (* X position *)
  mutable y: float; (* Y position *)
  mutable dx: float; (* X velocity *)
  mutable dy: float; (* Y velocity *)
  image: Html.imageElement Js.t (* Image element *)
}

(* Create a new creet *)
val create_creet : string -> float -> float -> float -> float -> creet

(* Global speed factor that will increase over time *)
val global_speed : float ref

(* Update creet position and handle bouncing *)
val update_creet : creet -> int -> int -> float -> unit

(* Draw a creet *)
val draw_creet : Html.canvasRenderingContext2D Js.t -> creet -> unit