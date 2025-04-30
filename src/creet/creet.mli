open Js_of_ocaml
module Html = Dom_html
type health_status = Healthy | Contaminated | Berserker | Mean
(* Creet type definition *)
type creet = {
mutable x: float; (* X position *)
mutable y: float; (* Y position *)
mutable dx: float; (* X velocity *)
mutable dy: float; (* Y velocity *)
mutable status: health_status; (* Health status: healthy or contaminated *)
mutable speed_factor: float; (* Speed factor for the creet *)
mutable is_dragged: bool; (* Indicates if the creet is being dragged *)
mutable size_factor: float;
mutable last_direction_change: float; (* Per-creet direction change timer *)
 image: Html.imageElement Js.t (* Image element *)
}
(* Create a new creet *)
val create_creet : string -> float -> float -> float -> float -> creet
(* Update creet's image based on health status *)
val update_creet_image : creet -> unit
(* Global speed factor that will increase over time *)
val global_speed : float ref
(* Update creet position and handle bouncing *)
val update_creet : creet -> int -> int -> float -> creet list -> unit
(* Draw a creet *)
val check_collisions : creet -> creet list -> unit
(* Handle mouse events for dragging the creet *)
val creet_hitbox_size : float

val change_status : creet -> health_status -> unit
(* Check if a creet is being dragged *)