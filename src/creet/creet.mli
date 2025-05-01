open Js_of_ocaml
module Html = Dom_html

(* Health status type *)
type health_status = Healthy | Contaminated | Berserker | Mean

(* Creet type *)
type creet = {
  mutable x: float;
  mutable y: float;
  mutable dx: float;
  mutable dy: float;
  mutable status: health_status;
  mutable speed_factor: float;
  mutable is_dragged: bool;
  mutable size_factor: float;
  mutable last_direction_change: float;
  image: Html.imageElement Js.t
}

(* Global speed factor *)
val global_speed : float ref

(* Global hitbox size for creets and static elements *)
val creet_hitbox_size : float

(* Spawn rates for special creet types *)
val mean_spawn_rate : int
val berserker_spawn_rate : int

(* Create a new creet - default to healthy status *)
val create_creet : string -> float -> float -> float -> float -> creet

(* Update creet's image based on status *)
val update_creet_image : creet -> unit

(* Change creet status and update associated properties *)
val change_status : creet -> health_status -> unit

(* Assign a new random direction to a creet *)
val assign_new_direction : creet -> unit

(* Find the nearest healthy creet *)
val find_nearest_healthy_creet : creet -> creet list -> creet option

(* Update creet position, handle bouncing, and check for river/hospital contact *)
val update_creet : creet -> int -> int -> float -> creet list -> unit

(* Check for collisions between creets *)
val check_collisions : creet -> creet list -> unit