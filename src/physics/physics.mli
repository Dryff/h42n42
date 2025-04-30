open Js_of_ocaml
open Creet
module Html = Dom_html

(** Constants for physics calculations *)
val river_height : float
val hospital_height : float
val creet_collision_radius : float

(** Assign a new random direction to a creet 
    @param creet The creet to assign a new direction to
*)
val assign_new_direction : creet -> unit

(** Find the nearest healthy creet 
    @param creet The creet looking for a target
    @param all_creets List of all creets to search
    @return Some creet if a healthy creet is found, None otherwise
*)
val find_nearest_healthy_creet : creet -> creet list -> creet option

(** Check if a creet is touching the river
    @param creet The creet to check
    @return true if the creet is touching the river, false otherwise
*)
val check_river_contact : creet -> bool

(** Calculate distance between two creets
    @param creet1 The first creet
    @param creet2 The second creet
    @return The distance between the creets
*)
val distance_between : creet -> creet -> float

(** Check if two creets are colliding
    @param creet1 The first creet
    @param creet2 The second creet
    @return true if the creets are colliding, false otherwise
*)
val are_colliding : creet -> creet -> bool

(** Handle boundary collisions and keep creet inside canvas
    @param creet The creet to check
    @param canvas_width The width of the canvas
    @param canvas_height The height of the canvas
*)
val handle_boundary_collisions : creet -> int -> int -> unit

(** Update the size of a berserker creet
    @param creet The creet to update
    @param dt Time delta since last frame
*)
val update_berserker_size : creet -> float -> unit

(** Update creet position based on velocity
    @param creet The creet to update
    @param dt Time delta since last frame
*)
val update_position : creet -> float -> unit

(** Handle Mean creet hunting behavior
    @param creet The creet to update
    @param all_creets List of all creets
*)
val handle_mean_creet_hunting : creet -> creet list -> unit

(** Update creet direction periodically
    @param creet The creet to update
*)
val update_direction : creet -> unit

(** Update all physics for a creet
    @param creet The creet to update
    @param canvas_width The width of the canvas
    @param canvas_height The height of the canvas
    @param dt Time delta since last frame
    @param all_creets List of all creets
*)
val update_creet_physics : creet -> int -> int -> float -> creet list -> unit