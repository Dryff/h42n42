open Js_of_ocaml
open Creet
module Html = Dom_html

(** Create a creet with movement only along X or Y axis (not diagonal)
    @param img_src The image source for the creet
    @param x Initial X position
    @param y Initial Y position
    @return A new creet
*)
val create_axis_aligned_creet : string -> float -> float -> creet

(** Create a creet at a random position
    @param img_src The image source for the creet
    @param canvas_width The width of the canvas
    @param canvas_height The height of the canvas
    @return A new creet at a random position
*)
val create_random_creet : string -> int -> int -> creet

(** Create a healthy creet at a random position
    @param canvas_width The width of the canvas
    @param canvas_height The height of the canvas
    @return A new healthy creet at a random position
*)
val create_random_healthy_creet : int -> int -> creet

(** Check if a point is inside a creet's hitbox
    @param creet The creet to check
    @param x X coordinate of the point
    @param y Y coordinate of the point
    @return true if the point is inside the creet, false otherwise
*)
val is_point_inside_creet : creet -> float -> float -> bool

(** Find a creet at the given position
    @param creets List of creets to search
    @param x X coordinate of the point
    @param y Y coordinate of the point
    @return Some creet if found at position, None otherwise
*)
val find_creet_at_position : creet list -> float -> float -> creet option

(** Check if all creets are unhealthy (game over condition)
    @param creets List of creets to check
    @return true if all creets are unhealthy and the list is not empty
*)
val all_creets_unhealthy : creet list -> bool

(** Create a list of initial creets
    @param count Number of creets to create
    @param canvas_width The width of the canvas
    @param canvas_height The height of the canvas
    @return A list of healthy creets
*)
val create_initial_creets : int -> int -> int -> creet list