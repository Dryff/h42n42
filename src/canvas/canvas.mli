open Js_of_ocaml

(* Create a canvas element with specified dimensions *)
val create_canvas : Dom_html.element Js.t -> int -> int -> Dom_html.canvasElement Js.t

(* Get an HTML element by its ID *)
val get_element_by_id : string -> Dom_html.element Js.t