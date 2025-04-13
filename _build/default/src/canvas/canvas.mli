open Js_of_ocaml
module Html = Dom_html

(** Create a canvas element for the game and append it to the parent element
    @param parent The parent DOM element
    @param width Width of the canvas
    @param height Height of the canvas
    @return The created canvas element
*)
val create_canvas : #Dom.node Js.t -> int -> int -> Html.canvasElement Js.t

(** Get a DOM element by its ID
    @param id The ID of the element to find
    @return The found DOM element
    @raise Failure if the element is not found
*)
val get_element_by_id : string -> Html.element Js.t