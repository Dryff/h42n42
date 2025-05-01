(* UI interface for h42n42 *)
open Js_of_ocaml
module Html = Dom_html

val init_ui : Html.document Js.t -> Html.divElement Js.t -> int -> unit
val show_game_over : unit -> unit
val update_timer : float -> unit
val create_spell_button : Dom_html.document Js.t -> Dom_html.element Js.t -> unit
val register_spell_button_handler : (unit -> unit) -> unit
val spell_button : Dom_html.buttonElement Js.t option ref
val create_pause_button : Dom_html.document Js.t -> Dom_html.element Js.t -> unit
val register_pause_button_handler : (unit -> unit) -> unit
val pause_button : Dom_html.buttonElement Js.t option ref
val update_pause_button_state : bool -> unit
val create_parameters_ui : Html.document Js.t -> Html.divElement Js.t -> unit
val create_parameter_button_pair : Dom_html.document Js.t -> Dom_html.element Js.t -> string -> Dom_html.buttonElement Js.t * Dom_html.buttonElement Js.t
val parameters_container : Dom_html.element Js.t option ref

(* Speed control handlers *)
val register_speed_plus_handler : (unit -> unit) -> unit
val register_speed_minus_handler : (unit -> unit) -> unit
val speed_plus_button : Dom_html.buttonElement Js.t option ref
val speed_minus_button : Dom_html.buttonElement Js.t option ref

(* Spawn rate control handlers *)
val register_spawn_plus_handler : (unit -> unit) -> unit
val register_spawn_minus_handler : (unit -> unit) -> unit

(* Mean and Berserker Creet spawn button handlers *)
val register_mean_creet_handler : (unit -> unit) -> unit
val register_berserker_creet_handler : (unit -> unit) -> unit

(* Healthy and Contaminated Creet spawn button handlers *)
val register_healthy_creet_handler : (unit -> unit) -> unit

(* Button references *)
val spawn_plus_button : Dom_html.buttonElement Js.t option ref
val spawn_minus_button : Dom_html.buttonElement Js.t option ref
val mean_creet_button : Dom_html.buttonElement Js.t option ref
val berserker_creet_button : Dom_html.buttonElement Js.t option ref
val healthy_creet_button : Dom_html.buttonElement Js.t option ref
