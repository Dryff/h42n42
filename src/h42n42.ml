open Js_of_ocaml
open Js_of_ocaml_lwt

(* Module for accessing DOM elements *)
module Html = Dom_html

(* Function to get document element by ID *)
let get_element_by_id id =
  Js.Opt.get
    (Html.document##getElementById(Js.string id))
    (fun () -> failwith ("Element " ^ id ^ " not found"))

(* Create game canvas in the page *)
let create_canvas parent width height =
  let canvas = Html.createCanvas Html.document in
  canvas##.width := width;
  canvas##.height := height;
  Dom.appendChild parent canvas;
  canvas

(* Main entry point for the application *)
let init () =
  let doc = Html.document in
  let body = doc##.body in
  
  (* Create a div for our game *)
  let game_div = Html.createDiv doc in
  Dom.appendChild body game_div;
  
  (* Add a title *)
  let title = Html.createH1 doc in
  title##.textContent := Js.some (Js.string "H42N42 Simulation");
  Dom.appendChild game_div title;
  
  (* Create our game canvas *)
  let canvas_width = 800 in
  let canvas_height = 600 in
  let canvas = create_canvas game_div canvas_width canvas_height in
  
  (* Get canvas 2D context *)
  let context = canvas##getContext (Html._2d_) in
  
  (* Convert OCaml floats to JavaScript numbers *)
  let float_to_js f = Js.number_of_float f in
  
  (* Draw a simple background with river at top and hospital at bottom *)
  context##.fillStyle := Js.string "#eeeeee";  (* Light gray background *)
  context##fillRect 
    (float_to_js 0.) 
    (float_to_js 0.) 
    (float_to_js (float_of_int canvas_width)) 
    (float_to_js (float_of_int canvas_height));
  
  (* Draw river at top *)
  context##.fillStyle := Js.string "#3498db";  (* Blue for river *)
  context##fillRect 
    (float_to_js 0.) 
    (float_to_js 0.) 
    (float_to_js (float_of_int canvas_width)) 
    (float_to_js 30.);
  
  (* Draw hospital at bottom *)
  context##.fillStyle := Js.string "#e74c3c";  (* Red for hospital *)
  context##fillRect 
    (float_to_js 0.) 
    (float_to_js (float_of_int canvas_height -. 30.)) 
    (float_to_js (float_of_int canvas_width)) 
    (float_to_js 30.);
  
  Lwt.return ()

(* Start the application when the page is loaded *)
let () =
  Html.window##.onload := Html.handler (fun _ ->
    let _ = Lwt_js_events.async init in
    Js._false)