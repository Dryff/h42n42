open Js_of_ocaml
open Lwt.Syntax
open Tyxml.Html
module Html = Dom_html

(* References to UI elements *)
let timer_div : Dom_html.divElement Js.t option ref = ref None
let spell_button : Dom_html.buttonElement Js.t option ref = ref None
let spell_button_handler = ref (fun () -> ())
let pause_button : Dom_html.buttonElement Js.t option ref = ref None
let pause_button_handler = ref (fun () -> ())
let ui_container : Dom_html.divElement Js.t option ref = ref None
let speed_minus_handler = ref (fun () -> ())
let speed_plus_handler = ref (fun () -> ())
let spawn_minus_handler = ref (fun () -> ())
let spawn_plus_handler = ref (fun () -> ())
let mean_creet_button : Dom_html.buttonElement Js.t option ref = ref None
let mean_creet_handler = ref (fun () -> ())
let berserker_creet_button : Dom_html.buttonElement Js.t option ref = ref None
let berserker_creet_handler = ref (fun () -> ())
let healthy_creet_button : Dom_html.buttonElement Js.t option ref = ref None
let healthy_creet_handler = ref (fun () -> ())

(* Helper function to create DOM element from TyXML *)
let create_element_from_tyxml doc tyxml_element =
  let html_string = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) tyxml_element in
  let temp_container = Html.createDiv doc in
  temp_container##.innerHTML := Js.string html_string;
  temp_container##.firstChild

(* Helper function to extract specific element type from TyXML *)
let extract_element doc tyxml_element selector =
  let html_string = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) tyxml_element in
  let temp_container = Html.createDiv doc in
  temp_container##.innerHTML := Js.string html_string;
  let element = Js.Opt.get 
    (temp_container##querySelector (Js.string selector))
    (fun () -> failwith ("Element not found: " ^ selector)) in
  Js.Unsafe.coerce element

(* Create the spell button UI element *)
let create_spell_button doc parent =
  let button_tyxml = button 
    ~a:[
      a_id "spell-button";
      a_class ["spell-button"];
      a_style "padding: 10px 15px; font-size: 16px; background-color: #7B68EE; color: white; border: none; border-radius: 5px; cursor: pointer; margin: 0 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.2); z-index: 1000;"
    ]
    [txt "Cast Spell"] in
  
  let btn = extract_element doc button_tyxml "button" in
  
  let rec listen_for_clicks () =
    let* _event = Js_of_ocaml_lwt.Lwt_js_events.click btn in
    !spell_button_handler ();
    listen_for_clicks ()
  in
  
  Lwt.async (fun () -> listen_for_clicks ());
  
  spell_button := Some btn;
  Dom.appendChild parent btn

let create_pause_button doc parent =
  let button_tyxml = button 
    ~a:[
      a_id "pause-button";
      a_class ["pause-button"];
      a_style "padding: 10px 15px; font-size: 16px; background-color: #FF9800; color: white; border: none; border-radius: 5px; cursor: pointer; margin: 0 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.2); z-index: 1000;"
    ]
    [txt "Pause"] in
  
  let btn = extract_element doc button_tyxml "button" in
  
  let rec listen_for_clicks () =
    let* _event = Js_of_ocaml_lwt.Lwt_js_events.click btn in
    !pause_button_handler ();
    listen_for_clicks ()
  in
  
  Lwt.async (fun () -> listen_for_clicks ());

  pause_button := Some btn;
  Dom.appendChild parent btn

let register_pause_button_handler handler =
  pause_button_handler := handler

let update_pause_button_state is_paused =
  match !pause_button with
  | Some btn -> 
      let button_text = if is_paused then "Resume" else "Pause" in
      let button_color = if is_paused then "#4CAF50" else "#FF9800" in 
      btn##.textContent := Js.some (Js.string button_text);
      btn##.style##.backgroundColor := Js.string button_color
  | None -> ()

let create_speed_buttons doc parent =
  let speed_container_tyxml = div 
    ~a:[
      a_style "display: flex; flex-direction: column; margin: 0 15px;"
    ]
    [
      div ~a:[a_style "color: white; margin-top: 10px; margin-bottom: 5px; text-align: center; font-weight: bold;"] [txt "🚀 Speed"];
      div ~a:[a_style "display: flex; justify-content: center;"] [
        button ~a:[
          a_id "speed-minus-button";
          a_class ["speed-button"];
          a_style "padding: 8px 12px; font-size: 18px; font-weight: bold; background-color: #E57373; color: white; border: none; border-radius: 50%; cursor: pointer; margin: 0 5px; width: 40px; height: 40px; box-shadow: 0 4px 8px rgba(0,0,0,0.2); transition: all 0.2s ease-in-out; z-index: 1000;"
        ] [txt "-"];
        button ~a:[
          a_id "speed-plus-button";
          a_class ["speed-button"];
          a_style "padding: 8px 12px; font-size: 18px; font-weight: bold; background-color: #81C784; color: white; border: none; border-radius: 50%; cursor: pointer; margin: 0 5px; width: 40px; height: 40px; box-shadow: 0 4px 8px rgba(0,0,0,0.2); transition: all 0.2s ease-in-out; z-index: 1000;"
        ] [txt "+"]
      ]
    ] in
  
  let speed_container = extract_element doc speed_container_tyxml "div" in
  let minus_btn = Js.Opt.get 
    (speed_container##querySelector (Js.string "#speed-minus-button"))
    (fun () -> failwith "Speed minus button not found") in
  let plus_btn = Js.Opt.get 
    (speed_container##querySelector (Js.string "#speed-plus-button"))
    (fun () -> failwith "Speed plus button not found") in
  
  let minus_btn = Js.Unsafe.coerce minus_btn in
  let plus_btn = Js.Unsafe.coerce plus_btn in
  
  let rec listen_for_plus_clicks () =
    let* _event = Js_of_ocaml_lwt.Lwt_js_events.click plus_btn in
    !speed_plus_handler ();
    listen_for_plus_clicks ()
  in
  
  let rec listen_for_minus_clicks () =
    let* _event = Js_of_ocaml_lwt.Lwt_js_events.click minus_btn in
    !speed_minus_handler ();
    listen_for_minus_clicks ()
  in
  
  Lwt.async (fun () -> listen_for_plus_clicks ());
  Lwt.async (fun () -> listen_for_minus_clicks ());
  
  Dom.appendChild parent speed_container

let register_speed_plus_handler handler =
  speed_plus_handler := handler
let register_speed_minus_handler handler =
  speed_minus_handler := handler
let register_spawn_plus_handler handler =
  spawn_plus_handler := handler
let register_spawn_minus_handler handler =
  spawn_minus_handler := handler

let create_spawn_buttons doc parent =
  let spawn_container_tyxml = div 
    ~a:[
      a_style "display: flex; flex-direction: column; margin: 0 15px;"
    ]
    [
      div ~a:[a_style "color: white; margin-top: 10px; margin-bottom: 5px; text-align: center; font-weight: bold;"] [txt "⚡ Spawn Rate"];
      div ~a:[a_style "display: flex; justify-content: center;"] [
        button ~a:[
          a_id "spawn-minus-button";
          a_class ["spawn-button"];
          a_style "padding: 8px 12px; font-size: 18px; font-weight: bold; background-color: #E57373; color: white; border: none; border-radius: 50%; cursor: pointer; margin: 0 5px; width: 40px; height: 40px; box-shadow: 0 4px 8px rgba(0,0,0,0.2); transition: all 0.2s ease-in-out; z-index: 1000;"
        ] [txt "-"];
        button ~a:[
          a_id "spawn-plus-button";
          a_class ["spawn-button"];
          a_style "padding: 8px 12px; font-size: 18px; font-weight: bold; background-color: #81C784; color: white; border: none; border-radius: 50%; cursor: pointer; margin: 0 5px; width: 40px; height: 40px; box-shadow: 0 4px 8px rgba(0,0,0,0.2); transition: all 0.2s ease-in-out; z-index: 1000;"
        ] [txt "+"]
      ]
    ] in
  
  let spawn_container = extract_element doc spawn_container_tyxml "div" in
  let minus_btn = Js.Opt.get 
    (spawn_container##querySelector (Js.string "#spawn-minus-button"))
    (fun () -> failwith "Spawn minus button not found") in
  let plus_btn = Js.Opt.get 
    (spawn_container##querySelector (Js.string "#spawn-plus-button"))
    (fun () -> failwith "Spawn plus button not found") in
  
  let minus_btn = Js.Unsafe.coerce minus_btn in
  let plus_btn = Js.Unsafe.coerce plus_btn in
  
  let rec listen_for_plus_clicks () =
    let* _event = Js_of_ocaml_lwt.Lwt_js_events.click plus_btn in
    !spawn_plus_handler ();
    listen_for_plus_clicks ()
  in
  
  let rec listen_for_minus_clicks () =
    let* _event = Js_of_ocaml_lwt.Lwt_js_events.click minus_btn in
    !spawn_minus_handler ();
    listen_for_minus_clicks ()
  in
  
  Lwt.async (fun () -> listen_for_plus_clicks ());
  Lwt.async (fun () -> listen_for_minus_clicks ());
  
  Dom.appendChild parent spawn_container

let create_mean_creet_button doc parent =
  let button_tyxml = button 
    ~a:[
      a_id "mean-creet-button";
      a_class ["creet-button"];
      a_style "padding: 10px 15px; font-size: 16px; background-color: #F44336; color: white; border: none; border-radius: 5px; cursor: pointer; margin: 0 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.2); z-index: 1000;"
    ]
    [txt "Spawn Mean Creet"] in
  
  let btn = extract_element doc button_tyxml "button" in
  
  let rec listen_for_clicks () =
    let* _event = Js_of_ocaml_lwt.Lwt_js_events.click btn in
    !mean_creet_handler ();
    listen_for_clicks ()
  in
  
  Lwt.async (fun () -> listen_for_clicks ());
  
  mean_creet_button := Some btn;
  Dom.appendChild parent btn

let create_berserker_creet_button doc parent =
  let button_tyxml = button 
    ~a:[
      a_id "berserker-creet-button";
      a_class ["creet-button"];
      a_style "padding: 10px 15px; font-size: 16px; background-color: #9C27B0; color: white; border: none; border-radius: 5px; cursor: pointer; margin: 0 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.2); z-index: 1000;"
    ]
    [txt "Spawn Berserker Creet"] in
  
  let btn = extract_element doc button_tyxml "button" in
  
  let rec listen_for_clicks () =
    let* _event = Js_of_ocaml_lwt.Lwt_js_events.click btn in
    !berserker_creet_handler ();
    listen_for_clicks ()
  in
  
  Lwt.async (fun () -> listen_for_clicks ());

  berserker_creet_button := Some btn;
  Dom.appendChild parent btn

let create_healthy_creet_button doc parent =
  let button_tyxml = button 
    ~a:[
      a_id "healthy-creet-button";
      a_class ["creet-button"];
      a_style "padding: 10px 15px; font-size: 16px; background-color: #4CAF50; color: white; border: none; border-radius: 5px; cursor: pointer; margin: 0 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.2); z-index: 1000;"
    ]
    [txt "Spawn Healthy Creet"] in
  
  let btn = extract_element doc button_tyxml "button" in
  
  let rec listen_for_clicks () =
    let* _event = Js_of_ocaml_lwt.Lwt_js_events.click btn in
    !healthy_creet_handler ();
    listen_for_clicks ()
  in

  Lwt.async (fun () -> listen_for_clicks ());
  
  healthy_creet_button := Some btn;
  Dom.appendChild parent btn

let register_mean_creet_handler handler =
  mean_creet_handler := handler
let register_berserker_creet_handler handler =
  berserker_creet_handler := handler
let register_healthy_creet_handler handler =
  healthy_creet_handler := handler

let init_ui doc game_div =
  let left_controls_tyxml = div 
    ~a:[
      a_id "left-controls";
      a_style "position: absolute; top: 30%; left: 15%; width: 200px; padding: 20px; border-radius: 5px; display: flex; flex-direction: column; background-color: rgb(78, 78, 78); gap: 16px;"
    ]
    [] in
  
  let left_controls = extract_element doc left_controls_tyxml "div" in
  ui_container := Some left_controls;
  Dom.appendChild doc##.body left_controls;
  
  let timer_container_tyxml = div 
    ~a:[
      a_id "timer-container";
      a_style "display: flex; justify-content: center; align-items: center; width: 100%; position: fixed; top: 5%; z-index: 2000; padding: 8px 0;"
    ]
    [
      div ~a:[
        a_id "timer";
        a_class ["timer"];
        a_style "color: rgb(253, 122, 122); font-size: 30px; font-weight: bold; padding: 5px 15px; z-index: 1000; background-color: rgb(78, 78, 78); border-radius: 5px;"
      ] [txt "00:00"]
    ] in
  
  let timer_container = extract_element doc timer_container_tyxml "div" in
  let timer = Js.Opt.get 
    (timer_container##querySelector (Js.string "#timer"))
    (fun () -> failwith "Timer not found") in
  let timer = Js.Unsafe.coerce timer in
  
  timer_div := Some timer;
  Dom.appendChild doc##.body timer_container;
  
  create_pause_button doc left_controls;
  create_speed_buttons doc left_controls;
  create_spawn_buttons doc left_controls;
  create_mean_creet_button doc left_controls;
  create_berserker_creet_button doc left_controls;
  create_healthy_creet_button doc left_controls;
  
  let spell_container_tyxml = div 
    ~a:[
      a_style "position: absolute; top: 85%; left: 50%; transform: translateX(-50%); z-index: 1000;"
    ]
    [] in
  
  let spell_container = extract_element doc spell_container_tyxml "div" in
  Dom.appendChild game_div spell_container;
  create_spell_button doc spell_container

let register_spell_button_handler handler =
  spell_button_handler := handler

let update_timer elapsed =
  match !timer_div with
  | Some div ->
    let minutes = int_of_float (elapsed /. 60.) in
    let seconds = int_of_float (elapsed -. (float_of_int minutes *. 60.)) in
    div##.textContent := Js.some (Js.string (Printf.sprintf "%02d:%02d" minutes seconds))
  | None -> ()