open Js_of_ocaml
open Lwt.Syntax
module Html = Dom_html

(* References to UI elements *)
let timer_div = ref None
let spell_button = ref None
let spell_button_handler = ref (fun () -> ())
let pause_button = ref None
let pause_button_handler = ref (fun () -> ())
let ui_container = ref None
let speed_minus_handler = ref (fun () -> ())
let speed_plus_handler = ref (fun () -> ())
let spawn_minus_handler = ref (fun () -> ())
let spawn_plus_handler = ref (fun () -> ())
let mean_creet_button = ref None
let mean_creet_handler = ref (fun () -> ())
let berserker_creet_button = ref None
let berserker_creet_handler = ref (fun () -> ())
let healthy_creet_button = ref None
let healthy_creet_handler = ref (fun () -> ())

(* Create the spell button UI element *)
let create_spell_button doc parent =
  let btn = Html.createButton doc in
  btn##.id := Js.string "spell-button";
  btn##.className := Js.string "spell-button";
  btn##.textContent := Js.some (Js.string "Cast Spell");
  
  (* Style the button *)
  btn##.style##.padding := Js.string "10px 15px";
  btn##.style##.fontSize := Js.string "16px";
  btn##.style##.backgroundColor := Js.string "#7B68EE"; (* Medium slate blue *)
  btn##.style##.color := Js.string "white";
  btn##.style##.border := Js.string "none";
  btn##.style##.borderRadius := Js.string "5px";
  btn##.style##.cursor := Js.string "pointer";
  btn##.style##.margin := Js.string "0 10px";
  ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "boxShadow" (Js.string "0 4px 8px rgba(0,0,0,0.2)"));
  btn##.style##.zIndex := Js.string "1000";
  
  let rec listen_for_clicks () =
    let* _event = Js_of_ocaml_lwt.Lwt_js_events.click btn in
    !spell_button_handler ();
    listen_for_clicks ()
  in
  
  Lwt.async (fun () -> listen_for_clicks ());
  
  spell_button := Some btn;
  Dom.appendChild parent btn

let create_pause_button doc parent =
  let btn = Html.createButton doc in
  btn##.id := Js.string "pause-button";
  btn##.className := Js.string "pause-button";
  btn##.textContent := Js.some (Js.string "Pause");
  btn##.style##.padding := Js.string "10px 15px";
  btn##.style##.fontSize := Js.string "16px";
  btn##.style##.backgroundColor := Js.string "#FF9800"; (* Orange *)
  btn##.style##.color := Js.string "white";
  btn##.style##.border := Js.string "none";
  btn##.style##.borderRadius := Js.string "5px";
  btn##.style##.cursor := Js.string "pointer";
  btn##.style##.margin := Js.string "0 10px";
  ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "boxShadow" (Js.string "0 4px 8px rgba(0,0,0,0.2)"));
  btn##.style##.zIndex := Js.string "1000";
  
  (* Add event listener *)
  let rec listen_for_clicks () =
    let* _event = Js_of_ocaml_lwt.Lwt_js_events.click btn in
    !pause_button_handler ();
    listen_for_clicks ()
  in
  
  Lwt.async (fun () -> listen_for_clicks ());

  pause_button := Some btn;
  Dom.appendChild parent btn

(* Register a handler for the pause button *)
let register_pause_button_handler handler =
  pause_button_handler := handler

(* Update the pause button text and style based on game paused state *)
let update_pause_button_state is_paused =
  match !pause_button with
  | Some btn -> 
      let button_text = if is_paused then "Resume" else "Pause" in
      let button_color = if is_paused then "#4CAF50" else "#FF9800" in (* Green when paused, Orange when playing *)
      btn##.textContent := Js.some (Js.string button_text);
      btn##.style##.backgroundColor := Js.string button_color
  | None -> ()

let create_speed_buttons doc parent =
  let speed_container = Html.createDiv doc in
  speed_container##.style##.display := Js.string "flex";
  ignore (Js.Unsafe.set (Js.Unsafe.get speed_container##.style) "flexDirection" (Js.string "column"));
  speed_container##.style##.margin := Js.string "0 15px";
  
  let speed_label = Html.createDiv doc in
  speed_label##.textContent := Js.some (Js.string "🚀 Speed");
  speed_label##.style##.color := Js.string "white";
  speed_label##.style##.marginTop := Js.string "10px";
  speed_label##.style##.marginBottom := Js.string "5px";
  speed_label##.style##.textAlign := Js.string "center";
  speed_label##.style##.fontWeight := Js.string "bold";
  Dom.appendChild speed_container speed_label;
  
  let button_row = Html.createDiv doc in
  button_row##.style##.display := Js.string "flex";
  ignore (Js.Unsafe.set (Js.Unsafe.get button_row##.style) "justifyContent" (Js.string "center"));
  Dom.appendChild speed_container button_row;
  
  (* Create +- button *)
  let minus_btn = Html.createButton doc in
  minus_btn##.id := Js.string "speed-minus-button";
  minus_btn##.className := Js.string "speed-button";
  minus_btn##.textContent := Js.some (Js.string "-");
  minus_btn##.style##.padding := Js.string "8px 12px";
  minus_btn##.style##.fontSize := Js.string "18px";
  minus_btn##.style##.fontWeight := Js.string "bold";
  minus_btn##.style##.backgroundColor := Js.string "#E57373"; (* Light red *)
  minus_btn##.style##.color := Js.string "white";
  minus_btn##.style##.border := Js.string "none";
  minus_btn##.style##.borderRadius := Js.string "50%"; (* Make it circular *)
  minus_btn##.style##.cursor := Js.string "pointer";
  minus_btn##.style##.margin := Js.string "0 5px";
  minus_btn##.style##.width := Js.string "40px";
  minus_btn##.style##.height := Js.string "40px";
  ignore (Js.Unsafe.set (Js.Unsafe.get minus_btn##.style) "boxShadow" (Js.string "0 4px 8px rgba(0,0,0,0.2)"));
  ignore (Js.Unsafe.set (Js.Unsafe.get minus_btn##.style) "transition" (Js.string "all 0.2s ease-in-out"));
  ignore (Js.Unsafe.set (Js.Unsafe.get minus_btn##.style) "zIndex" (Js.string "1000"));
  
  let plus_btn = Html.createButton doc in
  plus_btn##.id := Js.string "speed-plus-button";
  plus_btn##.className := Js.string "speed-button";
  plus_btn##.textContent := Js.some (Js.string "+");
  plus_btn##.style##.padding := Js.string "8px 12px";
  plus_btn##.style##.fontSize := Js.string "18px";
  plus_btn##.style##.fontWeight := Js.string "bold";
  plus_btn##.style##.backgroundColor := Js.string "#81C784"; (* Light green *)
  plus_btn##.style##.color := Js.string "white";
  plus_btn##.style##.border := Js.string "none";
  plus_btn##.style##.borderRadius := Js.string "50%"; (* Make it circular *)
  plus_btn##.style##.cursor := Js.string "pointer";
  plus_btn##.style##.margin := Js.string "0 5px";
  plus_btn##.style##.width := Js.string "40px";
  plus_btn##.style##.height := Js.string "40px";
  ignore (Js.Unsafe.set (Js.Unsafe.get plus_btn##.style) "boxShadow" (Js.string "0 4px 8px rgba(0,0,0,0.2)"));
  ignore (Js.Unsafe.set (Js.Unsafe.get plus_btn##.style) "transition" (Js.string "all 0.2s ease-in-out"));
  ignore (Js.Unsafe.set (Js.Unsafe.get plus_btn##.style) "zIndex" (Js.string "1000"));
  
  (* Add event listeners using Lwt *)
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
  
  Dom.appendChild button_row minus_btn;
  Dom.appendChild button_row plus_btn;
  Dom.appendChild parent speed_container

let register_speed_plus_handler handler =
  speed_plus_handler := handler
let register_speed_minus_handler handler =
  speed_minus_handler := handler
let register_spawn_plus_handler handler =
  spawn_plus_handler := handler
let register_spawn_minus_handler handler =
  spawn_minus_handler := handler

(* Create the spawn rate control buttons *)
let create_spawn_buttons doc parent =
  let spawn_container = Html.createDiv doc in
  spawn_container##.style##.display := Js.string "flex";
  ignore (Js.Unsafe.set (Js.Unsafe.get spawn_container##.style) "flexDirection" (Js.string "column"));
  spawn_container##.style##.margin := Js.string "0 15px";
  let spawn_label = Html.createDiv doc in
  spawn_label##.textContent := Js.some (Js.string "⚡ Spawn Rate");
  spawn_label##.style##.color := Js.string "white";
  spawn_label##.style##.marginTop := Js.string "10px";
  spawn_label##.style##.marginBottom := Js.string "5px";
  spawn_label##.style##.textAlign := Js.string "center";
  spawn_label##.style##.fontWeight := Js.string "bold";
  Dom.appendChild spawn_container spawn_label;
  let button_row = Html.createDiv doc in
  button_row##.style##.display := Js.string "flex";
  ignore (Js.Unsafe.set (Js.Unsafe.get button_row##.style) "justifyContent" (Js.string "center"));
  Dom.appendChild spawn_container button_row;
  
  (* Create +- button *)
  let minus_btn = Html.createButton doc in
  minus_btn##.id := Js.string "spawn-minus-button";
  minus_btn##.className := Js.string "spawn-button";
  minus_btn##.textContent := Js.some (Js.string "-");
  minus_btn##.style##.padding := Js.string "8px 12px";
  minus_btn##.style##.fontSize := Js.string "18px";
  minus_btn##.style##.fontWeight := Js.string "bold";
  minus_btn##.style##.backgroundColor := Js.string "#E57373"; (* Light red *)
  minus_btn##.style##.color := Js.string "white";
  minus_btn##.style##.border := Js.string "none";
  minus_btn##.style##.borderRadius := Js.string "50%"; (* Make it circular *)
  minus_btn##.style##.cursor := Js.string "pointer";
  minus_btn##.style##.margin := Js.string "0 5px";
  minus_btn##.style##.width := Js.string "40px";
  minus_btn##.style##.height := Js.string "40px";
  ignore (Js.Unsafe.set (Js.Unsafe.get minus_btn##.style) "boxShadow" (Js.string "0 4px 8px rgba(0,0,0,0.2)"));
  ignore (Js.Unsafe.set (Js.Unsafe.get minus_btn##.style) "transition" (Js.string "all 0.2s ease-in-out"));
  ignore (Js.Unsafe.set (Js.Unsafe.get minus_btn##.style) "zIndex" (Js.string "1000"));
  
  let plus_btn = Html.createButton doc in
  plus_btn##.id := Js.string "spawn-plus-button";
  plus_btn##.className := Js.string "spawn-button";
  plus_btn##.textContent := Js.some (Js.string "+");
  plus_btn##.style##.padding := Js.string "8px 12px";
  plus_btn##.style##.fontSize := Js.string "18px";
  plus_btn##.style##.fontWeight := Js.string "bold";
  plus_btn##.style##.backgroundColor := Js.string "#81C784"; (* Light green *)
  plus_btn##.style##.color := Js.string "white";
  plus_btn##.style##.border := Js.string "none";
  plus_btn##.style##.borderRadius := Js.string "50%"; (* Make it circular *)
  plus_btn##.style##.cursor := Js.string "pointer";
  plus_btn##.style##.margin := Js.string "0 5px";
  plus_btn##.style##.width := Js.string "40px";
  plus_btn##.style##.height := Js.string "40px";
  ignore (Js.Unsafe.set (Js.Unsafe.get plus_btn##.style) "boxShadow" (Js.string "0 4px 8px rgba(0,0,0,0.2)"));
  ignore (Js.Unsafe.set (Js.Unsafe.get plus_btn##.style) "transition" (Js.string "all 0.2s ease-in-out"));
  ignore (Js.Unsafe.set (Js.Unsafe.get plus_btn##.style) "zIndex" (Js.string "1000"));
  
  (* Add event listeners using Lwt *)
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
  
  Dom.appendChild button_row minus_btn;
  Dom.appendChild button_row plus_btn;
  Dom.appendChild parent spawn_container

(* Create the mean creet spawn button *)
let create_mean_creet_button doc parent =
  let btn = Html.createButton doc in
  btn##.id := Js.string "mean-creet-button";
  btn##.className := Js.string "creet-button";
  btn##.textContent := Js.some (Js.string "Spawn Mean Creet");
  btn##.style##.padding := Js.string "10px 15px";
  btn##.style##.fontSize := Js.string "16px";
  btn##.style##.backgroundColor := Js.string "#F44336"; (* Red *)
  btn##.style##.color := Js.string "white";
  btn##.style##.border := Js.string "none";
  btn##.style##.borderRadius := Js.string "5px";
  btn##.style##.cursor := Js.string "pointer";
  btn##.style##.margin := Js.string "0 10px";
  ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "boxShadow" (Js.string "0 4px 8px rgba(0,0,0,0.2)"));
  btn##.style##.zIndex := Js.string "1000";
  
  (* Add event listener *)
  let rec listen_for_clicks () =
    let* _event = Js_of_ocaml_lwt.Lwt_js_events.click btn in
    !mean_creet_handler ();
    listen_for_clicks ()
  in
  
  Lwt.async (fun () -> listen_for_clicks ());
  
  mean_creet_button := Some btn;
  Dom.appendChild parent btn

(* Create the berserker creet spawn button *)
let create_berserker_creet_button doc parent =
  let btn = Html.createButton doc in
  btn##.id := Js.string "berserker-creet-button";
  btn##.className := Js.string "creet-button";
  btn##.textContent := Js.some (Js.string "Spawn Berserker Creet");
  btn##.style##.padding := Js.string "10px 15px";
  btn##.style##.fontSize := Js.string "16px";
  btn##.style##.backgroundColor := Js.string "#9C27B0"; (* Purple *)
  btn##.style##.color := Js.string "white";
  btn##.style##.border := Js.string "none";
  btn##.style##.borderRadius := Js.string "5px";
  btn##.style##.cursor := Js.string "pointer";
  btn##.style##.margin := Js.string "0 10px";
  ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "boxShadow" (Js.string "0 4px 8px rgba(0,0,0,0.2)"));
  btn##.style##.zIndex := Js.string "1000";
  
  (* Add event listener *)
  let rec listen_for_clicks () =
    let* _event = Js_of_ocaml_lwt.Lwt_js_events.click btn in
    !berserker_creet_handler ();
    listen_for_clicks ()
  in
  
  Lwt.async (fun () -> listen_for_clicks ());

  berserker_creet_button := Some btn;
  Dom.appendChild parent btn

(* Create the healthy creet spawn button *)
let create_healthy_creet_button doc parent =
  let btn = Html.createButton doc in
  btn##.id := Js.string "healthy-creet-button";
  btn##.className := Js.string "creet-button";
  btn##.textContent := Js.some (Js.string "Spawn Healthy Creet");
  btn##.style##.padding := Js.string "10px 15px";
  btn##.style##.fontSize := Js.string "16px";
  btn##.style##.backgroundColor := Js.string "#4CAF50"; (* Green *)
  btn##.style##.color := Js.string "white";
  btn##.style##.border := Js.string "none";
  btn##.style##.borderRadius := Js.string "5px";
  btn##.style##.cursor := Js.string "pointer";
  btn##.style##.margin := Js.string "0 10px";
  ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "boxShadow" (Js.string "0 4px 8px rgba(0,0,0,0.2)"));
  btn##.style##.zIndex := Js.string "1000";
  
  let rec listen_for_clicks () =
    let* _event = Js_of_ocaml_lwt.Lwt_js_events.click btn in
    !healthy_creet_handler ();
    listen_for_clicks ()
  in

  Lwt.async (fun () -> listen_for_clicks ());
  
  healthy_creet_button := Some btn;
  Dom.appendChild parent btn

(* Register handlers for creet spawn buttons *)
let register_mean_creet_handler handler =
  mean_creet_handler := handler
let register_berserker_creet_handler handler =
  berserker_creet_handler := handler
let register_healthy_creet_handler handler =
  healthy_creet_handler := handler

(* Initialize the UI elements *)
let init_ui doc game_div =
  let left_controls = Html.createDiv doc in
  left_controls##.id := Js.string "left-controls";
  left_controls##.style##.cssText := Js.string "display: flex; flex-direction: column; gap: 16px;";
  left_controls##.style##.cssText := Js.string
    "position: absolute; \
     top: 30%; \
     left: 15%; \
     width: 200px; \
     padding: 20px; \
     border-radius: 5px; \
     display: flex; \
     flex-direction: column; \
     background-color: rgb(78, 78, 78); \
     gap: 16px;";
  ui_container := Some left_controls;
  Dom.appendChild doc##.body left_controls;
  
  (* Create timer container & div *)
  let timer_container = Html.createDiv doc in
  timer_container##.id := Js.string "timer-container";
  timer_container##.style##.display := Js.string "flex";
  ignore (Js.Unsafe.set (Js.Unsafe.get timer_container##.style) "justifyContent" (Js.string "center"));
  ignore (Js.Unsafe.set (Js.Unsafe.get timer_container##.style) "alignItems" (Js.string "center"));
  timer_container##.style##.width := Js.string "100%";
  timer_container##.style##.position := Js.string "fixed";
  timer_container##.style##.top := Js.string "5%";
  timer_container##.style##.left := Js.string "47%";
  timer_container##.style##.zIndex := Js.string "2000";
  timer_container##.style##.padding := Js.string "8px 0";
  Dom.appendChild (doc##.body) timer_container;
  
  let timer = Html.createDiv doc in
  timer##.id := Js.string "timer";
  timer##.className := Js.string "timer";
  timer##.textContent := Js.some (Js.string "00:00");
  timer##.style##.color := Js.string "rgb(253, 122, 122)";
  timer##.style##.fontSize := Js.string "30px";
  timer##.style##.fontWeight := Js.string "bold";
  timer##.style##.padding := Js.string "5px 15px";
  timer##.style##.zIndex := Js.string "1000";
  timer##.style##.backgroundColor := Js.string "rgb(78, 78, 78)";
  timer##.style##.borderRadius := Js.string "5px";
  timer_div := Some timer;
  Dom.appendChild timer_container timer;
  
  create_pause_button doc left_controls;
  create_speed_buttons doc left_controls;
  create_spawn_buttons doc left_controls;
  create_mean_creet_button doc left_controls;
  create_berserker_creet_button doc left_controls;
  create_healthy_creet_button doc left_controls;
  
  (* Create spell button *)
  let spell_container = Html.createDiv doc in
  spell_container##.style##.cssText := Js.string (
    "position: absolute; \
    top: 85%; \
    left: 50%; \
    transform: translateX(-50%); \
    z-index: 1000;"
  );
  Dom.appendChild game_div spell_container;
  create_spell_button doc spell_container

let register_spell_button_handler handler =
  spell_button_handler := handler

(* Update the timer display *)
let update_timer elapsed =
  match !timer_div with
  | Some div ->
    let minutes = int_of_float (elapsed /. 60.) in
    let seconds = int_of_float (elapsed -. (float_of_int minutes *. 60.)) in
    div##.textContent := Js.some (Js.string (Printf.sprintf "%02d:%02d" minutes seconds))
  | None -> ()