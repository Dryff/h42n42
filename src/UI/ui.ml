open Js_of_ocaml
module Html = Dom_html

(* References to UI elements *)
let timer_div = ref None
let spell_button = ref None
let spell_button_handler = ref (fun () -> ())
let pause_button = ref None
let pause_button_handler = ref (fun () -> ())
let ui_container = ref None
let parameters_container = ref None
let speed_minus_button = ref None
let speed_plus_button = ref None
let speed_minus_handler = ref (fun () -> ())
let speed_plus_handler = ref (fun () -> ())

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
  (* Corrected boxShadow property *)
  ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "boxShadow" (Js.string "0 4px 8px rgba(0,0,0,0.2)"));
  (* Add zIndex *)
  btn##.style##.zIndex := Js.string "1000";
  
  (* Add event listener *)
  btn##.onclick := Html.handler (fun _ -> 
    !spell_button_handler ();
    Js._false
  );
  
  spell_button := Some btn;
  Dom.appendChild parent btn

(* Create the pause button UI element *)
let create_pause_button doc parent =
  let btn = Html.createButton doc in
  btn##.id := Js.string "pause-button";
  btn##.className := Js.string "pause-button";
  btn##.textContent := Js.some (Js.string "Pause");
  
  (* Style the button *)
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
  btn##.onclick := Html.handler (fun _ -> 
    !pause_button_handler ();
    Js._false
  );
  
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

(* Create a parameter adjustment button pair with + and - buttons *)
let create_parameter_button_pair doc parent label =
  (* Create container for the label and buttons *)
  let param_container = Html.createDiv doc in
  param_container##.style##.display := Js.string "flex";
  ignore (Js.Unsafe.set (Js.Unsafe.get param_container##.style) "flexDirection" (Js.string "column"));
  param_container##.style##.width := Js.string "100%";
  param_container##.style##.marginBottom := Js.string "10px";
  
  (* Add label *)
  let param_label = Html.createDiv doc in
  param_label##.textContent := Js.some (Js.string label);
  param_label##.style##.color := Js.string "white";
  param_label##.style##.marginBottom := Js.string "5px";
  Dom.appendChild param_container param_label;
  
  (* Create button row container *)
  let button_row = Html.createDiv doc in
  button_row##.style##.display := Js.string "flex";
  ignore (Js.Unsafe.set (Js.Unsafe.get button_row##.style) "justifyContent" (Js.string "space-between"));
  Dom.appendChild param_container button_row;
  
  (* Create - button *)
  let minus_btn = Html.createButton doc in
  minus_btn##.className := Js.string "param-button minus-btn";
  minus_btn##.textContent := Js.some (Js.string "-");
  
  (* Style the - button *)
  minus_btn##.style##.padding := Js.string "6px 12px";
  minus_btn##.style##.fontSize := Js.string "14px";
  minus_btn##.style##.backgroundColor := Js.string "#E57373"; (* Light red *)
  minus_btn##.style##.color := Js.string "white";
  minus_btn##.style##.border := Js.string "none";
  minus_btn##.style##.borderRadius := Js.string "5px";
  minus_btn##.style##.cursor := Js.string "pointer";
  minus_btn##.style##.width := Js.string "45%";
  ignore (Js.Unsafe.set (Js.Unsafe.get minus_btn##.style) "boxShadow" (Js.string "0 2px 4px rgba(0,0,0,0.2)"));
  
  (* Create + button *)
  let plus_btn = Html.createButton doc in
  plus_btn##.className := Js.string "param-button plus-btn";
  plus_btn##.textContent := Js.some (Js.string "+");
  
  (* Style the + button *)
  plus_btn##.style##.padding := Js.string "6px 12px";
  plus_btn##.style##.fontSize := Js.string "14px";
  plus_btn##.style##.backgroundColor := Js.string "#81C784"; (* Light green *)
  plus_btn##.style##.color := Js.string "white";
  plus_btn##.style##.border := Js.string "none";
  plus_btn##.style##.borderRadius := Js.string "5px";
  plus_btn##.style##.cursor := Js.string "pointer";
  plus_btn##.style##.width := Js.string "45%";
  ignore (Js.Unsafe.set (Js.Unsafe.get plus_btn##.style) "boxShadow" (Js.string "0 2px 4px rgba(0,0,0,0.2)"));
  
  (* Add buttons to the row *)
  Dom.appendChild button_row minus_btn;
  Dom.appendChild button_row plus_btn;
  
  (* Add the complete parameter container to parent *)
  Dom.appendChild parent param_container;
  
  (* Return both buttons for event handling *)
  (minus_btn, plus_btn)

(* Create the speed control buttons next to the other buttons *)
let create_speed_buttons doc parent =
  (* Create + button *)
  let plus_btn = Html.createButton doc in
  plus_btn##.id := Js.string "speed-plus-button";
  plus_btn##.className := Js.string "speed-button";
  plus_btn##.textContent := Js.some (Js.string "+");
  
  (* Style the + button *)
  plus_btn##.style##.padding := Js.string "10px 15px";
  plus_btn##.style##.fontSize := Js.string "16px";
  plus_btn##.style##.fontWeight := Js.string "bold";
  plus_btn##.style##.backgroundColor := Js.string "#81C784"; (* Light green *)
  plus_btn##.style##.color := Js.string "white";
  plus_btn##.style##.border := Js.string "none";
  plus_btn##.style##.borderRadius := Js.string "5px";
  plus_btn##.style##.cursor := Js.string "pointer";
  plus_btn##.style##.margin := Js.string "0 10px";
  ignore (Js.Unsafe.set (Js.Unsafe.get plus_btn##.style) "boxShadow" (Js.string "0 4px 8px rgba(0,0,0,0.2)"));
  ignore (Js.Unsafe.set (Js.Unsafe.get plus_btn##.style) "zIndex" (Js.string "1000"));
  
  (* Create - button *)
  let minus_btn = Html.createButton doc in
  minus_btn##.id := Js.string "speed-minus-button";
  minus_btn##.className := Js.string "speed-button";
  minus_btn##.textContent := Js.some (Js.string "-");
  
  (* Style the - button *)
  minus_btn##.style##.padding := Js.string "10px 15px";
  minus_btn##.style##.fontSize := Js.string "16px";
  minus_btn##.style##.fontWeight := Js.string "bold";
  minus_btn##.style##.backgroundColor := Js.string "#E57373"; (* Light red *)
  minus_btn##.style##.color := Js.string "white";
  minus_btn##.style##.border := Js.string "none";
  minus_btn##.style##.borderRadius := Js.string "5px";
  minus_btn##.style##.cursor := Js.string "pointer";
  minus_btn##.style##.margin := Js.string "0 10px";
  ignore (Js.Unsafe.set (Js.Unsafe.get minus_btn##.style) "boxShadow" (Js.string "0 4px 8px rgba(0,0,0,0.2)"));
  ignore (Js.Unsafe.set (Js.Unsafe.get minus_btn##.style) "zIndex" (Js.string "1000"));
  
  (* Add event listeners *)
  plus_btn##.onclick := Html.handler (fun _ -> 
    !speed_plus_handler ();
    Js._false
  );
  
  minus_btn##.onclick := Html.handler (fun _ -> 
    !speed_minus_handler ();
    Js._false
  );
  
  (* Store references *)
  speed_plus_button := Some plus_btn;
  speed_minus_button := Some minus_btn;
  
  (* Add buttons to parent container *)
  Dom.appendChild parent minus_btn;
  Dom.appendChild parent plus_btn

(* Register handlers for speed control buttons *)
let register_speed_plus_handler handler =
  speed_plus_handler := handler

let register_speed_minus_handler handler =
  speed_minus_handler := handler

(* Initialize the UI elements *)
let init_ui doc game_div canvas_height =
  (* Create UI container div *)
  let container = Html.createDiv doc in
  container##.id := Js.string "ui-container";
  container##.style##.display := Js.string "flex";
  (* Use Js.Unsafe.set for CSS properties that aren't directly supported *)
  ignore (Js.Unsafe.set (Js.Unsafe.get container##.style) "justifyContent" (Js.string "center"));
  ignore (Js.Unsafe.set (Js.Unsafe.get container##.style) "alignItems" (Js.string "center"));
  container##.style##.marginTop := Js.string "15px";
  container##.style##.width := Js.string "100%";
  container##.style##.position := Js.string "relative";
  container##.style##.top := Js.string (string_of_int (canvas_height + 40) ^ "px");
  ui_container := Some container;
  Dom.appendChild game_div container;
  
  (* Create timer div *)
  let timer = Html.createDiv doc in
  timer##.id := Js.string "timer";
  timer##.className := Js.string "timer";
  timer##.textContent := Js.some (Js.string "Time: 00:00");
  timer##.style##.color := Js.string "white";
  timer##.style##.fontSize := Js.string "20px";
  timer##.style##.margin := Js.string "0 15px";
  timer_div := Some timer;
  Dom.appendChild container timer;
  
  (* Create speed control buttons first (on the left side) *)
  create_speed_buttons doc container;
  
  (* Create spell button *)
  create_spell_button doc container;  
  
  (* Create pause button *)
  create_pause_button doc container

(* Register a handler for the spell button *)
let register_spell_button_handler handler =
  spell_button_handler := handler

(* Show game over message *)
let show_game_over () =
  match !timer_div with
  | Some div -> div##.textContent := Js.some (Js.string "GAME OVER")
  | None -> ()

(* Update the timer display *)
let update_timer elapsed =
  match !timer_div with
  | Some div ->
    let minutes = int_of_float (elapsed /. 60.) in
    let seconds = int_of_float (elapsed -. (float_of_int minutes *. 60.)) in
    div##.textContent := Js.some (Js.string (Printf.sprintf "Time: %02d:%02d" minutes seconds))
  | None -> ()

(* Create parameters UI section *)
let create_parameters_ui doc parent =
  (* Create parameters container *)
  let container = Html.createDiv doc in
  container##.id := Js.string "parameters-container";
  container##.style##.position := Js.string "absolute";
  container##.style##.right := Js.string "20px";
  container##.style##.top := Js.string "10px";
  container##.style##.width := Js.string "150px";
  container##.style##.padding := Js.string "15px";
  container##.style##.backgroundColor := Js.string "rgba(0, 0, 0, 0.6)";
  container##.style##.borderRadius := Js.string "5px";
  ignore (Js.Unsafe.set (Js.Unsafe.get container##.style) "boxShadow" (Js.string "0 4px 8px rgba(0,0,0,0.3)"));
  parameters_container := Some container;
  Dom.appendChild parent container;
  
  (* Create parameter controls *)
  let _ = create_parameter_button_pair doc container "Power" in
  let _ = create_parameter_button_pair doc container "Frequency" in
  let _ = create_parameter_button_pair doc container "Speed" in
  ()
