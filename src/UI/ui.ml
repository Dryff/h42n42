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

(* Create a parameter adjustment button *)
let create_parameter_button doc parent label =
  let btn = Html.createButton doc in
  btn##.className := Js.string "param-button";
  btn##.textContent := Js.some (Js.string label);
  
  (* Style the button *)
  btn##.style##.padding := Js.string "8px 12px";
  btn##.style##.fontSize := Js.string "14px";
  btn##.style##.backgroundColor := Js.string "#4A90E2"; (* Blue *)
  btn##.style##.color := Js.string "white";
  btn##.style##.border := Js.string "none";
  btn##.style##.borderRadius := Js.string "5px";
  btn##.style##.cursor := Js.string "pointer";
  btn##.style##.margin := Js.string "5px 0";
  btn##.style##.width := Js.string "100%";
  ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "boxShadow" (Js.string "0 2px 4px rgba(0,0,0,0.2)"));
  
  Dom.appendChild parent btn;
  btn

(* Create parameter adjustment UI section *)
let create_parameters_ui doc game_div =
  (* Create parameters container *)
  let container = Html.createDiv doc in
  container##.id := Js.string "parameters-container";
  container##.style##.position := Js.string "absolute";
  container##.style##.top := Js.string "0";
  container##.style##.right := Js.string "0";
  container##.style##.width := Js.string "200px";
  container##.style##.padding := Js.string "15px";
  container##.style##.backgroundColor := Js.string "rgba(0, 0, 0, 0.7)";
  container##.style##.borderLeft := Js.string "1px solid #444";
  container##.style##.height := Js.string "100%";
  (* Use Js.Unsafe.set for boxSizing property *)
  ignore (Js.Unsafe.set (Js.Unsafe.get container##.style) "boxSizing" (Js.string "border-box"));
  parameters_container := Some container;
  
  (* Create title *)
  let title = Html.createH3 doc in
  title##.textContent := Js.some (Js.string "Game Parameters");
  title##.style##.color := Js.string "white";
  title##.style##.marginTop := Js.string "0";
  title##.style##.marginBottom := Js.string "15px";
  title##.style##.textAlign := Js.string "center";
  Dom.appendChild container title;
  
  (* Create spawn rate adjustment buttons *)
  let spawn_section = Html.createDiv doc in
  spawn_section##.style##.marginBottom := Js.string "20px";
  Dom.appendChild container spawn_section;
  
  let spawn_label = Html.createDiv doc in
  spawn_label##.textContent := Js.some (Js.string "Spawn Rate:");
  spawn_label##.style##.color := Js.string "white";
  spawn_label##.style##.marginBottom := Js.string "5px";
  Dom.appendChild spawn_section spawn_label;
  
  ignore (create_parameter_button doc spawn_section "Increase Spawn Rate");
  ignore (create_parameter_button doc spawn_section "Decrease Spawn Rate");
  
  (* Create speed adjustment buttons *)
  let speed_section = Html.createDiv doc in
  speed_section##.style##.marginBottom := Js.string "20px";
  Dom.appendChild container speed_section;
  
  let speed_label = Html.createDiv doc in
  speed_label##.textContent := Js.some (Js.string "Creet Speed:");
  speed_label##.style##.color := Js.string "white";
  speed_label##.style##.marginBottom := Js.string "5px";
  Dom.appendChild speed_section speed_label;
  
  ignore (create_parameter_button doc speed_section "Increase Speed");
  ignore (create_parameter_button doc speed_section "Decrease Speed");
  
  (* Create creet type probability buttons *)
  let type_section = Html.createDiv doc in
  Dom.appendChild container type_section;
  
  let type_label = Html.createDiv doc in
  type_label##.textContent := Js.some (Js.string "Creet Types:");
  type_label##.style##.color := Js.string "white";
  type_label##.style##.marginBottom := Js.string "5px";
  Dom.appendChild type_section type_label;
  
  ignore (create_parameter_button doc type_section "More Mean Creets");
  ignore (create_parameter_button doc type_section "Less Mean Creets");
  ignore (create_parameter_button doc type_section "More Berserker Creets");
  ignore (create_parameter_button doc type_section "Less Berserker Creets");
  
  Dom.appendChild game_div container

(* Initialize the UI elements *)
let init_ui doc game_div =
  (* Create UI container div *)
  let container = Html.createDiv doc in
  container##.id := Js.string "ui-container";
  container##.style##.display := Js.string "flex";
  (* Use Js.Unsafe.set for CSS properties that aren't directly supported *)
  ignore (Js.Unsafe.set (Js.Unsafe.get container##.style) "justifyContent" (Js.string "center"));
  ignore (Js.Unsafe.set (Js.Unsafe.get container##.style) "alignItems" (Js.string "center"));
  container##.style##.marginTop := Js.string "15px";
  container##.style##.width := Js.string "100%";
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
  
  (* Create spell button *)
  create_spell_button doc container;  
  
  (* Create pause button *)
  create_pause_button doc container;
  
  (* Create parameters UI section *)
  create_parameters_ui doc game_div

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
