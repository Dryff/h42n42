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
let spawn_minus_button = ref None
let spawn_plus_button = ref None
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
  minus_btn##.style##.width := Js.string "48%";
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
  (* Create container for speed controls *)
  let speed_container = Html.createDiv doc in
  speed_container##.style##.display := Js.string "flex";
  ignore (Js.Unsafe.set (Js.Unsafe.get speed_container##.style) "flexDirection" (Js.string "column"));
  speed_container##.style##.margin := Js.string "0 15px";
  
  (* Add label with icon *)
  let speed_label = Html.createDiv doc in
  speed_label##.textContent := Js.some (Js.string "🚀 Speed");
  speed_label##.style##.color := Js.string "white";
  speed_label##.style##.marginTop := Js.string "10px";
  speed_label##.style##.marginBottom := Js.string "5px";
  speed_label##.style##.textAlign := Js.string "center";
  speed_label##.style##.fontWeight := Js.string "bold";
  Dom.appendChild speed_container speed_label;
  
  (* Create button row container *)
  let button_row = Html.createDiv doc in
  button_row##.style##.display := Js.string "flex";
  ignore (Js.Unsafe.set (Js.Unsafe.get button_row##.style) "justifyContent" (Js.string "center"));
  Dom.appendChild speed_container button_row;
  
  (* Create - button *)
  let minus_btn = Html.createButton doc in
  minus_btn##.id := Js.string "speed-minus-button";
  minus_btn##.className := Js.string "speed-button";
  minus_btn##.textContent := Js.some (Js.string "-");
  
  (* Style the - button *)
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
  
  (* Create + button *)
  let plus_btn = Html.createButton doc in
  plus_btn##.id := Js.string "speed-plus-button";
  plus_btn##.className := Js.string "speed-button";
  plus_btn##.textContent := Js.some (Js.string "+");
  
  (* Style the + button *)
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
  
  (* Add hover effects *)
  let add_hover_effects btn base_color hover_color =
    btn##.onmouseover := Html.handler (fun _ -> 
      btn##.style##.backgroundColor := Js.string hover_color;
      ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "transform" (Js.string "scale(1.1)"));
      Js._false
    );
    btn##.onmouseout := Html.handler (fun _ -> 
      btn##.style##.backgroundColor := Js.string base_color;
      ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "transform" (Js.string "scale(1.0)"));
      Js._false
    )
  in
  
  add_hover_effects minus_btn "#E57373" "#EF5350"; (* Lighter to darker red *)
  add_hover_effects plus_btn "#81C784" "#66BB6A"; (* Lighter to darker green *)
  
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
  
  (* Add buttons to button row *)
  Dom.appendChild button_row minus_btn;
  Dom.appendChild button_row plus_btn;
  
  (* Add container to parent *)
  Dom.appendChild parent speed_container

(* Register handlers for speed control buttons *)
let register_speed_plus_handler handler =
  speed_plus_handler := handler

let register_speed_minus_handler handler =
  speed_minus_handler := handler

(* Register handlers for spawn rate control buttons *)
let register_spawn_plus_handler handler =
  spawn_plus_handler := handler

let register_spawn_minus_handler handler =
  spawn_minus_handler := handler

(* Create the spawn rate control buttons *)
let create_spawn_buttons doc parent =
  (* Create container for spawn rate controls *)
  let spawn_container = Html.createDiv doc in
  spawn_container##.style##.display := Js.string "flex";
  ignore (Js.Unsafe.set (Js.Unsafe.get spawn_container##.style) "flexDirection" (Js.string "column"));
  spawn_container##.style##.margin := Js.string "0 15px";
  
  (* Add label with icon *)
  let spawn_label = Html.createDiv doc in
  spawn_label##.textContent := Js.some (Js.string "⚡ Spawn Rate");
  spawn_label##.style##.color := Js.string "white";
  spawn_label##.style##.marginTop := Js.string "10px";
  spawn_label##.style##.marginBottom := Js.string "5px";
  spawn_label##.style##.textAlign := Js.string "center";
  spawn_label##.style##.fontWeight := Js.string "bold";
  Dom.appendChild spawn_container spawn_label;
  
  (* Create button row container *)
  let button_row = Html.createDiv doc in
  button_row##.style##.display := Js.string "flex";
  ignore (Js.Unsafe.set (Js.Unsafe.get button_row##.style) "justifyContent" (Js.string "center"));
  Dom.appendChild spawn_container button_row;
  
  (* Create - button *)
  let minus_btn = Html.createButton doc in
  minus_btn##.id := Js.string "spawn-minus-button";
  minus_btn##.className := Js.string "spawn-button";
  minus_btn##.textContent := Js.some (Js.string "-");
  
  (* Style the - button *)
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
  
  (* Create + button *)
  let plus_btn = Html.createButton doc in
  plus_btn##.id := Js.string "spawn-plus-button";
  plus_btn##.className := Js.string "spawn-button";
  plus_btn##.textContent := Js.some (Js.string "+");
  
  (* Style the + button *)
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
  
  (* Add hover effects *)
  let add_hover_effects btn base_color hover_color =
    btn##.onmouseover := Html.handler (fun _ -> 
      btn##.style##.backgroundColor := Js.string hover_color;
      ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "transform" (Js.string "scale(1.1)"));
      Js._false
    );
    btn##.onmouseout := Html.handler (fun _ -> 
      btn##.style##.backgroundColor := Js.string base_color;
      ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "transform" (Js.string "scale(1.0)"));
      Js._false
    )
  in
  
  add_hover_effects minus_btn "#E57373" "#EF5350"; (* Lighter to darker red *)
  add_hover_effects plus_btn "#81C784" "#66BB6A"; (* Lighter to darker green *)
  
  (* Add event listeners *)
  plus_btn##.onclick := Html.handler (fun _ -> 
    !spawn_plus_handler ();
    Js._false
  );
  
  minus_btn##.onclick := Html.handler (fun _ -> 
    !spawn_minus_handler ();
    Js._false
  );
  
  (* Store references *)
  spawn_plus_button := Some plus_btn;
  spawn_minus_button := Some minus_btn;
  
  (* Add buttons to button row *)
  Dom.appendChild button_row minus_btn;
  Dom.appendChild button_row plus_btn;
  
  (* Add container to parent *)
  Dom.appendChild parent spawn_container

(* Create the mean creet spawn button *)
let create_mean_creet_button doc parent =
  let btn = Html.createButton doc in
  btn##.id := Js.string "mean-creet-button";
  btn##.className := Js.string "creet-button";
  btn##.textContent := Js.some (Js.string "Spawn Mean Creet");
  
  (* Style the button *)
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
  
  (* Add hover effects *)
  btn##.onmouseover := Html.handler (fun _ -> 
    btn##.style##.backgroundColor := Js.string "#D32F2F"; (* Darker red *)
    ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "transform" (Js.string "scale(1.05)"));
    Js._false
  );
  btn##.onmouseout := Html.handler (fun _ -> 
    btn##.style##.backgroundColor := Js.string "#F44336"; (* Back to normal red *)
    ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "transform" (Js.string "scale(1.0)"));
    Js._false
  );
  
  (* Add event listener *)
  btn##.onclick := Html.handler (fun _ -> 
    !mean_creet_handler ();
    Js._false
  );
  
  mean_creet_button := Some btn;
  Dom.appendChild parent btn

(* Create the berserker creet spawn button *)
let create_berserker_creet_button doc parent =
  let btn = Html.createButton doc in
  btn##.id := Js.string "berserker-creet-button";
  btn##.className := Js.string "creet-button";
  btn##.textContent := Js.some (Js.string "Spawn Berserker Creet");
  
  (* Style the button *)
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
  
  (* Add hover effects *)
  btn##.onmouseover := Html.handler (fun _ -> 
    btn##.style##.backgroundColor := Js.string "#7B1FA2"; (* Darker purple *)
    ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "transform" (Js.string "scale(1.05)"));
    Js._false
  );
  btn##.onmouseout := Html.handler (fun _ -> 
    btn##.style##.backgroundColor := Js.string "#9C27B0"; (* Back to normal purple *)
    ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "transform" (Js.string "scale(1.0)"));
    Js._false
  );
  
  (* Add event listener *)
  btn##.onclick := Html.handler (fun _ -> 
    !berserker_creet_handler ();
    Js._false
  );
  
  berserker_creet_button := Some btn;
  Dom.appendChild parent btn

(* Create the healthy creet spawn button *)
let create_healthy_creet_button doc parent =
  let btn = Html.createButton doc in
  btn##.id := Js.string "healthy-creet-button";
  btn##.className := Js.string "creet-button";
  btn##.textContent := Js.some (Js.string "Spawn Healthy Creet");
  
  (* Style the button *)
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
  
  (* Add hover effects *)
  btn##.onmouseover := Html.handler (fun _ -> 
    btn##.style##.backgroundColor := Js.string "#388E3C"; (* Darker green *)
    ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "transform" (Js.string "scale(1.05)"));
    Js._false
  );
  btn##.onmouseout := Html.handler (fun _ -> 
    btn##.style##.backgroundColor := Js.string "#4CAF50"; (* Back to normal green *)
    ignore (Js.Unsafe.set (Js.Unsafe.get btn##.style) "transform" (Js.string "scale(1.0)"));
    Js._false
  );
  
  (* Add event listener *)
  btn##.onclick := Html.handler (fun _ -> 
    !healthy_creet_handler ();
    Js._false
  );
  
  healthy_creet_button := Some btn;
  Dom.appendChild parent btn

(* Register handlers for creet spawn buttons *)
let register_mean_creet_handler handler =
  mean_creet_handler := handler

let register_berserker_creet_handler handler =
  berserker_creet_handler := handler

(* Register handlers for additional creet spawn buttons *)
let register_healthy_creet_handler handler =
  healthy_creet_handler := handler

(* Initialize the UI elements *)
let init_ui doc game_div canvas_height =
  (* Create UI container div for buttons at the bottom *)
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
  
  (* Create timer div at the very top of the page, centered *)
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
  
  (* Create timer div *)
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
  
  (* Create speed control buttons first (on the left side) *)
  create_speed_buttons doc container;
  
  (* Create spawn rate control buttons *)
  create_spawn_buttons doc container;
  
  (* Create spell button *)
  create_spell_button doc container;  
  
  (* Create pause button *)
  create_pause_button doc container;
  
  (* Create mean creet spawn button *)
  create_mean_creet_button doc container;
  
  (* Create berserker creet spawn button *)
  create_berserker_creet_button doc container;
  
  (* Create healthy creet spawn button *)
  create_healthy_creet_button doc container

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
    div##.textContent := Js.some (Js.string (Printf.sprintf "%02d:%02d" minutes seconds))
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
