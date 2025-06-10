open Js_of_ocaml
open Lwt.Syntax 
module Html = Dom_html

let create_canvas parent width height =
  let canvas = Html.createCanvas Html.document in
  canvas##.width := width;
  canvas##.height := height;
  Dom.appendChild parent canvas;
  canvas

(* Periodic spawning and speed increase function *)
let rec spawn_and_speed_loop () =
  let current_time = Js.to_float (Js.Unsafe.js_expr "new Date().getTime()") /. 1000. in

  (* Only spawn if the game is not over and not paused *)
  if not !Gamestate.game_over && not !Gamestate.is_paused then begin
    (* Check if it's time to spawn a new creet *)
    if current_time -. !Gamestate.last_spawn >= !Gamestate.spawn_interval then begin
      let old_count = List.length !Gamestate.creets in
      Gamestate.spawn_creet ();
      let new_count = List.length !Gamestate.creets in

      (* If a new creet was spawned, start its movement and create its DOM element *)
      if new_count > old_count then begin
        match !Gamestate.creets with
        | new_creet :: _ -> 
            Movement.start_creet_movement new_creet Gamestate.canvas_width Gamestate.canvas_height Gamestate.creets;
            Creet_overlay.create_creet_dom_element new_creet
        | [] -> ()
      end;
      
      Gamestate.last_spawn := current_time;
      Gamestate.spawn_interval := float_of_int !Gamestate.spawn_interval_low +. 
                                  Random.float (float_of_int (!Gamestate.spawn_interval_high - !Gamestate.spawn_interval_low));
    end;
    
    (* Increasing global speed *)
    if current_time -. !Gamestate.last_speed_increase >= Gamestate.speed_increase_interval then begin
      Creet.global_speed := !Creet.global_speed +. Gamestate.speed_increase_factor;
      let speed_percentage = int_of_float (!Creet.global_speed *. 100.) in
      Printf.printf "Speed increased to %d%%\n" speed_percentage;
      Gamestate.last_speed_increase := current_time
    end;
  end;

  ignore (Html.window##setTimeout
    (Js.wrap_callback (fun () -> spawn_and_speed_loop ()))
    (Js.number_of_float 1000.)
  )

(* Handle mouse clicks on replay button *)
let handle_canvas_click canvas ev =
  let mouse_x = Js.to_float ev##.clientX in
  let mouse_y = Js.to_float ev##.clientY in
  let canvas_rect = canvas##getBoundingClientRect in
  let canvas_x = mouse_x -. Js.to_float canvas_rect##.left in
  let canvas_y = mouse_y -. Js.to_float canvas_rect##.top in
  
  if !Gamestate.game_over && Renderer.is_click_on_replay_button canvas_x canvas_y Gamestate.canvas_width Gamestate.canvas_height then begin
    Movement.stop_all_movements ();
    List.iter Creet_overlay.remove_creet_dom_element !Gamestate.creets;
    
    Gamestate.reset_game ();
    Movement.start_all_movements Gamestate.canvas_width Gamestate.canvas_height Gamestate.creets;
    
    Creet_overlay.create_dom_elements_for_creets !Gamestate.creets;

    Js._true
  end else
    Js._false

(* GAME LOOP *)
let rec game_loop doc canvas context =
  let current_time = Js.to_float (Js.Unsafe.js_expr "new Date().getTime()") /. 1000. in
  let dt = if !Gamestate.is_paused then 0. else current_time -. !Gamestate.last_time in
  Gamestate.last_time := current_time;

  if !Gamestate.game_over then begin
    List.iter Creet_overlay.remove_creet_dom_element !Gamestate.creets;
  end;

  (* Update Game if not paused or game over *)
  if not !Gamestate.is_paused && not !Gamestate.game_over then begin
    Gamestate.update_spell_cooldown dt;
    
    Gamestate.check_all_creets_health ();
    Gamestate.elapsed_time := !Gamestate.elapsed_time +. dt;
    Ui.update_timer !Gamestate.elapsed_time;
    
    (* Update DOM element positions *)
    Creet_overlay.update_all_dom_elements !Gamestate.creets;

    List.iter Creet_overlay.update_dom_element_texture !Gamestate.creets;
  end;
                        
  (* Draw background and static elements *)
  let hospital_config = (Gamestate.hospital_width, Gamestate.hospital_height, Gamestate.hospital_spacing, 
                        Gamestate.initial_hospital_x, Gamestate.num_hospitals) in
                        
  (* Create list of active spell circles for rendering *)
  let spell_circles = 
    if !Gamestate.spell_circle.duration > 0. then 
      [{ 
        Renderer.x = !Gamestate.spell_circle.x;
        y = !Gamestate.spell_circle.y;
        radius = !Gamestate.spell_circle.radius;
        duration = !Gamestate.spell_circle.duration 
      }] 
    else [] 
  in
  
  (* Render everything *)
  Renderer.render context doc canvas !Gamestate.creets !Gamestate.elapsed_time !Gamestate.game_over !Gamestate.is_paused spell_circles 
                 (float_of_int !Gamestate.spawn_interval_low) (float_of_int !Gamestate.spawn_interval_high) hospital_config;

  (* Request next animation frame *)
  ignore (Html.window##requestAnimationFrame(
    Js.wrap_callback (fun _ -> 
      let _ = game_loop doc canvas context in 
      ())
  ));
  Lwt.return ()

let rec handle_canvas_mousedown_events canvas =
  let* event = Js_of_ocaml_lwt.Lwt_js_events.mousedown canvas in
  let _ = handle_canvas_click canvas event in
  handle_canvas_mousedown_events canvas

(* Setup mouse event loops - simplified since DOM elements handle their own dragging *)
let setup_mouse_events canvas =
  Lwt.async (fun () -> handle_canvas_mousedown_events canvas)

(* INITIALIZATION OF CANVAS AND UI *)
let init () =
  let doc = Html.document in
  let body = doc##.body in
  
  (* Create a div for our game *)
  let game_div = Html.createDiv doc in
  Dom.appendChild body game_div;
  
  (* Add a title *)
  let title = Html.createH1 doc in
  title##.textContent := Js.some (Js.string "H42N42");
  Dom.appendChild game_div title;
  
  (* Create game container for canvas and parameters *)
  let game_container = Html.createDiv doc in
  game_container##.style##.position := Js.string "relative";
  game_container##.style##.width := Js.string (string_of_int Gamestate.canvas_width ^ "px");
  game_container##.style##.margin := Js.string "0 auto";
  Dom.appendChild game_div game_container;
  
  (* Create our game canvas *)
  let canvas = create_canvas game_container Gamestate.canvas_width Gamestate.canvas_height in
  
  (* Initialize overlay container *)
  let _ = Creet_overlay.init_overlay_container canvas in

  (* Add timer div below the canvas *)
  Ui.init_ui doc game_div;

  (* Setup mouse event loops *)
  setup_mouse_events canvas;
  
  (* Get canvas 2D context *)
  let context = canvas##getContext (Html._2d_) in
  
  (* Initialize random number generator *)
  Random.self_init ();
  
  (* Initialize game state *)
  Gamestate.reset_game ();

  Creet_overlay.create_dom_elements_for_creets !Gamestate.creets;
  Movement.start_all_movements Gamestate.canvas_width Gamestate.canvas_height Gamestate.creets;
  spawn_and_speed_loop ();

  (* Register keydown handler to toggle debug mode *)
  Dom_html.window##.onkeydown := Dom_html.handler Input.keydown_handler;

  (* Register handler to pause/unpause *)
  let _ = Dom_html.addEventListener Dom_html.document 
    (Dom_html.Event.make "visibilitychange")
    (Dom_html.handler (fun _ ->
      let was_hidden = Js.to_bool (Js.Unsafe.js_expr "document.hidden") in
      if was_hidden <> !Gamestate.is_paused then begin
        Gamestate.is_paused := was_hidden;
        Ui.update_pause_button_state !Gamestate.is_paused;
      end;
      Js._false))
    Js._false
  in
    
  (* Register spawn button handlers with DOM element creation *)
  let spawn_with_dom spawn_fn = fun () ->
    let old_count = List.length !Gamestate.creets in
    spawn_fn ();
    let new_count = List.length !Gamestate.creets in
    if new_count > old_count then begin
      match !Gamestate.creets with
      | new_creet :: _ -> Creet_overlay.create_creet_dom_element new_creet
      | [] -> ()
    end
  in

  (* REGISTER GAME SETTINGS UI *)
  Ui.register_spell_button_handler Gamestate.cast_healing_spell;
  Ui.register_pause_button_handler Gamestate.toggle_pause;
  Ui.register_speed_plus_handler (fun () ->
    if not !Gamestate.game_over && not !Gamestate.is_paused then begin
      let new_speed = !Creet.global_speed +. 0.1 in
      Creet.global_speed := min new_speed 3.0;  (* Cap at 300% speed *)
      let speed_percentage = int_of_float (!Creet.global_speed *. 100.) in
      Printf.printf "Speed increased to %d%%\n" speed_percentage
    end
  );
  Ui.register_speed_minus_handler (fun () ->
    if not !Gamestate.game_over && not !Gamestate.is_paused then begin
      let new_speed = !Creet.global_speed -. 0.1 in
      Creet.global_speed := max new_speed 0.1;  (* Minimum 10% speed *)
      let speed_percentage = int_of_float (!Creet.global_speed *. 100.) in
      Printf.printf "Speed decreased to %d%%\n" speed_percentage
    end
  );
  Ui.register_spawn_plus_handler (fun () ->
    if not !Gamestate.game_over && not !Gamestate.is_paused then begin
      Gamestate.spawn_interval_low := max 1 (!Gamestate.spawn_interval_low - 1);
      Gamestate.spawn_interval_high := max (!Gamestate.spawn_interval_low + 4) (!Gamestate.spawn_interval_high - 1);
      let avg_interval = (!Gamestate.spawn_interval_low + !Gamestate.spawn_interval_high) / 2 in
      Printf.printf "Spawn rate increased: %d-%d seconds (avg: %d)\n" 
        !Gamestate.spawn_interval_low !Gamestate.spawn_interval_high avg_interval
    end
  );
  Ui.register_spawn_minus_handler (fun () ->
    if not !Gamestate.game_over && not !Gamestate.is_paused then begin
      Gamestate.spawn_interval_low := !Gamestate.spawn_interval_low + 1;
      Gamestate.spawn_interval_high := !Gamestate.spawn_interval_high + 1;
      let avg_interval = (!Gamestate.spawn_interval_low + !Gamestate.spawn_interval_high) / 2 in
      Printf.printf "Spawn rate decreased: %d-%d seconds (avg: %d)\n" 
        !Gamestate.spawn_interval_low !Gamestate.spawn_interval_high avg_interval
    end
  );

(* Register button handlers with DOM element creation *)
Ui.register_mean_creet_handler (spawn_with_dom Movement.spawn_mean_creet);
Ui.register_berserker_creet_handler (spawn_with_dom Movement.spawn_berserker_creet);
Ui.register_healthy_creet_handler (spawn_with_dom Movement.spawn_healthy_creet);

  (* Start game loop *)
  game_loop doc canvas context

(* Start the application when the page is loaded *)
let () =
  Html.window##.onload := Html.handler (fun _ ->
    let _ = Lwt.async init in
    Js._false)