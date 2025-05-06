open Js_of_ocaml
open Creet
open Canvas
open Renderer
module Html = Dom_html

(* Periodic spawning and speed increase function *)
let rec spawn_and_speed_loop () =
  let current_time = Js.to_float (Js.Unsafe.js_expr "new Date().getTime()") /. 1000. in
  
  (* Only spawn if the game is not over *)
  if not !Gamestate.game_over && not !Gamestate.is_paused then begin
    (* Check if it's time to spawn a new creet *)
    if current_time -. !Gamestate.last_spawn >= !Gamestate.spawn_interval then begin
      Gamestate.spawn_creet ();
      Gamestate.last_spawn := current_time;
      (* Set new random spawn interval based on current range *)
      Gamestate.spawn_interval := float_of_int !Gamestate.spawn_interval_low +. 
                                  Random.float (float_of_int (!Gamestate.spawn_interval_high - !Gamestate.spawn_interval_low));
    end;
    
    (* Check if it's time to increase the global speed *)
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

(* Handle mouse clicks including the replay button *)
let handle_canvas_click canvas ev =
  let mouse_x = Js.to_float ev##.clientX in
  let mouse_y = Js.to_float ev##.clientY in
  let canvas_rect = canvas##getBoundingClientRect in
  let canvas_x = mouse_x -. Js.to_float canvas_rect##.left in
  let canvas_y = mouse_y -. Js.to_float canvas_rect##.top in
  
  if !Gamestate.game_over && Renderer.is_click_on_replay_button canvas_x canvas_y Gamestate.canvas_width Gamestate.canvas_height then begin
    Gamestate.reset_game ();
    Js._true
  end else if !Gamestate.game_over then
    Js._false
  else
    Input.mousedown_handler ~game_over:Gamestate.game_over ~creets:Gamestate.creets ~dragging:Gamestate.dragging ~offset_x:Gamestate.offset_x ~offset_y:Gamestate.offset_y canvas ev

(* Main game loop *)
let rec game_loop doc canvas context =
  (* Calculate time delta *)
  let current_time = Js.to_float (Js.Unsafe.js_expr "new Date().getTime()") /. 1000. in
  let dt = if !Gamestate.is_paused then 0. else current_time -. !Gamestate.last_time in
  Gamestate.last_time := current_time;

  (* Only update game state if not paused and not game over *)
  if not !Gamestate.is_paused && not !Gamestate.game_over then begin
    (* Update spell cooldown *)
    Gamestate.update_spell_cooldown dt;

    (* Update and draw creets *)
    List.iter (fun creet -> 
      (* Only update creets if game is not over *)
      update_creet creet Gamestate.canvas_width Gamestate.canvas_height dt !Gamestate.creets;
      if not !Gamestate.game_over then
        check_collisions creet !Gamestate.creets;
    ) !Gamestate.creets;
    
    (* Check if all creets are unhealthy *)
    Gamestate.check_all_creets_health ();

    (* Calculate elapsed time only if game is not over or paused *)
    Gamestate.elapsed_time := !Gamestate.elapsed_time +. dt;

    (* Update timer below the canvas *)
    Ui.update_timer !Gamestate.elapsed_time;
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
  Renderer.render context doc canvas !Gamestate.creets !Gamestate.elapsed_time !Gamestate.game_over spell_circles 
                 (float_of_int !Gamestate.spawn_interval_low) (float_of_int !Gamestate.spawn_interval_high) hospital_config;

  (* Display game over screen if game is over *)
  if !Gamestate.game_over then
    display_game_over context Gamestate.canvas_width Gamestate.canvas_height !Gamestate.elapsed_time !Gamestate.creets;
    
  (* Display pause overlay if paused *)
  if !Gamestate.is_paused && not !Gamestate.game_over then
    display_pause_overlay context Gamestate.canvas_width Gamestate.canvas_height;

  (* Request next animation frame *)
  ignore (Html.window##requestAnimationFrame(
    Js.wrap_callback (fun _ -> 
      let _ = game_loop doc canvas context in 
      ())
  ));
  Lwt.return ()

(* Handle visibility change to pause/unpause game *)
let handle_visibility_change _ =
  let is_hidden = Js.to_bool (Js.Unsafe.js_expr "document.hidden") in
  Gamestate.is_paused := is_hidden;
  if is_hidden then
    Printf.printf "Game paused - window lost focus\n"
  else
    Printf.printf "Game resumed - window regained focus\n";
  Js._false

(* Main initialization function *)
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
  
  (* Create game container for canvas and parameters *)
  let game_container = Html.createDiv doc in
  game_container##.style##.position := Js.string "relative";
  game_container##.style##.width := Js.string (string_of_int Gamestate.canvas_width ^ "px");
  game_container##.style##.margin := Js.string "0 auto";
  Dom.appendChild game_div game_container;
  
  (* Create our game canvas *)
  let canvas = create_canvas game_container Gamestate.canvas_width Gamestate.canvas_height in
  
  (* Set custom cursor for the entire canvas *)
  canvas##.style##.cursor := Js.string "pointer";
  canvas##.style##.position := Js.string "absolute";
  canvas##.style##.left := Js.string "0";
  canvas##.style##.top := Js.string "0";

  (* Add timer div below the canvas *)
  Ui.init_ui doc game_div;
  
  (* Set up mouse event handlers *)
  canvas##.onmousedown := Html.handler (handle_canvas_click canvas);
  Html.document##.onmousemove := Html.handler (Input.mousemove_handler
    ~dragging:Gamestate.dragging ~canvas_width:Gamestate.canvas_width ~canvas_height:Gamestate.canvas_height 
    ~offset_x:Gamestate.offset_x ~offset_y:Gamestate.offset_y canvas);
  Html.document##.onmouseup := Html.handler (fun _ -> Input.mouseup_handler ~dragging:Gamestate.dragging ());
  
  (* Get canvas 2D context *)
  let context = canvas##getContext (Html._2d_) in
  
  (* Initialize random number generator *)
  Random.self_init ();
  
  (* Initialize game state *)
  Gamestate.reset_game ();
  
  (* Start spawning creets and increasing speed *)
  spawn_and_speed_loop ();

  (* Register keydown handler for toggling hitboxes *)
  Dom_html.window##.onkeydown := Dom_html.handler Input.keydown_handler;

  (* Register visibility change handler to pause/unpause *)
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
    
  (* Register spell button handler *)
  Ui.register_spell_button_handler Gamestate.cast_healing_spell;
  
  (* Register pause button handler *)
  Ui.register_pause_button_handler Gamestate.toggle_pause;
  
  (* Register speed control button handlers *)
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
  
  (* Register spawn rate control button handlers *)
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
  
  (* Register mean creet spawn button handler *)
  Ui.register_mean_creet_handler Gamestate.spawn_mean_creet;
  
  (* Register berserker creet spawn button handler *)
  Ui.register_berserker_creet_handler Gamestate.spawn_berserker_creet;
  
  (* Register healthy creet spawn button handler *)
  Ui.register_healthy_creet_handler Gamestate.spawn_healthy_creet;
  
  (* Start the game loop *)
  game_loop doc canvas context

(* Start the application when the page is loaded *)
let () =
  Html.window##.onload := Html.handler (fun _ ->
    let _ = Lwt.async init in
    Js._false)