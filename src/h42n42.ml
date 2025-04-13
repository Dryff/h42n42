open Js_of_ocaml
open Creet
open Canvas
module Html = Dom_html

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
  
  (* Initialize random number generator *)
  Random.self_init ();
  
  (* Create creets with only horizontal or vertical movement (not diagonal) *)
  let create_axis_aligned_creet img_src x y =
    (* Choose to move either horizontally or vertically, not both *)
    if Random.bool() then
      (* Horizontal movement *)
      let base_speed = 10. in
      let dx = if Random.bool() then base_speed else -.base_speed in
      create_creet img_src x y dx 0.
    else
      (* Vertical movement *)
      let base_speed = 10. in
      let dy = if Random.bool() then base_speed else -.base_speed in
      create_creet img_src x y 0. dy
  in
  
  (* Initial creets *)
  let creet1 = create_axis_aligned_creet "creet1.png" 100. 200. in
  let creet2 = create_axis_aligned_creet "creet2.png" 400. 300. in
  let creets = ref [creet1; creet2] in

  (* Creet generator *)
  let last_spawn = ref (Js.to_float (Js.date##now) /. 1000.) in
  let spawn_interval = 2.0 +. Random.float 6.0 in (* spawn between 2 and 8 seconds *)
  
  (* Last time the global speed was increased *)
  let last_speed_increase = ref (Js.to_float (Js.date##now) /. 1000.) in
  let speed_increase_interval = 1.0 in (* increase speed every 10 seconds *)
  let speed_increase_factor = 0.15 in (* increase by 10% each time *)
  
  let spawn_creet () =
    let x = Random.float (float_of_int canvas_width) in
    let y = Random.float (float_of_int canvas_height) in
    let img_src = if Random.bool () then "creet1.png" else "creet2.png" in
    let new_creet = create_axis_aligned_creet img_src x y in
    creets := new_creet :: !creets
  in

  (* Set up periodic spawning and speed increase *)
  let rec spawn_and_speed_loop () =
    let current_time = Js.to_float (Js.date##now) /. 1000. in
    
    (* Check if it's time to spawn a new creet *)
    if current_time -. !last_spawn >= spawn_interval then begin
      spawn_creet ();
      last_spawn := current_time
    end;
    
    (* Check if it's time to increase the global speed *)
    if current_time -. !last_speed_increase >= speed_increase_interval then begin
      Creet.global_speed := !Creet.global_speed +. speed_increase_factor;
      let speed_percentage = int_of_float (!Creet.global_speed *. 100.) in
      Printf.printf "Speed increased to %d%%\n" speed_percentage;
      last_speed_increase := current_time
    end;
    
    ignore (Html.window##setTimeout
      (Js.wrap_callback (fun () -> spawn_and_speed_loop ()))
      (float_to_js 1000.)
    )
  in
  spawn_and_speed_loop ();
  
  (* Animation time handling *)
  let last_time = ref (Js.to_float (Js.date##now) /. 1000.) in
  
  (* Main game loop *)
  let rec game_loop () =
    (* Calculate time delta *)
    let current_time = Js.to_float (Js.date##now) /. 1000. in
    let dt = current_time -. !last_time in
    last_time := current_time;
    
    (* Draw background *)
    context##.fillStyle := Js.string "#eeeeee"; (* Light gray background *)
    context##fillRect
      (float_to_js 0.)
      (float_to_js 0.)
      (float_to_js (float_of_int canvas_width))
      (float_to_js (float_of_int canvas_height));
    
    (* Draw river at top *)
    context##.fillStyle := Js.string "#3498db"; (* Blue for river *)
    context##fillRect
      (float_to_js 0.)
      (float_to_js 0.)
      (float_to_js (float_of_int canvas_width))
      (float_to_js 30.);
    
    (* Draw hospital at bottom *)
    context##.fillStyle := Js.string "#e74c3c"; (* Red for hospital *)
    context##fillRect
      (float_to_js 0.)
      (float_to_js (float_of_int canvas_height -. 30.))
      (float_to_js (float_of_int canvas_width))
      (float_to_js 30.);
    
    (* Display current speed as text *)
    context##.fillStyle := Js.string "#000000"; (* Black text *)
    context##.font := Js.string "16px Arial";
    let speed_text = Printf.sprintf "Speed: %d%%" (int_of_float (!Creet.global_speed *. 100.)) in
    context##fillText 
      (Js.string speed_text)
      (float_to_js 10.)
      (float_to_js 50.);
    
    (* Update and draw creets *)
    List.iter (fun creet -> 
      update_creet creet canvas_width canvas_height dt;
      draw_creet context creet;
    ) !creets;
    
    (* Request next animation frame *)
    ignore (Html.window##requestAnimationFrame(
      Js.wrap_callback (fun _ -> 
        let _ = game_loop () in 
        ())
    ));
    Lwt.return ()
  in
  
  (* Start the game loop *)
  game_loop ()

(* Start the application when the page is loaded *)
let () =
  Html.window##.onload := Html.handler (fun _ ->
    let _ = Lwt.async init in
    Js._false)