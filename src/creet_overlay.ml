open Js_of_ocaml
open Lwt.Syntax
module Html = Dom_html

let overlay_container = ref None

let update_dom_element_texture creet =
  match creet.Creet.dom_element with
  | None -> ()
  | Some dom_elem ->
      let texture_path = match creet.Creet.status with
        | Creet.Healthy -> "HealthyCreet.png"
        | Creet.Contaminated -> "ContaminatedCreet.png"
        | Creet.Berserker -> "BerserkerCreet.png"
        | Creet.Mean -> "MeanCreet.png"
      in
      
      (* Get current position and size from the DOM element *)
      let current_left = Js.to_string dom_elem##.style##.left in
      let current_top = Js.to_string dom_elem##.style##.top in
      let current_width = Js.to_string dom_elem##.style##.width in
      let current_height = Js.to_string dom_elem##.style##.height in
      
      (* Use cssText to update all properties including position and size *)
      let css_style = Printf.sprintf 
        "position: absolute; pointer-events: auto; cursor: grab; z-index: 101; \
         background-image: url(%s); background-size: 80%%; background-repeat: no-repeat; \
         background-position: center; border-radius: 50%%; \
         left: %s; top: %s; width: %s; height: %s;" 
        texture_path current_left current_top current_width current_height in
      dom_elem##.style##.cssText := Js.string css_style

(* Initialize the overlay container *)
let init_overlay_container canvas =
  let container = Html.createDiv Html.document in
  container##.id := Js.string "creet-overlay-container";
  container##.style##.position := Js.string "absolute";
  container##.style##.top := Js.string "0";
  container##.style##.left := Js.string "0";
  container##.style##.width := Js.string (string_of_int canvas##.width ^ "px");
  container##.style##.height := Js.string (string_of_int canvas##.height ^ "px");
  container##.style##.pointerEvents := Js.string "none"; (* Allow events to pass through by default *)
  container##.style##.zIndex := Js.string "100";
  
  (* Get canvas parent and add container *)
  let canvas_parent = canvas##.parentNode in
  (match Js.Opt.to_option canvas_parent with
  | Some parent -> Dom.appendChild parent container
  | None -> ());
  
  overlay_container := Some container;
  container

let update_dom_element_position creet =
  match creet.Creet.dom_element with
  | None -> ()
  | Some dom_elem ->
      let base_size = Creet.creet_hitbox_size in
      let size = base_size *. creet.Creet.size_factor *. 1.5 in (* Make it bigger *)

      let elem_x = creet.Creet.x -. (size /. 2.) +. 5. in
      let elem_y = creet.Creet.y -. (size /. 2.) +. 25. in

      dom_elem##.style##.left := Js.string (string_of_float elem_x ^ "px");
      dom_elem##.style##.top := Js.string (string_of_float elem_y ^ "px");
      dom_elem##.style##.width := Js.string (string_of_float size ^ "px");
      dom_elem##.style##.height := Js.string (string_of_float size ^ "px")

(* Create DOM element for a creet *)
let rec create_creet_dom_element creet =
  match !overlay_container with
  | None -> ()
  | Some container ->
      let dom_elem = Html.createDiv Html.document in
      
      (* Style the DOM element *)
      dom_elem##.style##.position := Js.string "absolute";
      dom_elem##.style##.pointerEvents := Js.string "auto"; 
      dom_elem##.style##.cursor := Js.string "grab";
      dom_elem##.style##.zIndex := Js.string "101";
      
        (* Add texture based on creet status *)
  let texture_path = match creet.Creet.status with
    | Creet.Healthy -> "HealthyCreet.png"
    | Creet.Contaminated -> "ContaminatedCreet.png"
    | Creet.Berserker -> "BerserkerCreet.png"
    | Creet.Mean -> "MeanCreet.png"
  in

  (* Set all styling using cssText *)
  let css_style = Printf.sprintf 
    "position: absolute; pointer-events: auto; cursor: grab; z-index: 101; \
    background-image: url(%s); background-repeat: no-repeat; \
    background-position: center; border-radius: 50%%;" 
    texture_path in

  dom_elem##.style##.cssText := Js.string css_style;
      
      let base_size = Creet.creet_hitbox_size in
      let size = base_size *. creet.Creet.size_factor *. 1.5 in (* Make it bigger *)
      
      (* Position relative to canvas (not page) since container is already positioned *)
      let elem_x = creet.Creet.x -. (size /. 2.) +. 8. in
      let elem_y = creet.Creet.y -. (size /. 2.) +. 28. in

      dom_elem##.style##.left := Js.string (string_of_float elem_x ^ "px");
      dom_elem##.style##.top := Js.string (string_of_float elem_y ^ "px");
      dom_elem##.style##.width := Js.string (string_of_float size ^ "px");
      dom_elem##.style##.height := Js.string (string_of_float size ^ "px");
      
      (* Add to container *)
      Dom.appendChild container dom_elem;
      
      (* Store reference in creet *)
      creet.Creet.dom_element <- Some dom_elem;
      
      (* Setup drag event handlers *)
      setup_drag_handlers creet dom_elem

(* Setup drag event handlers for the DOM element *)
and setup_drag_handlers creet dom_elem =
  let dragging = ref false in
  let drag_offset_x = ref 0. in
  let drag_offset_y = ref 0. in
  
  (* Mouse down - start drag *)
  dom_elem##.onmousedown := Html.handler (fun ev ->
    dragging := true;
    creet.is_dragged <- true;
    
    let mouse_x = Js.to_float ev##.clientX in
    let mouse_y = Js.to_float ev##.clientY in
    
    drag_offset_x := creet.x -. mouse_x;
    drag_offset_y := creet.y -. mouse_y;
    
    dom_elem##.style##.cursor := Js.string "grabbing";
    
    (* Prevent default to avoid text selection *)
    Dom.preventDefault ev;
    Js._true
  );
  
  (* Setup global mouse move and mouse up handlers *)
  let rec handle_mouse_move () =
    let* event = Js_of_ocaml_lwt.Lwt_js_events.mousemove Html.document in
    if !dragging then begin
      let mouse_x = Js.to_float event##.clientX in
      let mouse_y = Js.to_float event##.clientY in
      
      let new_x = mouse_x +. !drag_offset_x in
      let new_y = mouse_y +. !drag_offset_y in
      
      (* Apply canvas boundaries *)
      let half_size = (Creet.creet_hitbox_size *. creet.size_factor) /. 2. in
      creet.x <- min (float_of_int Gamestate.canvas_width -. half_size) 
                    (max half_size new_x);
      creet.y <- min (float_of_int Gamestate.canvas_height -. half_size) 
                    (max half_size new_y);
      
      (* Update DOM element position *)
      update_dom_element_position creet;
    end;
    handle_mouse_move ()
  in
  
  let rec handle_mouse_up () =
    let* _event = Js_of_ocaml_lwt.Lwt_js_events.mouseup Html.document in
    if !dragging then begin
      dragging := false;
      creet.is_dragged <- false;
      creet.last_direction_change <- 0.; (* Reset direction change timer *)
      dom_elem##.style##.cursor := Js.string "grab";
    end;
    handle_mouse_up ()
  in
  
  Lwt.async (fun () -> handle_mouse_move ());
  Lwt.async (fun () -> handle_mouse_up ())

(* Remove DOM element for a creet *)
let remove_creet_dom_element creet =
  match creet.Creet.dom_element with
  | None -> ()
  | Some dom_elem ->
      (match Js.Opt.to_option dom_elem##.parentNode with
      | Some parent -> Dom.removeChild parent dom_elem
      | None -> ());
      creet.Creet.dom_element <- None

(* Update all creet DOM elements positions *)
let update_all_dom_elements creets =
  List.iter update_dom_element_position creets

(* Create DOM elements for all creets *)
let create_dom_elements_for_creets creets =
  List.iter (fun creet ->
    if creet.Creet.dom_element = None then
      create_creet_dom_element creet
  ) creets

(* Clean up DOM elements for removed creets *)
let cleanup_removed_creets current_creets =
  match !overlay_container with
  | None -> ()
  | Some container ->
      let children = container##.childNodes in
      let children_length = children##.length in
      for i = children_length - 1 downto 0 do
        match Js.Opt.to_option (children##item i) with
        | Some child ->
            let child_elem = Js.Unsafe.coerce child in
            let should_keep = List.exists (fun creet ->
              match creet.Creet.dom_element with
              | Some elem -> elem == child_elem
              | None -> false
            ) current_creets in
            if not should_keep then
              Dom.removeChild container child
        | None -> ()
      done