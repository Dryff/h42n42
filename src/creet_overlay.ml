open Js_of_ocaml
open Lwt.Syntax
open Tyxml.Html
module Html = Dom_html

let overlay_container : Dom_html.divElement Js.t option ref = ref None

(* Helper function to extract specific element type from TyXML *)
let extract_element doc tyxml_element selector =
  let html_string = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) tyxml_element in
  let temp_container = Html.createDiv doc in
  temp_container##.innerHTML := Js.string html_string;
  let element = Js.Opt.get 
    (temp_container##querySelector (Js.string selector))
    (fun () -> failwith ("Element not found: " ^ selector)) in
  Js.Unsafe.coerce element

(* Get texture path helper function *)
let get_texture_path creet_status =
  match creet_status with
  | Creet.Healthy -> "HealthyCreet.png"
  | Creet.Contaminated -> "ContaminatedCreet.png"
  | Creet.Berserker -> "BerserkerCreet.png"
  | _ -> "MeanCreet.png"

let update_dom_element_texture creet =
  match creet.Creet.dom_element with
  | None -> ()
  | Some dom_elem ->
      let texture_path = get_texture_path creet.Creet.status in
      
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

(* Initialize the overlay container using TyXML *)
let init_overlay_container canvas =
  let canvas_width = canvas##.width in
  let canvas_height = canvas##.height in
  
  let container_tyxml = div 
    ~a:[
      a_id "creet-overlay-container";
      a_style (Printf.sprintf 
        "position: absolute; top: 0; left: 0; width: %dpx; height: %dpx; \
         pointer-events: none; z-index: 100;" 
        canvas_width canvas_height)
    ]
    [] in
  
  let container = extract_element Html.document container_tyxml "div" in
  
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

(* Create DOM element for a creet using TyXML *)
let rec create_creet_dom_element creet =
  match !overlay_container with
  | None -> ()
  | Some container ->
      let texture_path = get_texture_path creet.Creet.status in
      let base_size = Creet.creet_hitbox_size in
      let size = base_size *. creet.Creet.size_factor *. 1.5 in (* Make it bigger *)
      
      (* Position relative to canvas (not page) since container is already positioned *)
      let elem_x = creet.Creet.x -. (size /. 2.) +. 8. in
      let elem_y = creet.Creet.y -. (size /. 2.) +. 28. in

      (* Create TyXML div element for validation *)
      let creet_div_tyxml = div 
        ~a:[
          a_class ["creet-sprite"];
          a_style (Printf.sprintf 
            "position: absolute; pointer-events: auto; cursor: grab; z-index: 101; \
             background-image: url(%s); background-repeat: no-repeat; \
             background-position: center; border-radius: 50%%; \
             left: %fpx; top: %fpx; width: %fpx; height: %fpx;" 
            texture_path elem_x elem_y size size)
        ]
        [] in
      
      let dom_elem = extract_element Html.document creet_div_tyxml "div" in
      
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

(* Test function to demonstrate TyXML validation *)
let test_creet_overlay_validation () =
  (* Uncomment any of these to see TyXML validation errors *)
  
  (* ERROR: Invalid nesting - button cannot contain div *)
  (* let invalid1 = button [
    div ~a:[a_class ["creet-sprite"]] []
  ] in *)
  
  (* ERROR: Invalid attribute - colspan not valid for div *)
  (* let invalid2 = div ~a:[a_colspan 2] [] in *)
  
  (* ERROR: Invalid content - img cannot have text content *)
  (* let invalid3 = img ~src:"creet.png" ~alt:"creet" [txt "Invalid content"] in *)
  
  (* Valid TyXML structure for creet overlay *)
  let valid_overlay = div 
    ~a:[
      a_id "valid-creet-overlay";
      a_class ["overlay-container"];
      a_style "position: absolute; width: 100%; height: 100%; pointer-events: none;"
    ]
    [
      div ~a:[
        a_class ["creet-sprite"; "healthy"];
        a_style "position: absolute; background-image: url(HealthyCreet.png); width: 40px; height: 40px;"
      ] [];
      div ~a:[
        a_class ["creet-sprite"; "berserker"];
        a_style "position: absolute; background-image: url(BerserkerCreet.png); width: 40px; height: 40px;"
      ] []
    ] in
  
  valid_overlay