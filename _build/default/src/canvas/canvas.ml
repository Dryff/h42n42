open Js_of_ocaml
module Html = Dom_html

(* Create game canvas in the page *)
let create_canvas parent width height =
  let canvas = Html.createCanvas Html.document in
  canvas##.width := width;
  canvas##.height := height;
  Dom.appendChild parent canvas;
  canvas

(* Function to get document element by ID *)
let get_element_by_id id =
  Js.Opt.get
    (Html.document##getElementById(Js.string id))
    (fun () -> failwith ("Element " ^ id ^ " not found"))