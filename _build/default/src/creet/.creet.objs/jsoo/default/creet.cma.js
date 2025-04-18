// Generated by js_of_ocaml
//# buildInfo:effects=disabled, kind=cma, use-js-string=true, version=6.0.1

//# unitInfo: Provides: Creet
//# unitInfo: Requires: Js_of_ocaml__Dom_html, Js_of_ocaml__Js, Stdlib, Stdlib__Random
(function
  (globalThis){
   "use strict";
   var runtime = globalThis.jsoo_runtime;
   function caml_call1(f, a0){
    return (f.l >= 0 ? f.l : f.l = f.length) === 1
            ? f(a0)
            : runtime.caml_call_gen(f, [a0]);
   }
   function caml_call2(f, a0, a1){
    return (f.l >= 0 ? f.l : f.l = f.length) === 2
            ? f(a0, a1)
            : runtime.caml_call_gen(f, [a0, a1]);
   }
   var
    global_data = runtime.caml_get_global_data(),
    Js_of_ocaml_Js = global_data.Js_of_ocaml__Js,
    Stdlib_Random = global_data.Stdlib__Random,
    Stdlib = global_data.Stdlib,
    Js_of_ocaml_Dom_html = global_data.Js_of_ocaml__Dom_html;
   function create_creet(img_src, x, y, dx, dy){
    var
     img = caml_call1(Js_of_ocaml_Dom_html[68], Js_of_ocaml_Dom_html[2]),
     t0 = runtime.caml_jsstring_of_string(img_src);
    img.src = t0;
    return [0, x, y, dx, dy, img];
   }
   var
    t2 = Js_of_ocaml_Js[34],
    last_direction_change = [0, t2.now() / 1000.],
    global_speed = [0, 1.];
   function update_creet(creet, canvas_width, canvas_height, dt){
    var
     t3 = Js_of_ocaml_Js[34],
     current_time = t3.now() / 1000.,
     _a_ = caml_call1(Stdlib_Random[14], 5.) + 1.;
    if(_a_ < current_time - last_direction_change[1]){
     var base_speed = 10.;
     if(caml_call1(Stdlib_Random[15], 0)){
      var base_speed$0 = caml_call1(Stdlib_Random[15], 0) ? base_speed : -10.;
      creet[3] = base_speed$0;
      creet[4] = 0.;
     }
     else{
      creet[3] = 0.;
      var base_speed$1 = caml_call1(Stdlib_Random[15], 0) ? base_speed : -10.;
      creet[4] = base_speed$1;
     }
     last_direction_change[1] = current_time;
    }
    creet[1] = creet[1] + creet[3] * dt * 10. * global_speed[1];
    creet[2] = creet[2] + creet[4] * dt * 10. * global_speed[1];
    var
     _b_ = creet[1] < 30. ? 1 : 0,
     half_width = 30.,
     half_height = 30.,
     _c_ = _b_ || (canvas_width - 30. < creet[1] ? 1 : 0);
    if(_c_){
     creet[3] = - creet[3];
     var _d_ = caml_call2(Stdlib[16], canvas_width - 30., creet[1]);
     creet[1] = caml_call2(Stdlib[17], half_width, _d_);
    }
    var
     _e_ = creet[2] < 30. ? 1 : 0,
     _f_ = _e_ || (canvas_height - 30. < creet[2] ? 1 : 0);
    if(_f_){
     creet[4] = - creet[4];
     var _g_ = caml_call2(Stdlib[16], canvas_height - 30., creet[2]);
     creet[2] = caml_call2(Stdlib[17], half_height, _g_);
     var _h_ = 0;
    }
    else
     var _h_ = _f_;
    return _h_;
   }
   function draw_creet(context, creet){
    var draw_x = creet[1] - 35., draw_y = creet[2] - 35., t4 = creet[5];
    return context.drawImage(t4, draw_x, draw_y, 70., 70.);
   }
   var Creet = [0, create_creet, global_speed, update_creet, draw_creet];
   runtime.caml_register_global(24, Creet, "Creet");
   return;
  }
  (globalThis));

//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiLmNyZWV0Lm9ianMvanNvby9kZWZhdWx0L2NyZWV0LmNtYS5qcyIsInNlY3Rpb25zIjpbeyJvZmZzZXQiOnsibGluZSI6OCwiY29sdW1uIjowfSwibWFwIjp7InZlcnNpb24iOjMsImZpbGUiOiIuY3JlZXQub2Jqcy9qc29vL2RlZmF1bHQvY3JlZXQuY21hLmpzIiwibmFtZXMiOlsicnVudGltZSIsImNhbWxfY2FsbDEiLCJmIiwiYTAiLCJjYW1sX2NhbGwyIiwiYTEiLCJnbG9iYWxfZGF0YSIsIkpzX29mX29jYW1sX0pzIiwiU3RkbGliX1JhbmRvbSIsIlN0ZGxpYiIsIkpzX29mX29jYW1sX0RvbV9odG1sIiwiY3JlYXRlX2NyZWV0IiwiaW1nX3NyYyIsIngiLCJ5IiwiZHgiLCJkeSIsImltZyIsInQwIiwidDIiLCJsYXN0X2RpcmVjdGlvbl9jaGFuZ2UiLCJnbG9iYWxfc3BlZWQiLCJ1cGRhdGVfY3JlZXQiLCJjcmVldCIsImNhbnZhc193aWR0aCIsImNhbnZhc19oZWlnaHQiLCJkdCIsInQzIiwiY3VycmVudF90aW1lIiwiYmFzZV9zcGVlZCIsImhhbGZfd2lkdGgiLCJoYWxmX2hlaWdodCIsImRyYXdfY3JlZXQiLCJjb250ZXh0IiwiZHJhd194IiwiZHJhd195IiwidDQiLCJDcmVldCJdLCJzb3VyY2VzIjpbIi9idWlsdGluL2JsYWNrYm94Lm1sIiwiL2hvbWUvY2dlbGluLzQyUHJvamVjdHMvaDQybjQyL19idWlsZC9kZWZhdWx0L3NyYy9jcmVldC9jcmVldC5tbCJdLCJtYXBwaW5ncyI6Ik9BQUFBLFVBQUE7QUFBQSxZQUFBQyxXQUFBQyxHQUFBQztBQUFBQSxJQUFBO0FBQUE7QUFBQTtBQUFBO0FBQUEsWUFBQUMsV0FBQUYsR0FBQUMsSUFBQUU7QUFBQUEsSUFBQTtBQUFBO0FBQUE7QUFBQTtBQUFBO0FBQUEsSUFBQUMsY0FBQTtBQUFBLElBQUFDLGlCQUFBO0FBQUEsSUFBQUMsZ0JBQUE7QUFBQSxJQUFBQyxTQUFBO0FBQUEsSUFBQUMsdUJBQUE7QUFBQSxZQUFBQyxhQUFBQyxTQUFBQyxHQUFBQyxHQUFBQyxJQUFBQztBQUFBQTtBQUFBQSxLQUFBQyxNQ2NZO0FBQUEsS0FBQUMsS0FDRztBQUFBLElBQWI7QUFBQSxJQUE4QjtBQUFBLEdBQ0Q7QUFBQTtBQUFBLElBQUFDLEtEaEIvQjtBQUFBLElBQUFDLHdCQ21CNkM7QUFBQSxJQUFBQyxlQUF3QjtBQUFBLFlBQUFDLGFBQUFDLE9BQUFDLGNBQUFDLGVBQUFDO0FBQUFBO0FBQUFBLEtBQUFDLEtBUW5FO0FBQUEsS0FBQUMsZUFBK0I7QUFBQSxXQUVhO0FBQUEsSUFBSDtBQUFBLFNBQUFDLGFBQWdDO0FBQUEsS0FHcEU7QUFBQSxVQUFBQSxlQUNlO0FBQUEsTUFBYztBQUFBLE1BQzlCO0FBQUE7QUFBQTtBQUFBLE1BQ087QUFBQSxVQUFBQSxlQUVTO0FBQUEsTUFBYztBQUFBO0FBQUEsS0FFaEM7QUFBQTtBQUFBLElBSTBEO0FBQUEsSUFDQTtBQUFBO0FBQUEsV0FNNUQ7QUFBQSxLQUFBQyxhQUFBO0FBQUEsS0FBQUMsY0FBQTtBQUFBO0FBQUE7QUFBQSxLQUN3QjtBQUFBLGVBRUk7QUFBQSxLQUFmO0FBQUE7QUFBQTtBQUFBLFdBSWI7QUFBQTtBQUFBO0FBQUEsS0FDd0I7QUFBQSxlQUVLO0FBQUEsS0FBaEI7QUFBQSxlQUEyRTtBQUFBO0FBQUE7QUFBQSxlQUh4RjtBQUFBO0FBQUEsR0FJRztBQUFBLFlBQUFDLFdBQUFDLFNBQUFWO0FBQUFBLFFBQUFXLFNBVVUsZ0JBQUFDLFNBQ0EsZ0JBQUFDLEtBS1M7QUFBQSwwREFHQTtBQUFBO0FBQUEsT0FBQUMsUUE3RDZDO0FBQUE7QUFBQTtBQUFBLEVEbkJyRSIsInNvdXJjZXNDb250ZW50IjpbIigqIGdlbmVyYXRlZCBjb2RlICopIiwib3BlbiBKc19vZl9vY2FtbFxubW9kdWxlIEh0bWwgPSBEb21faHRtbFxuXG4oKiBDcmVldCB0eXBlIGRlZmluaXRpb24gKilcbnR5cGUgY3JlZXQgPSB7XG4gIG11dGFibGUgeDogZmxvYXQ7ICgqIFggcG9zaXRpb24gKilcbiAgbXV0YWJsZSB5OiBmbG9hdDsgKCogWSBwb3NpdGlvbiAqKVxuICBtdXRhYmxlIGR4OiBmbG9hdDsgKCogWCB2ZWxvY2l0eSAtIHdpbGwgYmUgbm9uLXplcm8gb3IgZHkgd2lsbCBiZSBub24temVybywgYnV0IG5vdCBib3RoICopXG4gIG11dGFibGUgZHk6IGZsb2F0OyAoKiBZIHZlbG9jaXR5IC0gd2lsbCBiZSBub24temVybyBvciBkeCB3aWxsIGJlIG5vbi16ZXJvLCBidXQgbm90IGJvdGggKilcbiAgaW1hZ2U6IEh0bWwuaW1hZ2VFbGVtZW50IEpzLnQgKCogSW1hZ2UgZWxlbWVudCAqKVxufVxuXG4oKiBDcmVhdGUgYSBuZXcgY3JlZXQgKilcbmxldCBjcmVhdGVfY3JlZXQgaW1nX3NyYyB4IHkgZHggZHkgPVxuICBsZXQgaW1nID0gSHRtbC5jcmVhdGVJbWcgSHRtbC5kb2N1bWVudCBpblxuICBpbWcjIy5zcmMgOj0gSnMuc3RyaW5nIGltZ19zcmM7XG4gIHsgeDsgeTsgZHg7IGR5OyBpbWFnZSA9IGltZyB9XG5cbigqIFVwZGF0ZSBjcmVldCBwb3NpdGlvbiBhbmQgaGFuZGxlIGJvdW5jaW5nICopXG5sZXQgbGFzdF9kaXJlY3Rpb25fY2hhbmdlID0gcmVmIChKcy50b19mbG9hdCAoSnMuZGF0ZSMjbm93KSAvLiAxMDAwLilcblxuKCogR2xvYmFsIHNwZWVkIGZhY3RvciAqKVxubGV0IGdsb2JhbF9zcGVlZCA9IHJlZiAxLjBcblxuKCogS2VlcCB0aGUgb3JpZ2luYWwgaW50ZXJmYWNlIGJ1dCB1c2UgdGhlIGdsb2JhbCBzcGVlZCBmYWN0b3IgaW50ZXJuYWxseSAqKVxubGV0IHVwZGF0ZV9jcmVldCBjcmVldCBjYW52YXNfd2lkdGggY2FudmFzX2hlaWdodCBkdCA9XG4gICgqIENoZWNrIGlmIGl0J3MgdGltZSB0byBjaGFuZ2UgZGlyZWN0aW9uICopXG4gIGxldCBjdXJyZW50X3RpbWUgPSBKcy50b19mbG9hdCAoSnMuZGF0ZSMjbm93KSAvLiAxMDAwLiBpblxuICBcbiAgaWYgY3VycmVudF90aW1lIC0uICFsYXN0X2RpcmVjdGlvbl9jaGFuZ2UgPiAoUmFuZG9tLmZsb2F0IDUuKSArLiAxLiB0aGVuIGJlZ2luXG4gICAgKCogUmFuZG9tbHkgY2hvb3NlIG5ldyBkaXJlY3Rpb246IGVpdGhlciBob3Jpem9udGFsIG9yIHZlcnRpY2FsICopXG4gICAgbGV0IGJhc2Vfc3BlZWQgPSAxMC4gaW5cbiAgICBpZiBSYW5kb20uYm9vbCAoKSB0aGVuIGJlZ2luXG4gICAgICBjcmVldC5keCA8LSAoaWYgUmFuZG9tLmJvb2wgKCkgdGhlbiBiYXNlX3NwZWVkIGVsc2UgLS5iYXNlX3NwZWVkKTtcbiAgICAgIGNyZWV0LmR5IDwtIDAuXG4gICAgZW5kIGVsc2UgYmVnaW5cbiAgICAgIGNyZWV0LmR4IDwtIDAuO1xuICAgICAgY3JlZXQuZHkgPC0gKGlmIFJhbmRvbS5ib29sICgpIHRoZW4gYmFzZV9zcGVlZCBlbHNlIC0uYmFzZV9zcGVlZClcbiAgICBlbmQ7XG4gICAgbGFzdF9kaXJlY3Rpb25fY2hhbmdlIDo9IGN1cnJlbnRfdGltZVxuICBlbmQ7XG4gIFxuICAoKiBVcGRhdGUgcG9zaXRpb24gYmFzZWQgb24gdmVsb2NpdHksIGFwcGx5aW5nIHRoZSBnbG9iYWwgc3BlZWQgZmFjdG9yICopXG4gIGNyZWV0LnggPC0gY3JlZXQueCArLiBjcmVldC5keCAqLiBkdCAqLiAxMC4gKi4gIWdsb2JhbF9zcGVlZDtcbiAgY3JlZXQueSA8LSBjcmVldC55ICsuIGNyZWV0LmR5ICouIGR0ICouIDEwLiAqLiAhZ2xvYmFsX3NwZWVkO1xuICBcbiAgbGV0IGhhbGZfd2lkdGggPSAzMC4gaW5cbiAgbGV0IGhhbGZfaGVpZ2h0ID0gMzAuIGluXG4gIFxuICAoKiBCb3VuY2Ugb2ZmIGxlZnQgYW5kIHJpZ2h0IGJvcmRlcnMgKilcbiAgaWYgY3JlZXQueCA8IGhhbGZfd2lkdGggfHwgY3JlZXQueCA+IChmbG9hdF9vZl9pbnQgY2FudmFzX3dpZHRoKSAtLiBoYWxmX3dpZHRoIHRoZW4gYmVnaW5cbiAgICBjcmVldC5keCA8LSAtLmNyZWV0LmR4O1xuICAgICgqIEVuc3VyZSBjcmVldCBzdGF5cyB3aXRoaW4gYm91bmRzICopXG4gICAgY3JlZXQueCA8LSBtYXggaGFsZl93aWR0aCAobWluICgoZmxvYXRfb2ZfaW50IGNhbnZhc193aWR0aCkgLS4gaGFsZl93aWR0aCkgY3JlZXQueCk7XG4gIGVuZDtcbiAgXG4gICgqIEJvdW5jZSBvZmYgdG9wIGFuZCBib3R0b20gYm9yZGVycywgYWNjb3VudGluZyBmb3IgaW1hZ2UgaGVpZ2h0ICopXG4gIGlmIGNyZWV0LnkgPCBoYWxmX2hlaWdodCB8fCBjcmVldC55ID4gKGZsb2F0X29mX2ludCBjYW52YXNfaGVpZ2h0KSAtLiBoYWxmX2hlaWdodCB0aGVuIGJlZ2luXG4gICAgY3JlZXQuZHkgPC0gLS5jcmVldC5keTtcbiAgICAoKiBFbnN1cmUgY3JlZXQgc3RheXMgd2l0aGluIGJvdW5kcyAqKVxuICAgIGNyZWV0LnkgPC0gbWF4IGhhbGZfaGVpZ2h0IChtaW4gKChmbG9hdF9vZl9pbnQgY2FudmFzX2hlaWdodCkgLS4gaGFsZl9oZWlnaHQpIGNyZWV0LnkpO1xuICBlbmRcblxuKCogRHJhdyBhIGNyZWV0IHdpdGggY29vcmRpbmF0ZSBkaXNwbGF5IGFuZCBvcmlnaW4gbWFya2VyICopXG5sZXQgZHJhd19jcmVldCBjb250ZXh0IGNyZWV0ID1cbiAgbGV0IGZsb2F0X3RvX2pzIGYgPSBKcy5udW1iZXJfb2ZfZmxvYXQgZiBpblxuICBsZXQgd2lkdGggPSA3MC4gaW4gKCogV2lkdGggb2YgaW1hZ2UgKilcbiAgbGV0IGhlaWdodCA9IDcwLiBpbiAoKiBIZWlnaHQgb2YgaW1hZ2UgKilcbiAgXG4gICgqIENhbGN1bGF0ZSB0aGUgdG9wLWxlZnQgcG9zaXRpb24gZm9yIGRyYXdpbmcgdGhlIGltYWdlXG4gICBieSBvZmZzZXR0aW5nIGZyb20gdGhlIGNlbnRlciBwb3NpdGlvbiAqKVxuICBsZXQgZHJhd194ID0gY3JlZXQueCAtLiAod2lkdGggLy4gMi4pIGluXG4gIGxldCBkcmF3X3kgPSBjcmVldC55IC0uIChoZWlnaHQgLy4gMi4pIGluXG4gIFxuICAoKiBEcmF3IHRoZSBjcmVldCBpbWFnZSwgb2Zmc2V0IHNvIHgseSBpcyB0aGUgY2VudGVyICopXG4gIGNvbnRleHQjI2RyYXdJbWFnZV93aXRoU2l6ZVxuICAgIGNyZWV0LmltYWdlXG4gICAgKGZsb2F0X3RvX2pzIGRyYXdfeClcbiAgICAoZmxvYXRfdG9fanMgZHJhd195KVxuICAgIChmbG9hdF90b19qcyB3aWR0aClcbiAgICAoZmxvYXRfdG9fanMgaGVpZ2h0KSJdLCJpZ25vcmVMaXN0IjpbMF19fV19
