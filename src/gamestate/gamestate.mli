(* Spell circle effect type *)
type spell_circle = {
  x: float;        (* Center X position *)
  y: float;        (* Center Y position *)
  radius: float;   (* Circle radius *)
  duration: float; (* Duration remaining in seconds *)
}

(* Game state types and variables *)
val game_over : bool ref
val creets : Creet.creet list ref
val is_paused : bool ref
val elapsed_time : float ref
val spell_cooldown : float ref
val spell_active : bool ref
val spell_circle : spell_circle ref
val dragging : Creet.creet option ref
val offset_x : float ref
val offset_y : float ref
val last_time : float ref
val last_spawn : float ref
val last_speed_increase : float ref

(* Game configuration *)
val canvas_width : int
val canvas_height : int
val spawn_interval : float ref
val spawn_interval_low : int 
val spawn_interval_high : int 
val speed_increase_interval : float
val speed_increase_factor : float

(* Hospital configuration *)
val hospital_width : float
val hospital_height : float
val hospital_spacing : float
val initial_hospital_x : float
val num_hospitals : int

(* Actions *)
val reset_game : unit -> unit
val toggle_pause : unit -> unit
val cast_healing_spell : unit -> unit
val update_spell_cooldown : float -> unit
val check_all_creets_health : unit -> unit
val spawn_creet : unit -> unit