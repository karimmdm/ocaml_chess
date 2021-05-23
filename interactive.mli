(* Module to represent clickable fields *)

(* data type to represent  a text that is clickable 
    Text of ((x,y), font, length, text) *)
type clickable

(* [make_text pos size click_boundary text func] is a 
    clickable text at cartestian coordiante [pos] of 
    font size [size] with [click_boundary] as the location 
    of the end of the text (obtained experimentally) with function [func]
    to be executed on click*)
val make_text : int*int -> int -> int -> string -> (unit -> unit) -> clickable

(* [get_clickable_pos c] is the cartesian position of the clickable item [ce]*)
val get_clickable_pos : clickable -> int * int

(* [get_clickable_txt c] is the text of the clickable item [ce]*)
val get_clickable_text : clickable -> string

(* [get_clickable_font_size c] is the font size of the clickable item *)
val get_clickable_font_size : clickable -> int

(* [get_clickable_function c] is the function bound to the clickable item *)
val get_clickable_function : clickable -> (unit -> unit)

(* [on_click pos c f] determines if the clickable [c] was clicked and if it was
    it executes the function [func] bound to [c]*)
val on_click : (int * int) -> clickable -> (unit -> unit) -> unit