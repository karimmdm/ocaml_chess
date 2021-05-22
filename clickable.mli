(* Module to represent clickable fields *)

(* Abstract data type to represent  a button or text that is clickable *)
type clickable = Text of ((int * int) * int * string)


(* [get_clickable_pos c] is the cartesian position of the clickable item [ce]*)
val get_clickable_pos : clickable -> int * int

(* [get_clickable_txt c] is the text of the clickable item [ce]*)
val get_clickable_text : clickable -> string

(* [get_clickable_font_size c] is the font size of the clickable item *)
val get_clickable_font_size : clickable -> int