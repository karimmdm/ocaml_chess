type clickable =
  | Text of ((int * int) * int * int * string * (unit -> unit))

let make_text pos size click_boundary text func =
  Text (pos, size, click_boundary, text, func)

let get_clickable_pos = function Text (pos, _, _, _, _) -> pos

let get_clickable_text = function Text (_, _, _, str, _) -> str

let get_clickable_font_size = function
  | Text (_, ft_sz, _, _, _) -> ft_sz

let get_clickable_length = function
  | Text (_, _, length, _, _) -> length

let get_clickable_function = function Text (_, _, _, _, f) -> f

let on_click (clicked_x, clicked_y) c f =
  let x, y = get_clickable_pos c in
  let length = get_clickable_length c in
  let height = get_clickable_font_size c in
  if
    clicked_x >= x
    && clicked_x <= x + length
    && clicked_y >= y
    && clicked_y <= y + height
  then f ()
  else ()
