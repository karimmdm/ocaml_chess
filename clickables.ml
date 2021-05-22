type clickable = Text of ((int * int) * int * string)

let get_clickable_pos = function Text (pos, _, _) -> pos

let get_clickable_text = function Text (_, _, str) -> str

let get_clickable_font_size = function Text (_, ft_sz, _) -> ft_sz
