(**Module to handle gui changes such as listening for mouse events and
   drawing graphics from state. *)

(**[draw_interactives ()] draws the interactives i.e texts for now*)
val draw_interactives : Interactive.clickable list -> unit

(**[draw st my_player img_tabl room_id_interactive] will draw the chess
   board according to the state of the board [state] and based on which
   player's point of view it is. requires: [img_tbl] = images_dict
   State.init_state () *)
val draw_game :
  State.t ->
  int ->
  (string, Graphics.image) Hashtbl.t ->
  Interactive.clickable ->
  unit

(**[init ()] opens a new window and sets up the gui with size 800x800*)
val init : unit -> unit

(**[open_img path w h] is the image found at [path] with resolution
   ([w], [h])*)
val open_img : string -> float -> float -> Graphics.image

(**[images_dict st] is the dictionary of images of the chess pieces
   currently in the baord of state [st]*)
val images_dict : State.t -> (string, Graphics.image) Hashtbl.t

(**[coordinate_pair_bound st] is the coordinate positions (x,y) of the
   mouse*)
val coordinate_pair_bound : Graphics.status -> int * int

(**[coordinate_pair st] is the coordinate positions (x/100,y/100) of the
   mouse*)
val coordinate_pair : Graphics.status -> int * int

(**[string_of_coordinate_pair tuple]*)
val string_of_coordinate_pair : int * int -> string

(**[invert_pos my_player pos] is the inverted position *)
val invert_pos : int -> int * int -> int * int

(**[highlight_valid_locations st p_op my_player] highlights all the
   valid squares on the board that the [p_op] can move to. *)
val highlight_valid_locations : State.t -> Piece.t option -> int -> unit

(**[listen bound f] will listen for a mouse click and pass the square
   (in the 8x8) grid being clicked on if [bound] is true otherwise just
   the mouse location to function [f] *)
val listen : bool -> (int * int -> State.t) -> State.t

(**[get_piece st pair] is the piece that is being clicked on at the
   poisiton the mouse clicked *)
val get_piece : State.t -> int * int -> Piece.t option
