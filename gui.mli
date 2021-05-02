(* Module to handle gui changes such as listening for mouse events and
   drawing graphics from state. *)

(* [draw st my_player img_tabl] will draw the chess board according to the state
   of the board [state] and based on which player's point of view it is.
   requires: [img_tbl] = images_dict State.init_state () *)
val draw : State.t -> int -> (string, Graphics.image)Hashtbl.t -> unit

(* [init ()] opens a new window and sets up the gui with size 800x800*)
val init : unit -> unit

(* [images_dict st] is the dictionary of images of the chess pieces currently 
in the baord of state [st]*)
val images_dict : State.t -> (string, Graphics.image)Hashtbl.t

(* [coordinate_pair st] is the coordinate positions (x,y) of the mouse*)
val coordinate_pair : Graphics.status -> int * int

(* [string_of_coordinate_pair tuple] *)
val string_of_coordinate_pair : int * int -> string

(* [move st pos] checks if the current state has a piece clicked
   already. If there is no piece selected, then [move st pos] updates
   the current state. *)

(* [invert_pos my_player pos] is the inverted position *)
val invert_pos : int -> int * int -> int * int

(* highlight_valid_locations st p_op my_player] highlights all the valid
   squares on the board that the [p_op] can move to. *)
val highlight_valid_locations : State.t -> Piece.t option -> int -> unit

(* [listen f] will listen for a mouse click and pass the mouse location
   to [f] *)
val listen : (int * int -> State.t) -> State.t

(* [get_piece st pair] is the piece that is being clicked on at the
   poisiton the mouse clicked *)
val get_piece : State.t -> int * int -> Piece.t option
