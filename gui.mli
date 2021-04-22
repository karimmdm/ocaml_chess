(* Module to handle gui changes such as listening for mouse events and
   drawing graphics from state. *)

(* [draw st] will draw the chess board according to the state of the
   board [state] *)
val draw : State.t -> unit

(* [init ()] opens a new window and sets up the gui with size 800x800*)
val init : unit -> unit

(* [coordinate_pair st] is the coordinate positions (x,y) of the mouse*)
val coordinate_pair : Graphics.status -> int * int

(* [string_of_coordinate_pair tuple ]*)
val string_of_coordinate_pair : int * int -> string

(* [highlight_squares loc] highlights all the valid squares on the board
   that the selected piece can move to. *)
val highlight_squares : State.t -> int * int -> Piece.t option -> unit

(* [listen f] will listen for a mouse click and pass the mouse location
   to [f] *)
val listen : (int * int -> unit) -> unit

(* [get_piece st pair] is the piece that is being clicked 
   on at the poisiton the mouse clicked *)
val get_piece : State.t -> int*int -> Piece.t option
