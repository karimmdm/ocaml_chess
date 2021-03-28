(* 
    Module to handle gui changes such as listening for mouse events 
    and drawing graphics from state. *)

(* 
    [draw state] will draw the chess board according to 
    the state of the board [state] *)
val draw : State.t -> ()

(* [init ()] opens a new window and sets up the gui with size 800x800*)
val init : unit -> unit 

(* [coordinate_pair st] is the coordinate positions (x,y) of the mouse*)
val coordinate_pair : Graphics.status -> (int * int)

(* [string_of_coordinate_pair tuple ]*)
val string_of_coordinate_pair : (int * int) -> String

(* 
    [listen f] will listen for a mouse click and pass the mouse location to [f]
    Requires: listen to be executed in a loop 
    *)
val listen : ('a -> 'b) -> ()

