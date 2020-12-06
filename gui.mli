(** The module Gui is a library that sits on top of the Graphics Library and 
    helps us construct the interface for an application. There are _ sections, 
    each given by headings. *)


(******************************************************************************)
(*Graphics Context*)
(******************************************************************************)

(**[t] is the type containing the specifications of a Graphical User Interface 
   (GUI). *)
type t 

(** [make_default_context] sets default GUI settings for a GUI window. *)
val make_default_context: unit -> t

(** [get_bcol g] is the background color of [g]. *)
val get_bcol : t -> Graphics.color

(** [get_fcol g] is the primary color of [g]. *)
val get_fcol : t -> Graphics.color

(** [get_font g] is the font of the text in [g]. *)
val get_font : t -> string

(**[get_font_size g] is the font size of the text in [g]. *)
val get_font_size : t -> int

(** [get_lw g] is the length/witdth of [g]. *)
val get_lw : t -> int

(** [get_cur g] is the current position of the mouse on [g]. *)
val get_cur : t -> int * int

(** [set_bcol g c] sets [c] as the background color of [g]. *)
val set_bcol : t -> Graphics.color -> unit

(** [set_fcol g c] sets [c] as the primary color of [g]. *)
val set_fcol : t -> Graphics.color -> unit

(** [set_font g f] sets [f] as the font of [g]. *)
val set_font : t -> string -> unit

(** [set_font_size g s] sets [s] as the font_size of [g]. *)
val set_font_size : t -> int -> unit

(** [set_lw g l] sets [l] as the length and width of [g]. *)
val set_lw : t -> int -> unit

(** [set_curr g p] sets [p] as the current location of the mouse in [g]. *)
val set_curr : t -> int * int -> unit

(** [use_gui g] applies the settings in [g] to a graphical window. *)
val use_gui : t -> unit


(******************************************************************************)
(*Events*)
(******************************************************************************)

(** [rich_event] is the type of events that happen with interaction at a 
    component level. *)
type rich_event


(******************************************************************************)
(*Options*)
(******************************************************************************)

(** [opt_val] is the type of values of options for creating components. *)
type opt_val

(** [lopt] is the type of a list of [opt_val]. *)
type lopt

(** [get_color lo name default] is the decoding function for integers. If 
    [name] belongs to [lo] then return the value associated with [name]. 
    Otherwise return [default]. 
    Raises: Exception "Option Error" if [name] is associated with anything 
    other than a color. *)
val get_color : ('a * opt_val) list -> 'a -> Graphics.color -> Graphics.color

(** [get_string lo name default] is the decoding function for integers. If 
    [name] belongs to [lo] then return the value associated with [name]. 
    Otherwise return [default]. 
    Raises: Exception "Option Error" if [name] is associated with anything 
    other than a string. *)
val get_string : ('a * opt_val) list -> 'a -> string -> string

(** [get_int lo name default] is the decoding function for integers. If [name]
    belongs to [lo] then return the value associated with [name]. Otherwise 
    return [default]. 
    Raises: Exception "Option Error" if [name] is associated with anything 
    other than an int. *)
val get_int : ('a * opt_val) list -> 'a -> int -> int

(** [get_bool lo name default] is the decoding function for integers. If [name]
    belongs to [lo] then return the value associated with [name]. Otherwise 
    return [default]. 
    Raises: Exception "Option Error" if [name] is associated with anything 
    other than a bool. *)
val get_bool : ('a * opt_val) list -> 'a -> bool -> bool
(* Could factor these out into a helper *)

(** [set_gui gc lst_opt] creates a graphics context from [lopt]. *)
val set_gui : t -> (string * opt_val) list -> unit

(** [make_dc] IS THIS A NECESSARY FUNCTION? I will check *)
val make_dc : unit -> t


(******************************************************************************)
(*Construction of Components*)
(******************************************************************************)


(******************************************************************************)
(*Child Components*)
(******************************************************************************)


(******************************************************************************)
(*Event Handling*)
(******************************************************************************)


(******************************************************************************)
(*Defining Components*)
(******************************************************************************)


(******************************************************************************)
(*Enriched Components*)
(******************************************************************************)
