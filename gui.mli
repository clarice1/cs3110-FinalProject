(**[t] is the type containing the specifications of a Graphical
   User Interface (GUI).*)
type t 


(** [make_default_context] sets default GUI settings for a GUI window.*)
val make_default_context: unit -> t

(** [get_bcol g] is the background color of [g]. *)
val get_bcol : t -> Graphics.color

(** [get_fcol g] is the primary color of [g] *)
val get_fcol : t -> Graphics.color

(** [get_font g] is the font of the text in [g]*)
val get_font : t -> string

(**[get_font_size g] is the font size of the text in [g].*)
val get_font_size : t -> int

(** [get_lw g] is the length/witdth of [g].*)
val get_lw : t -> int

(** [get_cur g] is the current position of the mouse on [g]. *)
val get_cur : t -> int * int

(** [set_bcol g c] sets [c] as the background color of [g].*)
val set_bcol : t -> Graphics.color -> unit

(** [set_fcol g c] sets [c] as the primary color of [g].*)
val set_fcol : t -> Graphics.color -> unit

(** [set_font g f] sets [f] as the font of [g] *)
val set_font : t -> string -> unit

(** [set_font_size g s] sets [s] as the font_size of [g]*)
val set_font_size : t -> int -> unit

(** [set_lw g l] sets [l] as the length and width of [g]*)
val set_lw : t -> int -> unit

(** [set_curr g p] sets [p] as the current location of the mouse in [g]*)
val set_curr : t -> int * int -> unit

(** [use_gui g] applies the settings in [g] to a graphical window. change*)
val use_gui : t -> unit