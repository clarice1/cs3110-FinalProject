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

(** [component] is the type of a component that has a size, a position in the 
    main window, the context in which it is used, a parent, and a child. 
    [rich_status] is the type that handles event information, including keyboard
    focus. *)
type component and rich_status

(** [get_gc c] accesses the data fields of component [c]. *)
val get_gc : component -> t

(** [is_container c]  *)
val is_container : component -> bool

(** [in_rect c (xp, yp)] checks that the mouse position applies to coordinate 
    [(xp, yp)] within the rectangle. *)
val in_rect : component -> int * int -> bool

(** [display_rect c unit] fills the rectangle of component [c] with the 
    background color. *)
val display_rect : component -> unit -> unit

(** [direct_layout c c1 lopt] places [c] [c1] relatively within their 
    containers. *)
val direct_layout : component -> component -> (string * opt_val) list -> unit

(** [create_component iw ih] creates a component with width [iw] and height 
    [ih]. *)
val create_component : int -> int -> component 

(** [empty_component] is the empty component. *)
val empty_component : component


(******************************************************************************)
(*Child Components*)
(******************************************************************************)

(** [change_coord c (dx, dy)] applies a relative change to the coordinates 
    [(dx, dy)] of component [c] and those of all its children. *)
val change_coord : component -> int * int -> unit

(** [add_component c c1 lopt] checks that the conditions for adding [c1] have 
    been met and then joins [c] and [c1]. *)
val add_component : component -> component -> lopt -> unit

(** [remove_component c c1] changes the link between the parent [c] and the 
    child [c1] and changes to the coordinates of the child and all its own 
    children. *)
val remove_component : component -> component -> unit

(** [set_layout f c] if [c] has no children, the position is changed 
    immediately. Otherwise, first we remove children, modify the container using
    [f], and add the children back in. *)
val set_layout : (component -> lopt -> unit) -> component -> unit

(** [display c] gives [display] to the children of [c]. *)
val display : component -> unit


(******************************************************************************)
(*Event Handling*)
(******************************************************************************)

(** [get_event e] gets the event [e]. *)
val get_event : rich_status -> rich_event

(** [get_mouse_x e] gets the x position of the mouse applied to [e]. *)
val get_mouse_x : rich_status -> int

(** [get_mouse_y e] gets the y position of the mouse applied to [e]. *)
val get_mouse_y : rich_status -> int

(** [get_key e] reads key value. *)
val get_key : rich_status -> char

(** [has_key_focus e c] returns [true] if [c] is the key focus of [e], return 
    [false] otherwise. *)
val has_key_focus : rich_status -> component -> bool

(** [take_key_focus e c] updates the key focus of [e] with [c]. *)
val take_key_focus : rich_status -> component -> unit

(** [lose_key_focus e c] replaces the key focus of [e] with the empty 
    component. *)
val lose_key_focus : rich_status -> 'a -> unit

(** [has_gen_focus e c] returns [true] if [c] is the gen focus of [e], return 
    [false] otherwise. *)
val has_gen_focus : rich_status -> component -> bool

(** [take_gen_focus e c] updates the gen focus of [e] with [c]. *)
val take_gen_focus : rich_status -> component -> unit

(** [lose_gen_focus e c] replaces the gen focus of [e] with the empty 
    component. *)
val lose_gen_focus : rich_status -> 'a -> unit

(** [send_event rs c] returns [true] if the event was handled and [false] if the
    event was not handled. *)
val send_event : rich_status -> component -> bool

(** [compute_rich_event s0 s1] constructs a rich event out of [s0] and [s1]. *)
val compute_rich_event : Graphics.status -> Graphics.status -> rich_event

(** [send_new_events res0 res1] constructs a rich event out of [res0] and 
    [res1]. *)
val send_new_events : rich_status -> rich_status -> unit

(** [initial_re] is the initial value for [rich_event]. *)
val initial_re : rich_event

(** [loop b_disp b_motion c] manages the sequences of interactions with a 
    component, where [c] is the root of the component tree. *)
val loop : bool -> bool -> component -> unit

(** [make_click e x y] creates by hand status information corresponding to mouse
    events. *)
val make_click : rich_event -> int -> int -> rich_status

(** [make_key e ch c] creates by hand status information corresponding to 
    keyboard events.  *)
val make_key : rich_event -> 'a -> char -> rich_status 


(******************************************************************************)
(*Defining Components*)
(******************************************************************************)


(******************************************************************************)
(*Enriched Components*)
(******************************************************************************)
