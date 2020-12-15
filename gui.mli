(** 
   Graphical user interface for fractal production.

   This module implements our funtionality in a graphic window (as opposed to 
   working straight out of terminal).  
*)


(**[t] is the type containing the specifications of a Graphical
   User Interface (GUI).*)

(******************************************************************************)
(*Graphics Context*)
(******************************************************************************)

(**[t] is the type containing the specifications of a Graphical User Interface 
   (GUI). *)
type t 

(** [button_state] is the type representing a button in our GUI.*)
type button_state

(** [choice_state] is the type representing a set of choices in our GUI.*)
type choice_state

(**[textfield_state] is the type representing text string inputs. *)
type textfield_state

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

(** [get_curr g] is the current position of the mouse on [g]. *)
val get_curr : t -> int * int

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
type opt_val = 
  | Copt of Graphics.color 
  | Sopt of string 
  | Iopt of int 
  | Bopt of bool

(** [lopt] is the type of a list of [opt_val]. *)
type lopt = (string * opt_val) list

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

(** [set_gc gc lst_opt] creates a graphics context from [lopt]. *)
val set_gc : t -> (string * opt_val) list -> unit

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
val initial_re : rich_status

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
(** [display_init comp] erases the graphical region and selects the color of the
    label*)
val display_init : component -> unit

(** [display_label lab comp] returns the graphical component [comp] with label
    [lab].*)
val display_label : string -> component -> unit -> unit

(** [create_label lab plist] creates a componenet with label [lab].*)
val create_label : string -> (string * opt_val) list -> component

val courier_bold_18 : opt_val

val courier_bold_24 : opt_val

(** [create_panel b w h lopt] creates a panel that a graphical area 
    that can be a container *)
val create_panel : bool -> int -> int -> (string * opt_val) list -> component

(** [center_layout comp comp1 lopt] places a component on the center of a 
    container. *)
val center_layout : component -> component -> 'a -> unit

(** [grid_layout (a,b) comp comp1 lopt ] divides a container into a grid
    where each box has the same size.*)
val grid_layout : int * int -> component -> component -> 
  (string * opt_val) list -> unit

(**[open_main_window int int] opens main window where all the components 
   will be in with width [w] and height [h]. *)
val open_main_window : int -> int -> component

(** [create_bs st] creates a button state with *)
val create_bs : string -> button_state

(** [set_bs_action bs] changes the action function of the button. *)
val set_bs_action : button_state -> (button_state -> unit) -> unit 

(** [get_bs_text bs] retrieves the text of the button.*)
val get_bs_text : button_state -> string

(** [display_button comp bs ()] displays the button on the screen *)
val display_button : component -> button_state -> unit -> unit


(** [create_button st lopt] creates a button in a component. *)
val create_button : string -> (string * opt_val) list -> component * button_state 

(** [listener_button comp bs e] activates the action function when the button
    is pressed. *)
val listener_button : component -> button_state -> rich_status -> bool

(** [create_cs st_a] creates a choice state with choices [st_a].*)
val create_cs : string array -> choice_state

(** [set_cs_action bs f] changes the action function of the choice.*)
val set_cs_action : choice_state -> (choice_state -> unit) -> unit

(** [get_cs_text cs] retrieves the text of the choice state*)
val get_cs_text : choice_state -> string

(** [display_choice comp cs ()] shows the list of possible choices.*)
val display_choice : component -> choice_state -> unit -> unit

(** [listener_choice comp cs ] activates the action function when the choice 
    is pressed.*)
val listener_choice : component -> choice_state -> rich_status -> bool

(** [create_choice lc lopt] creates a component with the list 
    of possible choices.  *)
val create_choice : string list -> (string * opt_val) list -> component * choice_state

(** [create_tfs txt size dir] creates the internal state of textfields. *)
val create_tfs : string -> int -> bool -> textfield_state

(** [set_tfs_action] sets the action function for a textfield state.  *)
val set_tfs_action : textfield_state -> (textfield_state -> unit) -> unit

(** [set_tfs_cursor tfs b c] sets the bool of visible cursor and the cursor.*)
val set_tfs_cursor : bool -> char -> textfield_state -> unit

(** [set_tfs_echo tfs b c] sets the bool of visible echo and the echo. *)
val set_tfs_echo : bool -> char -> textfield_state -> unit

(** [get_tfs_text tfs] gets the text in the textfield state*)
val get_tfs_text : textfield_state -> string

(** [set_tfs_text comp tfs txt] changes the text within the state of the text
    field. *)
val set_tfs_text : component -> textfield_state -> string -> unit

(** [display_cursor comp tfs] shows where the cursor is on the screen. *)
val display_cursor : component -> textfield_state -> unit

(** [display_textfield comp tfs ()] displays textfield.*)
val display_textfield : component -> textfield_state -> unit -> unit

(** [listener_text_field comp tfs e] captures the focus achieved by a mouse
    click in the input zone. *)
val listener_text_field : component -> textfield_state -> rich_status -> bool

(** *)
val create_text_field : string ->int -> bool -> 
  (string * opt_val) list -> component * textfield_state


(******************************************************************************)
(*Enriched Components*)
(******************************************************************************)
type border_state = 
  {mutable relief : string; mutable line : bool;
   mutable bg2 : Graphics.color; mutable size : int};;

(**[create_border_state lopt] creates a border state. *)
val create_border_state : (string * opt_val) list -> border_state

(** [display_border bs c1 c ()] shows the borders of the window. *)
val display_border : border_state -> component -> component -> unit -> unit

(** [create_border c lopt] creates the border for the window. *)
val create_border : component -> (string * opt_val) list -> component
