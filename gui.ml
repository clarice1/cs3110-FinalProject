open Graphics


type t = {
  mutable bcol : Graphics.color;
  mutable fcol : Graphics.color;
  mutable font : string;
  mutable font_size : int;
  mutable lw : int;
  mutable x : int;
  mutable y : int;
}


