open OUnit2

(******************************************************************************)
(*Tests for Matrix*)
(******************************************************************************)
open Matrix

(**Change to true if test cases involving larger matrices should be run *)
let do_big_test = false

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_int] pretty-prints int [x]. *)
let pp_int x = pp_string (string_of_int x)

(** [pp_float] pretty-prints float [x]. *)
let pp_float x = pp_string (string_of_float x)

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(**[pp_listlist lst pp_elt] pretty-prints list [listlist], using [pp_list pp_elt]
   to print each entry *)
let pp_listlist pp_elt lst = 
  pp_list (pp_list pp_elt) lst

let str_complex = Parse.string_of_complex

let str_poly p = pp_list str_complex (Polynomial.to_list p)

(**[str_matrix pp_elt m] is a string representing the matrix where
   elements are printed according to pp_elt[m] *)
let str_matrix pp_elt m =
  pp_listlist pp_elt (Matrix.to_lst m)

(**[check_eq name expected_output value] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [value] and printer [printer].*)
let check_eq name expected_output value printer =
  name >:: (fun _ -> assert_equal expected_output value ~printer:printer)

(** [matrix_test name m i j printer expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [Matrix.get i j m]. *)
let matrix_test name m i j printer expected_output = 
  check_eq name expected_output (Matrix.get i j m) printer

(** [test_exception name m i j expected_ex] constructs an OUnit
    test named [name] that asserts  [Matrix.get i j m] raises [expected_ex]*)
let test_exception name m i j expected_ex = 
  name >:: (fun _ -> assert_raises expected_ex (fun () -> Matrix.get i j m))

(** [iter_check name m f n expected_out printer] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [Matrix.iterate f n m]. *)
let iter_check name m f n expected_out printer =
  check_eq name expected_out (Matrix.iterate f n m) printer

(** [iter_check name m p f n expected_out printer] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [Matrix.iterate f n p m]. *)
let iterate_with_stop_check name m p f n expected_out printer =
  check_eq name expected_out 
    (Matrix.iterate_with_stop (fun x -> if p x then None else Some (f x)) n m) 
    printer

let iterate_with_stop_2_check name m f n expected_out printer =
  check_eq name expected_out (Matrix.iterate_with_stop_2 f n m) printer

(**The 7x7 matrix with all 0s *)
let zero_matrix = Matrix.init 7 7 (fun x y -> 0)

(**The 10x10 matrix with values the unique quadratic bijection N -> N. *)
let quadratic_matrix = Matrix.init 8 7 
    (fun x y -> (x + y) * (x + y + 1) / 2 + y)

(**The 11x11 complex matrix with values evenly spaced in the unit square *)
let cx_matrix = Matrix.cx_init {re = -1.; im = -1.} {re = 1.; im = 1.} 11 11

(**The matrix
   [0 1]
   [2 3] *)
let matrix_0123 = Matrix.init 2 2 (fun x y -> x + 2 * y)

(**The 1_000 by 1_000 matrix whose value at row i, column j is i *)
let big_row_num = Matrix.init 1_000 1_000 (fun x y -> x)

let string_option_int_matrix = 
  str_matrix (fun (n, x) -> match n, x with 
      | (None, x) -> "(None, " ^ pp_string (string_of_int x) ^ ")"
      | (Some n, x) -> 
        "(Some " ^ pp_int n ^ ", " ^ pp_int x ^ ")")

let matrix_tests = [
  check_eq "list representation of 0123 matrix" [[0; 2]; [1; 3]] 
    (Matrix.to_lst matrix_0123) (pp_listlist pp_int);

  check_eq "zero_matrix has 7 rows" 7 (Matrix.rows zero_matrix) pp_int;
  check_eq "zero_matrix has 7 columns" 7 (Matrix.columns zero_matrix) pp_int;
  check_eq "quadratic_matrix has 8 rows" 8 
    (Matrix.rows quadratic_matrix) pp_int;
  check_eq "quadratic matrix has 7 columns" 7 
    (Matrix.columns quadratic_matrix) pp_int;

  matrix_test "zero_matrix at row 0, column 0 is 0" zero_matrix 0 0 
    pp_int 0;
  test_exception "zero matrix has no value at 10, 5" zero_matrix 10 5 
    (Invalid_argument "(10, 5)");
  test_exception "zero matrix has no value at 5, 10" zero_matrix 5 10 
    (Invalid_argument "(5, 10)");
  test_exception "quadratic_matrix has no value at 6, 7" quadratic_matrix 6 7
    (Invalid_argument "(6, 7)");
  test_exception "quadratic matrix has no value at -2, 1" quadratic_matrix (-2) 
    1 (Invalid_argument "(-2, 1)");
  test_exception "quadratic matrix has no value at 1, -2" quadratic_matrix 1 
    (-2) (Invalid_argument "(1, -2)");

  matrix_test "cx_matrix at 0, 0 is -1 + 1i" cx_matrix 0 0 str_complex
    {re = -1.; im = 1.};
  matrix_test "cx_matrix at 1, 1 is -.8 + .8i" cx_matrix 1 1 str_complex
    {re = -0.8; im = 0.8};
  matrix_test "cx_matrix at 1, 2 is -.6 + .8i" cx_matrix 1 2 str_complex
    {re = -0.6; im = 0.8};
  matrix_test "cx_matrix at 2, 1 is -.8 + .6i" cx_matrix 2 1 str_complex
    {re = -0.8; im = 0.6};
  matrix_test "cx_matrix at 5, 5 is 0" cx_matrix 5 5 str_complex
    {re = 0.; im = 0.};
  matrix_test "cx_matrix at 0, 10 is 1 + i" cx_matrix 0 10 str_complex
    {re = 1.; im = 1.};
  matrix_test "cx_matrix at 10, 0 is -1 - i" cx_matrix 10 0 str_complex
    {re = -1.; im = -1.};
  matrix_test "cx_matrix at 10, 10 is 1 - i" cx_matrix 10 10 str_complex
    {re = 1.; im = -1.};

  matrix_test "quadratic_matrix at 0, 0 is 0" quadratic_matrix 0 0 
    pp_int 0;
  matrix_test "quadratic_matrix at 1, 0 is 1" quadratic_matrix 1 0 
    pp_int 1;
  matrix_test "quadratic_matrix at 0, 1 is 2" quadratic_matrix 0 1 
    pp_int 2;
  matrix_test "quadratic_matrix at 2, 0 is 3" quadratic_matrix 2 0 
    pp_int 3;
  matrix_test "quadratic_matrix at 1, 1 is 4" quadratic_matrix 1 1
    pp_int 4;
  matrix_test "quadratic_matrix at 7, 6 is 97" quadratic_matrix 7 6
    pp_int 97;

  iter_check "iterating 0 times is the identity" quadratic_matrix 
    (fun x -> x * x) 0 quadratic_matrix (str_matrix pp_int);
  iter_check "iterating the identity function is the identity" quadratic_matrix 
    (fun x -> x) 100 quadratic_matrix (str_matrix pp_int);
  iter_check "adding 100 to each value in zero yields 100 matrix" zero_matrix
    ((+) 1) 100 (Matrix.init 7 7 (fun x y -> 100)) (str_matrix pp_int);

  iterate_with_stop_check 
    "iterating 0 times with false includes each into (None, p)"
    matrix_0123 
    (fun x -> false) 
    (fun x -> 7) 
    0
    (Matrix.init 2 2 (fun x y -> (None, x + 2 * y)))
    string_option_int_matrix;

  iterate_with_stop_check 
    "iterating 0 times maps to (Some 0, z) when p, otherwise (None, z)"
    matrix_0123
    (fun x -> x > 1)
    (fun x -> 7)
    0
    (Matrix.init 2 2 
       (fun x y -> let z = x + 2 * y in 
         if z > 1 then (Some 0, z) else (None, z)))
    string_option_int_matrix;

  iterate_with_stop_check "add 100 with no stop to 0123"
    matrix_0123 
    (fun x -> false) 
    ((+) 1) 
    100
    (Matrix.init 2 2 (fun x y -> (None, 100 + x + 2 * y)))
    string_option_int_matrix;

  iterate_with_stop_check "add 2 to 0123, stopping when bigger than 2"
    matrix_0123 
    (fun x -> x > 2) 
    ((+) 1) 
    2
    (Matrix.init 2 2 (fun x y -> 
         match x, y with 
         | 0, 0 -> (None, 2)
         | 1, 0 -> (Some 2, 3)
         | 0, 1 -> (Some 1, 3)
         | 1, 1 -> (Some 0, 3)
         | _ -> failwith "impossible"))
    string_option_int_matrix;

  check_eq "add index to 0123"
    (Matrix.init 2 2 (fun x y -> 
         match x, y with 
         | 0, 0 -> 0
         | 1, 0 -> 2
         | 0, 1 -> 3
         | 1, 1 -> 5
         | _ -> failwith "impossible"))
    (Matrix.mapi (fun i j x -> i + j + x) matrix_0123) 
    (str_matrix string_of_int);

  if do_big_test then 
    begin
      iterate_with_stop_check 
        "add 1_000 to big_row_num, stopping when over 1_500"
        big_row_num
        (fun x -> x > 1_500)
        ((+) 1)
        1_000
        (Matrix.init 1_000 1_000 (fun x y -> 
             if x <= 500 then (None, x + 1_000) 
             else (Some (1_501 - x), 1501)))
        string_option_int_matrix;
    end
  else matrix_test "quadratic_matrix at 0, 2 is 5" quadratic_matrix 0 2 
      pp_int 5;

  iterate_with_stop_2_check 
    "add initial_val to 0123, stopping when bigger than 2"
    matrix_0123 
    (fun x y -> if y > 2 then None else Some (x + y)) 
    2
    (Matrix.init 2 2 (fun x y -> 
         match x, y with 
         | 0, 0 -> (None, 0)
         | 1, 0 -> (Some 2, 3)
         | 0, 1 -> (Some 1, 4)
         | 1, 1 -> (Some 0, 3)
         | _ -> failwith "impossible"))
    string_option_int_matrix;

  iterate_with_stop_2_check 
    "square then add initial_val to 0123, stopping when bigger than 5"
    matrix_0123 
    (fun x y -> if y > 10 then None else Some (x + y * y)) 
    10
    (Matrix.init 2 2 (fun x y -> 
         match x, y with 
         | 0, 0 -> (None, 0)
         | 1, 0 -> (Some 3, 26)
         | 0, 1 -> (Some 2, 38)
         | 1, 1 -> (Some 1, 12)
         | _ -> failwith "impossible"))
    string_option_int_matrix;
]

(******************************************************************************)
(*Tests for Polynomial*)
(******************************************************************************)
open Polynomial

let eval_test 
    (name : string) 
    (p : t ) 
    (z : Complex.t)
    (expected_output : Complex.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (eval p z)
        ~printer:str_complex)

let get_bound_test
    (name : string)
    (p : t)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_bound p)~printer:string_of_float)

let bounded_test
    (name : string)
    (p : t)
    (z : Complex.t)
    (expected_output : Complex.t option) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (bounded p z))

let sum_test name p q z = 
  check_eq name (Complex.add (Polynomial.eval p z) (Polynomial.eval q z))
    (Polynomial.eval (Polynomial.sum p q) z) str_complex

let mul_test name p q z = 
  check_eq name (Complex.mul (Polynomial.eval p z) (Polynomial.eval q z))
    (Polynomial.eval (Polynomial.mul p q) z) str_complex

let polydeg1 = from_list [{re = 1.; im = 0.}]
let polydeg2 = from_list [{re = 1.; im = 0.}; {re = 4.; im = 0.}]
let polydeg3 = from_list [{re = 1.; im = 45.}]
let polydeg4 = from_list [{re = 1.; im = 7.}; {re = 4.; im = 2.}]
let (z1 : Complex.t) = {re = 4.; im = 0.}
let (z2 : Complex.t) = {re = 2.; im = 1.}
let polydeg5 = from_list [{re = 54.; im = 0.}; {re = 3.; im = 0.}; 
                          {re = 0.; im = 9.}]
let polydeg6 = from_list [{re = 1.; im = 0.}; {re = 0.; im = 0.}]
let polydeg7 = from_list [{re = -1.; im = 0.}; {re = 3.; im = 0.}]
let polydeg8 = from_list [{re = 70.; im = 0.}; {re = 78.; im = 0.}]
let polydeg9 = from_list [{re = 0.3; im = 0.}; {re = 78.; im = 0.}]
let polydeg10 = from_list [{re = 4.; im = 0.}; {re = 0.; im = 2.}; 
                           {re = 7.; im = 0.}]
let polydeg11 = from_list [{re = 17.; im = 4.}; {re = 4.; im = 0.}; 
                           {re = 0.; im = 2.}; {re = 7.; im = 0.}]

let three_roots = from_roots [Complex.zero; Complex.one; Complex.(neg one)]

let polynomial_tests = [
  eval_test "zero polynomial" zero {re = 4.; im = 5.} {re = 0.; im = 0.};
  eval_test "deg 0 poly, no im" polydeg1 z1 {re = 1.; im = 0.};
  eval_test "deg 1 poly, no im" polydeg2 z1 {re = 8.; im = 0.};
  eval_test "deg 0 poly w im" polydeg3 z2 {re = 1.; im = 45.};
  eval_test "deg 1 poly w im" polydeg4 z2 {re = -1.; im = 17.};
  get_bound_test "zero poly" zero infinity;
  get_bound_test "p = 54x^2 + 3x + 9i" polydeg5 1.;
  get_bound_test "p = 4x^2 + 2ix + 7" polydeg10 2.5;
  get_bound_test "p = (17+4i)x^3 + 4x^2 + 2ix + 7" polydeg11 1.;
  get_bound_test "p = constant" polydeg1 infinity;
  get_bound_test "|a| < 1 " polydeg9 infinity;
  get_bound_test "|a| > 1 " polydeg8 0.;
  get_bound_test "|a| = 1, a != 1, b != 0" polydeg7 infinity;
  get_bound_test "|a| = 1, a = 1, b = 0" polydeg6 infinity;
  get_bound_test "|a| = 1, a = 1, b != 0" polydeg2 0.;
  bounded_test "poly diverges" polydeg2 z1 None;
  bounded_test "poly does not diverge" polydeg1 z1 (Some {re = 1.; im = 0.});

  check_eq "derivative of 54x^2 + 3x + 9i is 108x + 3" 
    (Polynomial.from_list [{re = 108.; im = 0.}; {re = 3.; im = 0.}])
    (Polynomial.diff polydeg5)
    str_poly;

  sum_test "two degree 2s at 7" polydeg5 polydeg10 {re = 7.; im = 0.};
  sum_test "degree 2 plus degree 3 at 7" polydeg5 polydeg11 {re = 7.; im = 0.};
  sum_test "degree 3 plus degree 2 at 7" polydeg11 polydeg5 {re = 7.; im = 0.};

  mul_test "two degree 2s at 7" polydeg5 polydeg10 {re = 7.; im = 0.};
  mul_test "degree 2 plus degree 3 at 7" polydeg5 polydeg11 {re = 7.; im = 0.};
  mul_test "degree 3 plus degree 2 at 7" polydeg11 polydeg5 {re = 7.; im = 0.};

  eval_test "0 is a root of z(z - 1)(z + 1)" three_roots 
    Complex.zero Complex.zero;
  eval_test "1 is a root of z(z - 1)(z + 1)" 
    three_roots Complex.one Complex.zero;
  eval_test "-1 is a root of z(z - 1)(z + 1)" three_roots
    Complex.(neg one) Complex.zero;
  eval_test "2(2 - 1)(2 + 1) = 6" three_roots {re = 2.; im = 0.} 
    {re = 6.; im = 0.};
]

let f = QCheck.Gen.map2 (fun r im  : Complex.t -> {re = r; im = im}) 
    QCheck.Gen.float QCheck.Gen.float
        |> QCheck.make ~print:str_complex


let lst = [Complex.zero; Complex.one; Complex.i; Complex.(neg one); 
           Complex.(neg i)]

let within_e z1 z2 = 
  Complex.norm (Complex.sub z1 z2) < Complex.norm z1 /. 1e9


let within_e_opt z1 z2 = 
  match z1, z2 with 
  | None, None -> true
  | None, Some _ | Some _, None -> false 
  | Some v1, Some v2 -> within_e v1 v2

let p1 = Polynomial.from_roots lst

let p2 = RootPolynomial.from_roots Complex.one lst

let poly_test_aux comp a b = 
  QCheck.Test.make ~count:1_000 f 
    (fun z -> comp (a z) (b z))
  |> QCheck_ounit.to_ounit2_test

let poly_test = poly_test_aux within_e

let poly_test_opt = poly_test_aux within_e_opt

let rand_poly_tests = 
  poly_test (Polynomial.eval p1) (RootPolynomial.eval p2)

let rand_poly_bounded_tests = 
  poly_test_opt (Polynomial.bounded p1) 
    (RootPolynomial.bbounded (RootPolynomial.bound p2))

let rand_poly_bbounded_tests = 
  poly_test_opt (Polynomial.bbounded (Polynomial.bound p1)) 
    (RootPolynomial.bbounded (RootPolynomial.bound p2))

let rand_poly_convert_tests = 
  poly_test (Polynomial.eval p1) 
    (Polynomial.eval (RootPolynomial.to_poly p2))

let rand_mul_tests = 
  poly_test 
    (fun z -> Complex.mul (Polynomial.eval p1 z) (RootPolynomial.eval p2 z))
    (Polynomial.eval (Polynomial.mul p1 (RootPolynomial.to_poly p2)))

let rand_p_tests = [
  rand_poly_tests;
  rand_poly_bounded_tests;
  rand_poly_bbounded_tests;
  rand_poly_convert_tests;
  rand_mul_tests
]

(******************************************************************************)
(*Tests for ToImage*)
(******************************************************************************)
open ToImage
open Complex

let black : Color.rgb = {r = 0; g = 0; b = 0}
let red : Color.rgb = {r = 250; g = 0; b = 0}
let blue : Color.rgb = {r = 0; g = 0; b = 250}
let green : Color.rgb = {r = 0; g = 250; b = 0}

let light_blue : Color.rgb = {r = 0; g = 0; b = 255 - ((500) * 255 / 5000) }
let dark_red : Color.rgb = {r = 255 - ((3000) * 255 / 5000); g = 0; b = 0}
let light_green : Color.rgb = {r = 0; g = 255 - ((300) * 255 / 2000); b = 0}

(** [julia_color_test name iter col coodinate expected_output] is an OUnit test
    named [name] that checks equivalence between [expected_output] and 
    [julia_color iter col coordinate] *)
let julia_color_test
    (name : string)
    (iter : int)
    (col : Color.rgb)
    (coordinate : (int option * Complex.t))
    (expected_output : Color.rgb) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (julia_color iter col coordinate))

(** [populate_black row col] paints the area at [row], [col] black *)
let populate_black row col = black

let small_black_matrix = init 10 10 populate_black

(** [colorize_test name f m expected_output] is an OUnit test named [name] that
    checks equivalence between [exected_output] and [colorize f m] *)
let colorize_test name f m expected_output = 
  name >:: (fun _ -> assert_equal expected_output (colorize f m))

let toImage_tests = [
  julia_color_test "Point is colored black" 
    5000 
    red
    (None, {re = 0.; im = 0.}) 
    black;
  julia_color_test "Point is colored black" 
    5000 
    blue
    (None, {re = 0.; im = 0.}) 
    black;
  julia_color_test "Point is colored black" 
    5000 
    green
    (None, {re = 0.; im = 0.}) 
    black;

  julia_color_test "Point is colored black" 
    5000 
    light_blue
    (None, {re = 0.; im = 0.}) 
    black;

  julia_color_test "Point is colored black" 
    5000 
    dark_red
    (None, {re = 0.; im = 0.}) 
    black;

  julia_color_test "Point is colored black" 
    5000 
    light_green
    (None, {re = 0.; im = 0.}) 
    black;
]


(******************************************************************************)
(*Tests for Main*)                                                     
(******************************************************************************)
(*
let complex_of_float_test name f expected_output = 
  name >:: fun _ -> (assert_equal expected_output (Main.complex_of_float f))

let lst_of_complex_floats_test name lst expected_output = 
  name >:: fun _ -> (assert_equal expected_output (Main.lst_of_complex_floats lst))

let string_of_rgb_test name str expected_output = 
  name >:: fun _ -> (assert_equal expected_output (Main.string_of_rgb str))

let string_of_rgb_exc_test name str expected_output = 
  name >:: fun _ -> (assert_raises expected_output 
                       (fun () -> Main.string_of_rgb str))

let main_tests = [
  (*Testing complex_of_float*)
  complex_of_float_test "complex_of_float 0. should be {re = 0.; im = 0.}"
    0.
    {re = 0.; im = 0.};
  complex_of_float_test "complex_of_float 1. should be {re = 0.; im = 0.}"
    1.
    {re = 1.; im = 0.};
  complex_of_float_test "complex_of_float 0.1 should be {re = 0.1; im = 0.}"
    0.1
    {re = 0.1; im = 0.};
  complex_of_float_test "complex_of_float -0.1 should be {re = -0.1; im = 0.}"
    (-0.1)
    {re = -0.1; im = 0.};

  (*Testing lst_of_complex_floats*)
  lst_of_complex_floats_test {|lst_of_complex_floats "0.1" should be 
    [{re = 0.1; im = 0.}]|}
    "0.1"
    [{re = 0.1; im = 0.}]; 
  lst_of_complex_floats_test {|lst_of_complex_floats "0.1 0.2" should be 
    [{re = 0.1; im = 0.}; {re = 0.2; im = 0.}]|}
    "0.1 0.2"
    [{re = 0.1; im = 0.}; {re = 0.2; im = 0.}]; 
  lst_of_complex_floats_test {|lst_of_complex_floats "0.1      0.2" should be 
    [{re = 0.1; im = 0.}; {re = 0.2; im = 0.}]|}
    "0.1       0.2"
    [{re = 0.1; im = 0.}; {re = 0.2; im = 0.}]; 
  lst_of_complex_floats_test {|lst_of_complex_floats "     0.1 0.2" should be 
    [{re = 0.1; im = 0.}; {re = 0.2; im = 0.}]|}
    "      0.1 0.2"
    [{re = 0.1; im = 0.}; {re = 0.2; im = 0.}];
  lst_of_complex_floats_test {|lst_of_complex_floats "0.1 0.2      " should be 
    [{re = 0.1; im = 0.}; {re = 0.2; im = 0.}]|}
    "0.1 0.2       "
    [{re = 0.1; im = 0.}; {re = 0.2; im = 0.}];

  (*testing string_of_rgb*)
  string_of_rgb_test {|string_of_rgb "R" should be {b = 39; r = 234; g = 32}|}
    "R"
    {b = 39; r = 234; g = 32}; 
  string_of_rgb_test {|string_of_rgb "V" should be {b = 241; r = 205; g = 132}|}
    "R"
    {b = 241; r = 205; g = 132};

  (*testing string_of_rgb_exc_test*)
  string_of_rgb_exc_test {|string_of_rgb "Q" should raise Color_not_found|}
    "Q"
    Main.Color_not_found;
]
*)


(******************************************************************************)
(*Tests for LineDrawer: We test this visually, so no unit tests here*)                                        
(******************************************************************************)

(******************************************************************************)
(*Tests for Newton*)                                                      
(******************************************************************************)
open Newton

let identity_f (x : Complex.t) : Complex.t = x

let deriv_identity_f (x : Complex.t) = {re = 1.; im = 0.}

let root_identity_f = [{re = 0.; im = 0.}]

let x3minx = 
  from_list [Complex.one; Complex.zero; Complex.(neg one); Complex.zero] 
  |> Polynomial.eval

let diffx3minx = 
  from_list [{re = 3.; im = 0.}; Complex.zero; Complex.(neg one)]
  |> Polynomial.eval

let rootx3minx = [Complex.one; 
                  Complex.zero;
                  Complex.(neg one)]

let tolerance_identity_f = 1.

let cmp z1 z2 = Complex.norm (Complex.sub z1 z2) < 1e-15

(** [newton_fun_test name f f' roots tolerance z expected_output] is an OUnit
    test named [name] that asserts equality between [expected_output] and 
    [newton_fun f f' roots tolerance z]*)
let newton_fun_test name f f' roots tolerance z expected_output = 
  name >:: fun _ ->
    assert_equal
      expected_output 
      (newton_fun f f' roots tolerance z) 
      ~printer:(function 
          |None -> "None"
          |Some v -> str_complex v)
      ~cmp:(fun z w -> 
          match z, w with
          | Some a, Some b -> cmp a b 
          | Some a, None | None, Some a -> false
          | None, None -> true)

let newton_fun_test_no_stop name f f' z expected_output = 
  name >:: fun _ ->
    assert_equal
      expected_output
      (newton_fun_no_stop f f' z)
      ~printer:str_complex
      ~cmp:cmp


let newton_tests = [
  newton_fun_test 
    "Newton's method on the identity starting at 0 has converged"
    identity_f
    deriv_identity_f
    root_identity_f
    tolerance_identity_f
    Complex.zero
    None;

  newton_fun_test_no_stop 
    "Newton's method on the identity starting at 0 goes to 0"
    identity_f
    deriv_identity_f
    Complex.zero
    Complex.zero;

  newton_fun_test 
    "Newton's method on the identity starting at 1+1i goes to 0 immediately"
    identity_f
    deriv_identity_f
    root_identity_f
    tolerance_identity_f
    {re = 1.; im = 1.}
    (Some Complex.zero);

  newton_fun_test_no_stop
    "Newton's method on the identity starting at 1+1i goes to 0 no stop"
    identity_f
    deriv_identity_f
    {re = 1.; im = 1.}
    Complex.zero;

  newton_fun_test 
    "Newton's method on the identity starting at 100+100i goes to 0 immediately"
    identity_f
    deriv_identity_f
    root_identity_f
    tolerance_identity_f
    {re = 100.; im = 100.}
    (Some Complex.zero);

  newton_fun_test_no_stop
    "Newton's method on the identity starting at 100+100i goes to 0 no stop"
    identity_f
    deriv_identity_f
    {re = 100.; im = 100.}
    Complex.zero;

  newton_fun_test_no_stop
    "Newton's method on the identity starting at -1-i goes to 0 no stop"
    identity_f
    deriv_identity_f
    {re = -1.; im = -1.}
    Complex.zero;

  newton_fun_test 
    "Newton's method on the identity starting at 10k+10ki goes to 0 immediately"
    identity_f
    deriv_identity_f
    root_identity_f
    tolerance_identity_f
    {re = 10000.; im = 10000.}
    (Some Complex.zero);

  newton_fun_test_no_stop
    "Newton's method on the identity starting at 10k+10ki goes to 0 no stop"
    identity_f
    deriv_identity_f
    {re = 10000.; im = 10000.}
    Complex.zero;

  newton_fun_test
    "Newton's method on x^3-x starting at i for one step"
    x3minx
    diffx3minx
    rootx3minx
    0.01
    Complex.i 
    (Some {re = 0.; im = 0.5});

  newton_fun_test_no_stop
    "Newton's method on x^3-x starting at i for one step no stop"
    x3minx
    diffx3minx
    Complex.i 
    {re = 0.; im = 0.5};

  newton_fun_test 
    "Newton's method on x^3-x starting at 1+i"
    x3minx
    diffx3minx
    rootx3minx
    0.01
    Complex.(add one i)
    (let x = Complex.(add one i) in 
     (Some Complex.(sub x (div (x3minx x) (diffx3minx x) ))));

  newton_fun_test_no_stop
    "Newton's method on x^3-x starting at 1+i no stop"
    x3minx
    diffx3minx
    Complex.(add one i)
    (let x = Complex.(add one i) in 
     (Complex.(sub x (div (x3minx x) (diffx3minx x)))));

  newton_fun_test
    "0.001 is within 0.1 of 0"
    x3minx
    diffx3minx
    rootx3minx
    0.01
    {re = 0.001; im = 0.}
    None;

  newton_fun_test_no_stop
    "Newton of x^3-x starting at 0.001"
    x3minx
    diffx3minx
    {re = 0.001; im = 0.}
    {re = 0.001 -. (0.001 ** 3. -. 0.001) /. (3. *. 0.001 ** 2. -. 1.); im = 0.};

  newton_fun_test
    "Newton's method on x^3-x starting at 0.1"
    x3minx
    diffx3minx
    rootx3minx
    0.1
    {re = 0.1; im = 0.}
    (Some {re = 0.1 -. (0.1 ** 3. -. 0.1) /. (3. *. 0.1 ** 2. -. 1.); im = 0.});

  newton_fun_test_no_stop
    "Newton's method on x^3-x starting at 0.1 no stop"
    x3minx
    diffx3minx
    {re = 0.1; im = 0.}
    {re = 0.1 -. (0.1 ** 3. -. 0.1) /. (3. *. 0.1 ** 2. -. 1.); im = 0.}

]

(******************************************************************************)
(*Tests for Parse*)
(******************************************************************************)
open Parse 

let check_raise name ex f = 
  name >:: (fun _ -> assert_raises ex f)

let parse_tests = [
  check_eq "Complex neg i" {re = -1.; im = -1.} (complex_of_string "-1 + -i") 
    str_complex;
  check_eq "Complex one" "1. + 0.i" (string_of_complex Complex.one) pp_string;
  check_eq "1 from string 1" Complex.one (complex_of_string "1") str_complex;
  check_eq "1 from string 1+0i" Complex.one (complex_of_string "1+0i") 
    str_complex;
  check_eq "1 from string 1 + 0.i" Complex.one (complex_of_string "1 + 0.i") 
    str_complex;
  check_eq "7 + 46i from string" {re = 7.; im = 46.} 
    (complex_of_string "7+ 46i") str_complex;
  check_eq "i from string" {re = 0.; im = 1.} (complex_of_string "i") 
    str_complex;
  check_raise "7i8 is not a complex number" (Failure "invalid") 
    (fun () -> complex_of_string "7i8");
  check_raise "hello is not a complex number" (Failure "float_of_string") 
    (fun () -> complex_of_string "hello");
  check_raise "the empty string is not a complex number" 
    (Failure "float_of_string") 
    (fun () -> complex_of_string "");
  check_raise {|"hello" is not a complex number|} (Failure "float_of_string") 
    (fun () -> complex_of_string {|"hello"|});
  check_raise "1    i is not a complex number" (Failure "float_of_string")
    (fun () -> complex_of_string "1    i");
  check_eq {|complex numbers from string|} 
    [Complex.one; Complex.i; Complex.zero; Complex.one; {re = 7.; im = 0.26}]
    (lst_cx "1+ 0i, 0+1.i,0., 1, 7+ 0.26i") (pp_list str_complex);
  check_eq "empty list" [] (lst_cx "") (pp_list str_complex);
  check_raise "bad number in list" (Failure "float_of_string") 
    (fun () -> lst_cx {|1+7i, "hello"|});
  check_raise "10 + 8i + 7 fails" (Failure "invalid") 
    (fun () -> complex_of_string "10 + 8i + 7");
  check_raise "10 + +8i fails" (Failure "invalid") 
    (fun () -> complex_of_string "10 + +8i");
  check_raise "6 - 8 + i fails" (Failure "invalid")
    (fun () -> complex_of_string "6 - 8 + i");
  check_raise "3 real pieces h&h raises invalid" (Failure "invalid")
    (fun () -> complex_of_string "-6 - 8 + 2");
  check_raise "3 real pieces raises invalid" (Failure "invalid")
    (fun () -> complex_of_string "6 + 8 + 2");
  check_raise "3 real pieces all n raises invalid" (Failure "invalid")
    (fun () -> complex_of_string "-6 - 8 - 2");
  check_raise "two i's separated" (Failure "invalid")
    (fun () -> complex_of_string "-2-i+i");
  check_eq "minus instead of plus" {re = -2.; im = -1.} 
    (complex_of_string "-2-i") str_complex;
]


let tests =
  "test suite for Final Project"  >::: List.flatten [
    matrix_tests;
    polynomial_tests;
    toImage_tests;
    (*main_tests;*)
    newton_tests;
    parse_tests;
    rand_p_tests
  ]

let _ = run_test_tt_main tests
