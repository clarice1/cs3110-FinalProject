open OUnit2
(* If you get an "unbound module" error from the line below,
   it's most likely because you have not (re)compiled [enigma.ml]. 
   To do that, run [make build]. *)
open Matrix

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_int] pretty-prints int [x]. *)
let pp_int x = pp_string (string_of_int x)

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

let str_complex (z : Complex.t) = 
  pp_string (string_of_float z.re ^ " + "^ string_of_float z.im ^ "i")

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
  check_eq name expected_out (Matrix.iterate_with_stop f n p m) printer

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

let string_option_int_matrix= 
  str_matrix (fun (n, x) -> match n, x with 
      | (None, x) -> "(None, " ^ pp_string (string_of_int x) ^ ")"
      | (Some n, x) -> 
        "(Some " ^ pp_int n ^ ", " ^ pp_int x ^ ")")

let matrix_tests = [
  check_eq "list representation of 0123 matrix" [[0; 2]; [1; 3]] 
    (Matrix.to_lst matrix_0123) (pp_listlist pp_int);

  matrix_test "zero_matrix at row 0, column 0 is 0" zero_matrix 0 0 
    pp_int 0;
  test_exception "zero matrix has no value at 10, 5" zero_matrix 10 5 
    (Invalid_argument "(10, 5)");
  test_exception "zero matrix has no value at 5, 10" zero_matrix 5 10 
    (Invalid_argument "(5, 10)");
  test_exception "quadratic_matrix has no value at 6, 7" quadratic_matrix 6 7
    (Invalid_argument "(6, 7)");

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

  let do_big_test = false in
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
]

let tests =
  "test suite for A1"  >::: List.flatten [
    matrix_tests
  ]

let _ = run_test_tt_main tests
