open Java_lib.Interpreter.ClassLoader (Java_lib.Interpreter.Result)

open Java_lib.Interpreter.Main (Java_lib.Interpreter.Result)

open Java_lib.Parser
open Java_lib.Interpreter
open Java_lib.Ast
open Java_lib.Hashtbl_p

let print_hashtbl ht pp_k pp_el = pp pp_k pp_el Format.std_formatter ht

let ppb_key_t buf k = Buffer.add_string buf k

let ppb_var buf v = Buffer.add_string buf (show_variable v)

let show_hashtbl ht pp_k pp_v =
  match Hashtbl.length ht with
  | 0 -> "[[]]"
  | _ ->
      let buf = Buffer.create 10000 in
      Printf.bprintf buf "[[";
      Hashtbl.iter (fun k v -> Printf.bprintf buf "%a -> %a\n" pp_k k pp_v v) ht;
      Printf.bprintf buf "]]";
      Buffer.contents buf

let show_hashtbl_values ht show_v =
  let buf = Buffer.create 10000 in
  Hashtbl.iter
    (fun _ v ->
      Buffer.add_string buf (show_v v);
      Buffer.add_string buf "\n")
    ht;
  Buffer.contents buf

let std_classes_key =
  {|
 class BubbleSorter {
    public void sort(int[] arr, int n) {
    	int i = 0;
    	while (i < n - 1) {
    		int j = 0;
    		while (j < n - i - 1) {
    			if (arr[j] > arr[j + 1]) {
               		int temp = arr[j];
                    		arr[j] = arr[j + 1];
                    		arr[j + 1] = temp;
                	}
                	j = j + 1;
    		}  
    		i = i + 1;
    	}
    }
}

class QuickSorter {
    public void quickSort(int [] array, int n, int low, int high) {
        if (n == 0)
            return;
        if (low >= high)
            return;
        int middle = low + (high - low) / 2;
        int pivot = array[middle];
        int i = low, j = high;
        while (i <= j) {
            while (array[i] < pivot) {
                i++;
            }
            while (array[j] > pivot) {
                j--;
            }
            if (i <= j) {
                int temp = array[i];
                array[i] = array[j];
                array[j] = temp;
                i++;
                j--;
            }
        }
        if (low < j)
            quickSort(array, n, low, j);

        if (high > i)
            quickSort(array, n, i, high);
    }
}

abstract class Figure {
	public final int PI = 3;
	abstract int area();
	abstract int perimeter(); 
}

class Circle extends Figure {
	public int radius;
	
	public Circle(int radius) {
		this.radius = radius;
	}
	
	public int area() {
		return PI * radius * radius;
	} 
	public int perimeter() {
		return 2 * PI * radius;
	}
}

class Rectangle extends Figure {
	public int a, b;
	
	public Rectangle(int a, int b) {
		this.a = a;
		this.b = b;
	}
	
	public int area() {
		return a * b;
	} 
	public int perimeter() {
		return 2 * (a + b);
	}
}

class Triangle extends Figure {
	public int a, b, c;
	
	public Triangle(int a, int b, int c) {
		this.a = a;
		this.b = b;
		this.c = c;
	}
	
	public int area() {
		int p = (a + b + c) / 2;
        	return p * (p - a) * (p - b) * (p - c);
	} 
	public int perimeter() {
		return a + b + c;
	}

}

class Factorial {
    public int getFact(int n) {
        if (n <= 1) return 1;
        else return n * getFact(n - 1);
    }
}

class Fibonacci {
		public int fib(int n) {
				if (n <= 1) {
					return 0;
				}
				else if (n == 2) {
					return 1;
				} 
				else {
					return fib(n - 1) + fib(n - 2);
				} 
		} 
}

class Repl {	
}
  
  |}

let std_val_o = apply parser std_classes_key

let is_end_of_input test_str =
  let r = Str.regexp {|.*@[ ]*$|} in
  Str.string_match r test_str 0

let str_before_delim str =
  match String.rindex_opt str '@' with
  | Some pos -> Str.string_before str pos
  | None -> raise (Invalid_argument "Input string must have '@' character!")

exception Invalid_input of string

exception Invalid_state of string

exception Evaluation_error of string

let rec repl buffer class_table ctx m_key_table =
  print_string "> ";
  let new_str = read_line () in
  match is_end_of_input new_str with
  | false ->
      Buffer.add_string buffer new_str;
      repl buffer class_table ctx m_key_table
  | true -> (
      Buffer.add_string buffer (str_before_delim new_str);
      let process_exn ts =
        (* Пытаемся попарсить метод *)
        match apply method_declaration ts with
        | Some (Method (m_type, Name name, args, body)) -> (
            let key = make_method_key name args in
            if body = None then (
              Buffer.clear buffer;
              raise (Invalid_input "Repl method cannot be abstract!") )
            else
              match Hashtbl.find_opt class_table "Repl" with
              | None ->
                  Buffer.clear buffer;
                  raise (Invalid_state "Repl class not found")
              | Some repl_r ->
                  let new_m =
                    {
                      m_type;
                      is_abstract = false;
                      is_overridable = false;
                      is_overriden = false;
                      has_override_annotation = false;
                      args;
                      key;
                      body;
                    }
                  in
                  if not (Hashtbl.mem repl_r.method_table key) then (
                    Hashtbl.add m_key_table key ts;
                    Hashtbl.add repl_r.method_table key new_m )
                  else (
                    Hashtbl.replace m_key_table key ts;
                    Hashtbl.replace repl_r.method_table key new_m );
                  Hashtbl.replace class_table "Repl"
                    { repl_r with method_table = repl_r.method_table };
                  print_endline "Method added";
                  Buffer.clear buffer;
                  repl buffer class_table ctx m_key_table )
        | Some _ ->
            Buffer.clear buffer;
            raise (Invalid_state "Not method declaration after parsing")
        (* Не попарсили метод - пытаемся попарсить statement *)
        | None -> (
            match apply Stmt.statement ts with
            | Some stmt -> (
                match eval_stmt stmt ctx class_table with
                | Error m -> raise (Evaluation_error m)
                | Ok stmt_evaled_ctx ->
                    print_endline "Statement evaluated";
                    Buffer.clear buffer;
                    repl buffer class_table stmt_evaled_ctx m_key_table )
            (* Не попарсили statement - пытаемся попарсить выражение *)
            | None -> (
                match apply Expr.expression ts with
                | Some expr -> (
                    match eval_expr expr ctx class_table with
                    | Error m ->
                        Buffer.clear buffer;
                        raise (Evaluation_error m)
                    | Ok expr_evaled_ctx ->
                        print_endline
                          ( "Result: "
                          ^ show_value expr_evaled_ctx.last_expr_result );
                        Buffer.clear buffer;
                        repl buffer class_table expr_evaled_ctx m_key_table )
                | None ->
                    Buffer.clear buffer;
                    raise (Invalid_input "Wrong input, try again!") ) )
      in
      match str_before_delim new_str with
      | "show_var_table" ->
          print_endline "Current table of variables:";
          print_endline (show_hashtbl ctx.var_table ppb_key_t ppb_var);
          Buffer.clear buffer;
          repl buffer class_table ctx m_key_table
      | "exit" -> ()
      | "show_curr_stdlib" ->
          print_endline "Current classes in stdlib:";
          print_endline std_classes_key;
          Buffer.clear buffer;
          repl buffer class_table ctx m_key_table
      | "show_available_methods" -> (
          match Hashtbl.find_opt class_table "Repl" with
          | None -> print_endline "Repl class not found"
          | Some _ ->
              print_endline "Current available methods:";
              print_endline (show_hashtbl_values m_key_table show_key_t);
              Buffer.clear buffer;
              repl buffer class_table ctx m_key_table )
      | _ -> (
          try process_exn (Buffer.contents buffer) with
          | Invalid_state m -> print_endline m
          | Invalid_input m | Evaluation_error m ->
              print_endline m;
              Buffer.clear buffer;
              repl buffer class_table ctx m_key_table ) )

let () =
  print_endline
    "|||------------------ Welcome to Java Repl! ------------------|||"

let () = print_endline "You can input statement, expression, or new method."

let () =
  print_endline "In the end of your input you need to type '@' character."

let () =
  print_endline
    "Special commands:\n\
    \ --- show_var_table@ for showing all information about local variables\n\
    \ --- show_curr_stdlib@ for showing current available classes in standart \
     library\n\
    \ --- show_available_methods@ for showing current available methods\n\
    \ --- exit@ for exiting REPL"

let start =
  match std_val_o with
  | None -> print_endline "Syntax Error! \n"
  | Some std_val -> (
      match load std_val (Hashtbl.create 1000) with
      | Error m -> print_endline ("Error in loading standart library: " ^ m)
      | Ok load_table ->
          let new_buf = Buffer.create 1000 in
          let repl_ctx =
            {
              cur_object =
                RObj
                  {
                    class_key = "Repl";
                    field_ref_table = Hashtbl.create 100;
                    number = 0;
                  };
              var_table = Hashtbl.create 100;
              last_expr_result = VVoid;
              runtime_signal = NoSignal;
              curr_method_type = Void;
              is_main_scope = true;
              nested_loops_cnt = 0;
              scope_level = 0;
              cur_constr_key = None;
              prev_context = None;
              obj_created_cnt = 0;
              is_creation = false;
              constr_affilation = None;
            }
          in
          let mk_table = Hashtbl.create 1000 in
          repl new_buf load_table repl_ctx mk_table )
