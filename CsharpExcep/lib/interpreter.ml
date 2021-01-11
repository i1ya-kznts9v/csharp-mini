open Ast
open Parser
open Hashtbl_der
open Operators
open Extractors
open Printf

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >> ) : 'a t -> 'b t -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let return = Result.ok
  let error = Result.error
  let ( >> ) x f = x >>= fun _ -> f
end

type table_key = string [@@deriving show]

type table_constructor =
  {key: table_key; args: (data_type * string) list; body: statement}
[@@deriving show {with_path= false}]

type table_field =
  { field_type: data_type
  ; field_key: table_key
  ; is_const: bool
  ; sub_tree: expr option }
[@@deriving show {with_path= false}]

type table_method =
  { method_type: data_type
  ; has_override: bool
  ; has_static_mod: bool
  ; method_key: table_key
  ; args: (data_type * string) list
  ; body: statement }
[@@deriving show {with_path= false}]

type table_class =
  { class_key: table_key
  ; field_table: (table_key, table_field) Hashtbl_der.t
  ; method_table: (table_key, table_method) Hashtbl_der.t
  ; constructor_table: (table_key, table_constructor) Hashtbl_der.t
  ; parent_key: table_key option
  ; children_keys: table_key list
  ; dec_tree: cs_class }
[@@deriving show {with_path= false}]

let find_substr all_str sub_str =
  if String.length sub_str > String.length all_str then false
  else
    let sub = String.sub all_str 0 (String.length sub_str) in
    String.equal sub sub_str

let convert_table_to_seq = Hashtbl.to_seq_values

(*because we work with delayed list*)
let get_seq_hd seq =
  match seq () with Seq.Nil -> raise Not_found | Seq.Cons (el, _) -> el

let type_of_list = List.map fst

let method_make_key m_name args =
  String.concat "" (m_name :: List.map show_data_type (type_of_list args))

let constructor_make_key cl args =
  String.concat "" (cl :: List.map show_data_type (type_of_list args))

let to_string_key = method_make_key "ToString" []

module ClassLoader (M : MONADERROR) = struct
  open M

  let is_override = List.mem Override
  let is_static = List.mem Static
  let is_public = List.mem Public
  let check_const = List.mem Const

  let get_elem hashtable key =
    match Hashtbl_der.get_value_option hashtable key with
    | None -> error "Element dont found in table!"
    | Some elem -> return elem

  let replace_elem_hash_table hashtable key value =
    Hashtbl.replace hashtable key value ;
    return hashtable

  let rec monadic_list_iter list action base =
    match list with
    | [] -> return base
    | x :: xs -> action x >> monadic_list_iter xs action base

  let rec monadic_seq_iter seq action base =
    match seq () with
    | Seq.Nil -> return base
    | Seq.Cons (x, del_tail) ->
        action x >> monadic_seq_iter del_tail action base

  let system_exception_init hashtable =
    let constructor_table = Hashtbl.create 16 in
    let field_table = Hashtbl.create 16 in
    let method_table = Hashtbl.create 16 in
    let get_body =
      match
        apply_parser Stat.stat_block
          {| 
              {
                return message;
              }


          |}
      with
      | None -> error "Error parsing ToString method in Exception class"
      | Some bd -> return bd in
    get_body
    >>= fun body ->
    let to_string : table_method =
      { method_type= String
      ; has_override= true
      ; has_static_mod= false
      ; method_key= to_string_key
      ; args= []
      ; body } in
    let message : table_field =
      {field_type= String; field_key= "message"; is_const= false; sub_tree= None}
    in
    let get_dec =
      match
        apply_parser parse_class
          {|
          public class Exception 
          {
            public string message;
            public string ToString()
            {
                return message;
            }
          }
      |}
      with
      | None -> error "Error in parsing Exception class"
      | Some tree -> return tree in
    get_dec
    >>= fun dec_tree ->
    Hashtbl.add method_table to_string_key to_string ;
    Hashtbl.add field_table "message" message ;
    Hashtbl.add hashtable "Exception"
      { class_key= "Exception"
      ; field_table
      ; method_table
      ; constructor_table
      ; parent_key= None
      ; children_keys= []
      ; dec_tree } ;
    return hashtable

  let field_modifiers_check pair =
    match pair with
    | l, f -> (
      match f with
      | Method (Void, "Main", [], _)
        when is_static l && (not (check_const l)) && not (is_override l) ->
          return ()
      | Method (_, "Main", _, _) ->
          error "Only one main method can be in program!"
      | Method (_, _, _, _) when check_const l ->
          error "Method can not be const"
      | Method (_, _, _, _) -> return ()
      | VariableField (_, _) when (not (is_static l)) && not (is_override l) ->
          return ()
      | VariableField (_, _) -> error "Wrong modifiers"
      | Constructor (_, _, _)
        when (not (is_static l))
             && (not (check_const l))
             && (not (is_override l))
             && is_public l ->
          return ()
      | Constructor (_, _, _) -> error "Wrong constructor modifiers" )

  let class_modifiers_check = function
    | Class (ml, _, _, _)
      when (not (is_static ml))
           && (not (is_override ml))
           && not (check_const ml) ->
        return ()
    | Class (_, _, _, _) -> error "Wrong class modifiers"

  let add_default_constructor hashtable =
    Hashtbl.iter
      (fun key some_class ->
        if Hashtbl.length some_class.constructor_table = 0 then
          let cl_key = constructor_make_key key [] in
          Hashtbl.add some_class.constructor_table cl_key
            {key= cl_key; args= []; body= StatementBlock []})
      hashtable ;
    return hashtable

  let class_adding class_list hastable =
    let add_to_table hashtable key value message =
      match get_value_option hashtable key with
      | None ->
          Hashtbl.add hashtable key value ;
          return hashtable
      | _ -> error message in
    let add_class_table hashtable adding_class =
      match adding_class with
      | Class (_, class_key, parent, fields) ->
          let method_table = Hashtbl.create 1024 in
          let field_table = Hashtbl.create 1024 in
          let constructor_table = Hashtbl.create 1024 in
          class_modifiers_check adding_class
          >>
          let add_class_elem : modifier list * field -> unit M.t =
           fun field_elem ->
            match field_elem with
            | mod_list, VariableField (field_type, arg_list) ->
                let rec add_var_field = function
                  | [] -> return ()
                  | (field_key, sub_tree) :: ps ->
                      let is_const = check_const mod_list in
                      add_to_table field_table field_key
                        {field_type; field_key; is_const; sub_tree}
                        "Similar fields"
                      >> add_var_field ps in
                field_modifiers_check field_elem >> add_var_field arg_list
            | mod_list, Method (method_type, m_name, args, body) ->
                let method_key = method_make_key m_name args in
                let has_override = is_override mod_list in
                let has_static_mod = is_static mod_list in
                field_modifiers_check field_elem
                >> add_to_table method_table method_key
                     { method_type
                     ; has_override
                     ; has_static_mod
                     ; method_key
                     ; args
                     ; body }
                     "Method with this type exists"
                >> return ()
            | _, Constructor (name, args, body) ->
                let make_key = constructor_make_key name args in
                let check_name =
                  if name = class_key then return ()
                  else error "Constructor name error" in
                check_name
                >> field_modifiers_check field_elem
                >> add_to_table constructor_table make_key
                     {key= make_key; args; body}
                     "Constructor with this type exists"
                >> return () in
          let add_parent p = match p with None -> None | _ -> p in
          let parent_key = add_parent parent in
          monadic_list_iter fields add_class_elem ()
          >> add_to_table hashtable class_key
               { class_key
               ; field_table
               ; method_table
               ; constructor_table
               ; parent_key
               ; children_keys= []
               ; dec_tree= adding_class }
               "Similar Classes" in
    monadic_list_iter class_list (add_class_table hastable) hastable

  let update_exception_class_childs hashtable =
    let helper : table_class -> table_class M.t =
     fun some_class ->
      match some_class.parent_key with
      | None -> return some_class
      | Some key -> (
          let parent = get_value_option hashtable key in
          let exception_is_parent = String.compare "Exception" in
          match parent with
          | None ->
              error "The class can only be inherited from the Exception class!"
          | Some parent_o when exception_is_parent parent_o.class_key = 0 ->
              (*CHECK THIS!*)
              let new_val =
                { parent_o with
                  children_keys= some_class.class_key :: parent_o.children_keys
                } in
              replace_elem_hash_table hashtable key new_val >> return new_val
          | Some _ ->
              error "The class can only be inherited from the Exception class!"
          ) in
    monadic_seq_iter (convert_table_to_seq hashtable) helper hashtable

  let transfer_fields parent children =
    let exception_transfer_field : table_class -> table_field -> unit t =
     fun child_class p_field ->
      match get_value_option child_class.field_table p_field.field_key with
      | None ->
          return (Hashtbl.add child_class.field_table p_field.field_key p_field)
      | _ -> return () in
    monadic_seq_iter
      (convert_table_to_seq parent.field_table)
      (exception_transfer_field children)
      ()

  let transfer_methods parent children =
    let exception_transfer_method : table_class -> table_method -> unit t =
     fun child_class p_method ->
      match get_value_option child_class.method_table p_method.method_key with
      | None ->
          return
            (Hashtbl.add child_class.method_table p_method.method_key p_method)
      | _ -> return () in
    monadic_seq_iter
      (convert_table_to_seq parent.method_table)
      (exception_transfer_method children)
      ()

  let check_override_mod parent children =
    let check : table_class -> table_method -> unit t =
     fun parent child_method ->
      match child_method.has_override with
      | false -> return ()
      | true -> (
        match get_value_option parent.method_table child_method.method_key with
        | None ->
            error "Not overriden method or parent does not exist this method!"
        | _ -> return () ) in
    monadic_seq_iter
      (convert_table_to_seq children.method_table)
      (check parent) ()

  let transfer_to_child parent children =
    transfer_fields parent children
    >> transfer_methods parent children
    >> check_override_mod parent children

  let begin_inheritance_from_exception hashtable =
    get_elem hashtable "Exception"
    >>= fun ex_cl ->
    let helper child_key =
      get_elem hashtable child_key
      >>= fun child_c -> transfer_to_child ex_cl child_c in
    monadic_list_iter ex_cl.children_keys helper hashtable

  let load_classes class_list class_table =
    match class_list with
    | [] ->
        error
          "No class found, you may have submitted an empty file or it's syntax \
           error in input"
    | _ ->
        system_exception_init class_table
        >>= fun table ->
        class_adding class_list table
        >>= fun table_with_classes ->
        add_default_constructor table_with_classes
        >>= fun table_update ->
        update_exception_class_childs table_update
        >>= fun new_table -> begin_inheritance_from_exception new_table
end

module Interpreter (M : MONADERROR) = struct
  open M

  type variable =
    { var_key: table_key
    ; var_type: data_type
    ; var_value: value
    ; is_const: bool
    ; assignment_count: int
    ; visibility_level: int }
  [@@deriving show {with_path= false}]

  type flag = WasBreak | WasContinue | WasReturn | WasThrown | NoFlag
  [@@deriving show {with_path= false}]

  type context =
    { current_o: obj_ref
    ; variable_table: (table_key, variable) Hashtbl_der.t
    ; current_meth_type: data_type
    ; last_expr_result: value
    ; runtime_flag: flag
    ; is_main: bool
    ; curr_constructor: table_key option
    ; count_of_nested_cycle: int
    ; visibility_level: int
    ; prev_ctx: context option
    ; count_of_obj: int
    ; is_creation: bool }
  [@@deriving show {with_path= false}]

  let context_init current_o variable_table =
    return
      { current_o
      ; variable_table
      ; current_meth_type= Void
      ; last_expr_result= VVoid
      ; runtime_flag= NoFlag
      ; is_main= true
      ; curr_constructor= None
      ; count_of_nested_cycle= 0
      ; visibility_level= 0
      ; prev_ctx= None
      ; count_of_obj= 0
      ; is_creation= false }

  let rec fold_left2 func acc l1 l2 =
    match (l1, l2) with
    | [], [] -> return acc
    | x :: xs, y :: ys -> func acc x y >>= fun res -> fold_left2 func res xs ys
    | _, _ -> error "Incorrect lists for fold_left2"

  let get_elem hashtable key =
    match Hashtbl_der.get_value_option hashtable key with
    | None -> error "Element dont found in table!"
    | Some elem -> return elem

  let find_main_class hashtable =
    Hashtbl_der.filter hashtable (fun _ cl ->
        Hashtbl.mem cl.method_table "Main")
    |> fun filter_hasht ->
    match Hashtbl.length filter_hasht with
    | 0 -> error "Must be one main method"
    | 1 -> return (get_seq_hd (convert_table_to_seq filter_hasht))
    | _ -> error "Must be one main method"

  let obj_num obj =
    try get_obj_num obj |> fun n -> return n
    with Invalid_argument m -> error m

  let rec expression_check cur_expr ctx class_table =
    match cur_expr with
    | Add (left, right) -> (
        expression_check left ctx class_table
        >>= fun left_type ->
        match left_type with
        | Int -> (
            expression_check right ctx class_table
            >>= fun right_type ->
            match right_type with
            | Int -> return Int
            | String ->
                return String (*because we can write this: string a = 3 + "b";*)
            | _ -> error "Incorrect type: it must be Int or String!" )
        | String -> (
            expression_check right ctx class_table
            >>= fun right_type ->
            match right_type with
            | Int | String -> return String
            | _ -> error "Incorrect type: it must be Int or String!" )
        | _ -> error "Incorrect type: it must be Int or String!" )
    | Sub (left, right)
     |Div (left, right)
     |Mod (left, right)
     |Mul (left, right) -> (
        expression_check left ctx class_table
        >>= fun left_type ->
        match left_type with
        | Int -> (
            expression_check right ctx class_table
            >>= fun right_type ->
            match right_type with
            | Int -> return Int
            | _ -> error "Incorrect type: it must be Int!" )
        | _ -> error "Incorrect type: it must be Int!" )
    | PostDec value | PostInc value | PrefDec value | PrefInc value -> (
        expression_check value ctx class_table
        >>= fun value_type ->
        match value_type with
        | Int -> return Int
        | _ -> error "Incorrect type: it must be Int!" )
    | And (left, right) | Or (left, right) -> (
        expression_check left ctx class_table
        >>= fun left_type ->
        match left_type with
        | Bool -> (
            expression_check right ctx class_table
            >>= fun right_type ->
            match right_type with
            | Bool -> return Bool
            | _ -> error "Incorrect type: it must be Bool!" )
        | _ -> error "Incorrect type: it must be Bool!" )
    | Not value -> (
        expression_check value ctx class_table
        >>= fun value_type ->
        match value_type with
        | Bool -> return Bool
        | _ -> error "Incorrect type: it must be Bool!" )
    | Less (left, right)
     |More (left, right)
     |LessOrEqual (left, right)
     |MoreOrEqual (left, right) -> (
        expression_check left ctx class_table
        >>= fun left_type ->
        match left_type with
        | Int -> (
            expression_check right ctx class_table
            >>= fun right_type ->
            match right_type with
            | Int -> return Bool
            | _ -> error "Incorrect type: it must be Int!" )
        | _ -> error "Incorrect type: it must be Int!" )
    | Equal (left, right) | NotEqual (left, right) -> (
        expression_check left ctx class_table
        >>= fun left_type ->
        match left_type with
        | Int -> (
            expression_check right ctx class_table
            >>= fun right_type ->
            match right_type with
            | Int -> return Bool
            | _ -> error "Incorrect type: it must be Int!" )
        | String -> (
            expression_check right ctx class_table
            >>= fun right_type ->
            match right_type with
            | String -> return Bool
            | _ -> error "Incorrect type: it must be String!" )
        | Bool -> (
            expression_check right ctx class_table
            >>= fun right_type ->
            match right_type with
            | Bool -> return Bool
            | _ -> error "Incorrect type: it must be Bool!" )
        | CsClass left_name -> (
            expression_check right ctx class_table
            >>= fun right_type ->
            match right_type with
            | CsClass right_name when left_name = right_name -> return Bool
            | CsClass "null" -> return Bool
            | _ -> error "Incorrect class type!" )
        | _ -> error "Incorrect type in equal-expression!" )
    | Null -> return (CsClass "null")
    | CallMethod (method_name, args) ->
        let obj_key =
          match ctx.current_o with
          | ObjNull -> "null"
          | ObjRef {class_key= key; _} -> key in
        get_elem class_table obj_key
        >>= fun curr_class ->
        method_verify curr_class method_name args ctx class_table
        >>= fun t_mth -> return t_mth.method_type
    | Access (calling_obj, IdentVar var_name) -> (
        expression_check calling_obj ctx class_table
        >>= fun obj_type ->
        match obj_type with
        | CsClass "null" -> error "NullReferenceException"
        | CsClass obj_key -> (
            get_elem class_table obj_key
            >>= fun found_obj ->
            let find_field = get_value_option found_obj.field_table var_name in
            match find_field with
            | None -> error "No matching field found"
            | Some found_field -> return found_field.field_type )
        | _ -> error "Invalid type: type must be reference" )
    | Access (calling_obj, CallMethod (m_name, args)) -> (
        expression_check calling_obj ctx class_table
        >>= fun obj_type ->
        match obj_type with
        | CsClass "null" -> error "NullReferenceException"
        | CsClass obj_key ->
            get_elem class_table obj_key
            >>= fun found_obj ->
            method_verify found_obj m_name args ctx class_table
            >>= fun found_mth -> return found_mth.method_type
        | _ -> error "Invalid type: type must be reference" )
    | ClassCreate (class_name, args) -> (
      match get_value_option class_table class_name with
      | None -> error ("Class not found" ^ class_name ^ "\n")
      | Some t_class -> (
        match args with
        | [] -> return (CsClass class_name)
        | _ ->
            constructor_verify t_class args ctx class_table
            >> return (CsClass class_name) ) )
    | IdentVar var_key -> (
        let find_variable = get_value_option ctx.variable_table var_key in
        match find_variable with
        | None -> (
          match ctx.current_o with
          | ObjRef {class_table= table; _} -> (
            match get_value_option table var_key with
            | None -> error "There is no such variable or field with that name!"
            | Some field -> return field.f_type )
          | ObjNull -> error "NullReferenceException" )
        | Some variable -> return variable.var_type )
    | ConstExpr value -> (
      match value with
      | VInt _ -> return Int
      | VBool _ -> return Bool
      | VString _ -> return String
      | VClass ObjNull -> return (CsClass "null")
      | VClass (ObjRef {class_key= key; _}) -> return (CsClass key)
      | _ -> error "Incorrect const expression type!" )
    | Assign (left, right) -> (
        expression_check left ctx class_table
        >>= fun left_type ->
        match left_type with
        | Void -> error "Can not assign to void"
        | CsClass left_key -> (
            expression_check right ctx class_table
            >>= fun right_type ->
            match right_type with
            | CsClass "null" -> return (CsClass left_key)
            | CsClass right_key ->
                assign_verify_polymorphic left_key right_key class_table
            | _ -> error "Incorrect type assigning!" )
        | _ ->
            expression_check right ctx class_table
            >>= fun right_type ->
            if left_type = right_type then return right_type
            else error "Incorrect type assigning!" )
    | _ -> error "Incorrect expression"

  and assign_verify_polymorphic left_key right_key class_table =
    (*For subtype polymorphism (inclusion polymorphism)*)
    match classname_verify_polymorphic left_key right_key class_table with
    | false -> error "Incorrect assign type"
    | true -> return (CsClass right_key)

  (*copy-paste*)
  (* let rec check_parent key =
       let find_class = Option.get (get_value_option class_table key) in
       if find_class.class_key = left_key then return (CsClass right_key)
       else
         match find_class.parent_key with
         | None -> error "Incorrect assign type"
         | Some parent_key -> check_parent parent_key in
     check_parent right_key *)
  and constructor_verify t_class args g_ctx class_table =
    let check_type : int -> data_type -> table_key -> table_constructor -> bool
        =
     fun position curr_type _ value ->
      match List.nth_opt value.args position with
      | None -> false
      | Some (f_type, _) -> (
        match curr_type with
        | CsClass "null" -> (
          match f_type with CsClass _ -> true | _ -> false )
        | CsClass cl_key -> (
          match f_type with
          | CsClass this_key ->
              classname_verify_polymorphic this_key cl_key class_table
          | _ -> false )
        | _ -> f_type = curr_type ) in
    let rec helper hashtable pos expr_list ctx =
      match Hashtbl.length hashtable with
      | 0 -> error "Constructor not found"
      | other -> (
        match expr_list with
        | [] -> (
          match other with
          | 1 -> return (get_seq_hd (convert_table_to_seq hashtable))
          | _ -> error "Constructor not recognized" )
        | x :: xs ->
            expression_check x ctx class_table
            >>= fun x_type ->
            helper
              (Hashtbl_der.filter hashtable (check_type pos x_type))
              (pos + 1) xs ctx ) in
    helper
      (Hashtbl_der.filter t_class.constructor_table (fun _ cr ->
           List.length cr.args = List.length args))
      0 args g_ctx

  and classname_verify_polymorphic left_key right_key class_table =
    (*For subtype polymorphism (inclusion polymorphism) while passing parameters*)
    let rec check_par key =
      match get_value_option class_table key with
      | None -> false
      | Some get_p -> (
          if get_p.class_key = left_key then true
          else
            match get_p.parent_key with
            | None -> false
            | Some pk -> check_par pk ) in
    check_par right_key

  and method_verify t_class m_name args g_ctx class_table =
    let check_type : int -> data_type -> table_key -> table_method -> bool =
     fun position curr_type _ value ->
      match List.nth_opt value.args position with
      | None -> false
      | Some (f_type, _) -> (
        match curr_type with
        | CsClass "null" -> (
          match f_type with CsClass _ -> true | _ -> false )
        | CsClass cl_key -> (
          match f_type with
          | CsClass this_key ->
              classname_verify_polymorphic this_key cl_key class_table
          | _ -> false )
        | _ -> f_type = curr_type ) in
    let rec helper hashtable pos expr_list ctx =
      match Hashtbl.length hashtable with
      | 0 -> error "Method not found"
      | other -> (
        match expr_list with
        | [] -> (
          match other with
          | 1 -> return (get_seq_hd (convert_table_to_seq hashtable))
          | _ -> error "Method not recognized" )
        | x :: xs ->
            expression_check x ctx class_table
            >>= fun x_type ->
            helper
              (Hashtbl_der.filter hashtable (check_type pos x_type))
              (pos + 1) xs ctx ) in
    (*для экономии*)
    Hashtbl_der.filter t_class.method_table (fun _ mr ->
        find_substr mr.method_key m_name)
    |> fun filter_name ->
    helper
      (Hashtbl_der.filter filter_name (fun _ mr ->
           List.length mr.args = List.length args))
      0 args g_ctx

  let make_list_of_elem el size =
    let rec helper acc curr =
      match curr with 0 -> acc | x -> helper (el :: acc) (x - 1) in
    helper [] size

  let inc_visibility_level ctx =
    {ctx with visibility_level= ctx.visibility_level + 1}

  let dec_visibility_level ctx =
    {ctx with visibility_level= ctx.visibility_level - 1}

  let delete_var_visibility : context -> context M.t =
   fun ctx ->
    let delete : table_key -> variable -> unit =
     fun key element ->
      if element.visibility_level = ctx.visibility_level then
        Hashtbl.remove ctx.variable_table key in
    Hashtbl.iter delete ctx.variable_table ;
    return ctx

  let check_assign_field : field_ref -> unit M.t =
   fun field ->
    match field.assignment_count with
    | 0 -> return ()
    | _ when not field.is_const -> return ()
    | _ -> error "Assigment to a constant field"

  let check_assign_variable : variable -> unit M.t =
   fun var ->
    match var.assignment_count with
    | 0 -> return ()
    | _ when not var.is_const -> return ()
    | _ -> error "Assigment to a constant variable"

  let expr_in_stat = function
    | PostDec _ | PostInc _ | PrefDec _ | PrefInc _
     |CallMethod (_, _)
     |Access (_, CallMethod (_, _))
     |Assign (_, _) ->
        true
    | _ -> false

  let rec interprete_stat stat input_ctx class_table =
    match stat with
    | StatementBlock stat_list ->
        let rec eval_stat : statement list -> context -> context M.t =
         fun es_list e_ctx ->
          match es_list with
          | [] -> return e_ctx
          | st :: tail -> (
            match st with
            | (Break | Continue | Return _) when tail <> [] ->
                error "Unreachable code"
            | _
              when e_ctx.count_of_nested_cycle >= 1
                   && e_ctx.runtime_flag = WasBreak ->
                return e_ctx
            | _
              when e_ctx.count_of_nested_cycle >= 1
                   && e_ctx.runtime_flag = WasContinue ->
                return e_ctx
            | _ when e_ctx.runtime_flag = WasReturn -> return e_ctx
            | _ when e_ctx.runtime_flag = WasThrown -> return e_ctx
            | _ ->
                interprete_stat st e_ctx class_table
                >>= fun head_ctx -> eval_stat tail head_ctx ) in
        eval_stat stat_list input_ctx
        >>= fun new_ctx ->
        if new_ctx.is_main then return new_ctx
        else delete_var_visibility new_ctx
    | While (smexpr, stat) -> (
        let was_main = input_ctx.is_main in
        let rec loop l_stat ctx =
          if ctx.runtime_flag = WasBreak then
            match l_stat with
            (*decrement visibility level if StatementBlock*)
            | StatementBlock _ ->
                return
                  (dec_visibility_level
                     { ctx with
                       runtime_flag= NoFlag
                     ; count_of_nested_cycle= ctx.count_of_nested_cycle - 1 })
            | _ ->
                return
                  { ctx with
                    runtime_flag= NoFlag
                  ; count_of_nested_cycle= ctx.count_of_nested_cycle - 1 }
          else
            interprete_expr smexpr ctx class_table
            >>= fun new_ctx ->
            match new_ctx.last_expr_result with
            | VBool false -> (
              match l_stat with
              | StatementBlock _ ->
                  return
                    (dec_visibility_level
                       { new_ctx with
                         count_of_nested_cycle= ctx.count_of_nested_cycle - 1
                       ; is_main= was_main })
                  (*additional flag for context *)
              | _ ->
                  return
                    { new_ctx with
                      count_of_nested_cycle= ctx.count_of_nested_cycle - 1 } )
            | VBool true -> (
                interprete_stat l_stat new_ctx class_table
                >>= fun g_ctx ->
                match g_ctx.runtime_flag with
                | WasReturn -> return g_ctx
                | WasContinue -> loop l_stat {g_ctx with runtime_flag= NoFlag}
                | _ -> loop l_stat g_ctx )
            | _ -> error "Incorrect expression type for while stametent" in
        match stat with
        | StatementBlock _ ->
            loop stat
              (inc_visibility_level
                 { input_ctx with
                   count_of_nested_cycle= input_ctx.count_of_nested_cycle + 1
                 ; is_main= false })
        | _ ->
            loop stat
              { input_ctx with
                count_of_nested_cycle= input_ctx.count_of_nested_cycle + 1 } )
    | Break ->
        if input_ctx.count_of_nested_cycle <= 0 then
          error "There is no loop to do break"
        else return {input_ctx with runtime_flag= WasBreak}
    | Continue ->
        if input_ctx.count_of_nested_cycle <= 0 then
          error "There is no loop to do continue"
        else return {input_ctx with runtime_flag= WasContinue}
    | Throw s_expr -> (
        interprete_expr s_expr input_ctx class_table
        >>= fun new_ctx ->
        match new_ctx.last_expr_result with
        | VClass ex_cl -> (
          match ex_cl with
          | ObjNull -> error "NullReferenceException"
          | ObjRef ex_obj -> (
            match ex_obj.parent_key with
            | Some "Exception" -> return {new_ctx with runtime_flag= WasThrown}
            | None -> (
              match ex_obj.class_key with
              | "Exception" -> return {new_ctx with runtime_flag= WasThrown}
              | _ -> error "Cannot implicitly convert type to System.Exception"
              )
            | _ -> error "Cannot implicitly convert type to System.Exception" )
          )
        | _ -> error "Can't throw exceptions not of type VClass" )
    | Try (try_stat, catch_list, finally_stat_o) -> (
        let was_main = input_ctx.is_main in
        let eval_try = function
          | StatementBlock _ ->
              interprete_stat try_stat
                (inc_visibility_level {input_ctx with is_main= false})
                class_table
              >>= fun t_ctx ->
              return (dec_visibility_level {t_ctx with is_main= was_main})
          | _ -> error "Expected { and } in try block!" in
        let eval_finally finally_ctx =
          match finally_stat_o with
          | None -> return finally_ctx
          | Some (StatementBlock _ as finally_stat)
            when finally_ctx.runtime_flag = WasReturn ->
              let get_return_v = finally_ctx.last_expr_result in
              interprete_stat finally_stat
                (inc_visibility_level {finally_ctx with is_main= false})
                class_table
              >>= fun f_ctx ->
              return
                (dec_visibility_level
                   {f_ctx with is_main= was_main; last_expr_result= get_return_v})
          | Some (StatementBlock _ as finally_stat) ->
              let save_flag = finally_ctx.runtime_flag in
              (*очень важно сохранять флаг перед исполнением блока finally, ведь если мы не словили исключения в catch,
                то мы не сможем исполнить ни один statement в finally, потому что флаг будет поднят, поэтому мы его на время исполнения опустим,
                а после исполнения вернем контекст с тем флагом, который был до этого,
                поэтому если до этого флаг был поднят, то после исполнения finally так и останется *)
              interprete_stat finally_stat
                (inc_visibility_level
                   {finally_ctx with is_main= false; runtime_flag= NoFlag})
                class_table
              >>= fun f_ctx ->
              return
                (dec_visibility_level
                   {f_ctx with is_main= was_main; runtime_flag= save_flag})
          | _ -> error "Expected { and } in finally block!" in
        eval_try try_stat
        >>= fun after_try_ctx ->
        match after_try_ctx.runtime_flag = WasThrown with
        | true ->
            let check_catch_stat = function
              | StatementBlock _ -> return ()
              | _ -> error "Expected { and } in catch block!" in
            (*рассматриваем все случаи, что были и в парсере, да, немного копипаста, но они все достаточно индивидуальны,
              поэтому расписаны по отдельности*)
            let eval_catch eval_ctx = function
              | None, None, catch_stat ->
                  check_catch_stat catch_stat
                  >> interprete_stat catch_stat
                       (inc_visibility_level
                          {eval_ctx with runtime_flag= NoFlag; is_main= false})
                       class_table
                  >>= fun catch_ctx ->
                  return
                    (dec_visibility_level {catch_ctx with is_main= was_main})
              | None, Some condit, catch_stat -> (
                  interprete_expr condit eval_ctx class_table
                  >>= fun filter_ctx ->
                  match filter_ctx.last_expr_result with
                  | VBool true ->
                      check_catch_stat catch_stat
                      >> interprete_stat catch_stat
                           (inc_visibility_level
                              { filter_ctx with
                                runtime_flag= NoFlag
                              ; is_main= false })
                           class_table
                      >>= fun catch_ctx ->
                      return
                        (dec_visibility_level
                           {catch_ctx with is_main= was_main})
                  | _ -> return filter_ctx )
              | Some (CsClass cl_name, None), None, catch_stat -> (
                match eval_ctx.last_expr_result with
                | VClass (ObjRef {class_key= throw_name; _}) ->
                    if throw_name = cl_name || cl_name = "Exception" then
                      check_catch_stat catch_stat
                      >> interprete_stat catch_stat
                           (inc_visibility_level
                              { eval_ctx with
                                runtime_flag= NoFlag
                              ; is_main= false })
                           class_table
                      >>= fun catch_ctx ->
                      return
                        (dec_visibility_level
                           {catch_ctx with is_main= was_main})
                    else return eval_ctx
                | _ -> error "Incorrect type of result" )
              | Some (CsClass cl_name, None), Some condit, catch_stat -> (
                match eval_ctx.last_expr_result with
                | VClass (ObjRef {class_key= throw_name; _}) ->
                    if throw_name = cl_name || cl_name = "Exception" then
                      interprete_expr condit eval_ctx class_table
                      >>= fun filter_ctx ->
                      match filter_ctx.last_expr_result with
                      | VBool true ->
                          check_catch_stat catch_stat
                          >> interprete_stat catch_stat
                               (inc_visibility_level
                                  { filter_ctx with
                                    runtime_flag= NoFlag
                                  ; is_main= false })
                               class_table
                          >>= fun catch_ctx ->
                          return
                            (dec_visibility_level
                               {catch_ctx with is_main= was_main})
                      | _ -> return filter_ctx
                    else return eval_ctx
                | _ -> error "Incorrect type of result" )
              | ( Some (CsClass cl_name, Some (IdentVar th_ex_name))
                , None
                , catch_stat ) -> (
                match eval_ctx.last_expr_result with
                | VClass (ObjRef {class_key= throw_name; _}) ->
                    if throw_name = cl_name || cl_name = "Exception" then
                      let adder =
                        Hashtbl.add eval_ctx.variable_table th_ex_name
                          { var_key= th_ex_name
                          ; var_type= CsClass cl_name
                          ; var_value= eval_ctx.last_expr_result
                          ; is_const= false
                          ; assignment_count= 0
                          ; visibility_level= eval_ctx.visibility_level + 1 } ;
                        return eval_ctx in
                      adder
                      >>= fun adder_ctx ->
                      check_catch_stat catch_stat
                      >> interprete_stat catch_stat
                           (inc_visibility_level
                              { adder_ctx with
                                runtime_flag= NoFlag
                              ; is_main= false })
                           class_table
                      >>= fun catch_ctx ->
                      return
                        (dec_visibility_level
                           {catch_ctx with is_main= was_main})
                    else return eval_ctx
                | _ -> error "Incorrect type of result" )
              | ( Some (CsClass cl_name, Some (IdentVar th_ex_name))
                , Some condit
                , catch_stat ) -> (
                match eval_ctx.last_expr_result with
                | VClass (ObjRef {class_key= throw_name; _}) ->
                    if throw_name = cl_name || cl_name = "Exception" then
                      let adder =
                        Hashtbl.add eval_ctx.variable_table th_ex_name
                          { var_key= th_ex_name
                          ; var_type= CsClass cl_name
                          ; var_value= eval_ctx.last_expr_result
                          ; is_const= false
                          ; assignment_count= 0
                          ; visibility_level= eval_ctx.visibility_level + 1 } ;
                        return eval_ctx in
                      adder
                      >>= fun adder_ctx ->
                      interprete_expr condit adder_ctx class_table
                      >>= fun filter_ctx ->
                      match filter_ctx.last_expr_result with
                      | VBool true ->
                          check_catch_stat catch_stat
                          >> interprete_stat catch_stat
                               (inc_visibility_level
                                  { filter_ctx with
                                    runtime_flag= NoFlag
                                  ; is_main= false })
                               class_table
                          >>= fun catch_ctx ->
                          return
                            (dec_visibility_level
                               {catch_ctx with is_main= was_main})
                      | _ -> return filter_ctx
                    else return eval_ctx
                | _ -> error "Incorrect type of result" )
              | _ -> error "Incorrect catch statement" in
            let rec eval_catch_list = function
              (*очень важно запускать рекурсивный проход именно на after_try_ctx, потому что если мы не смогли обработать исключение,
                то мы попортим контекст и потеряем текущий объект исключения, поэтому если мы обработали один catch из списка и
                он не подошел нам, то забываем испорченный контекст, запускаясь заново на after_try_ctx*)

              (*все случаи были протестированы в тестах*)
              | [] -> return after_try_ctx
              | x :: xs -> (
                  eval_catch after_try_ctx x
                  >>= fun new_ctx ->
                  match new_ctx.runtime_flag = WasThrown with
                  | false -> return new_ctx
                  | true -> eval_catch_list xs ) in
            eval_catch_list catch_list
            >>= fun handle_ctx ->
            eval_finally handle_ctx >>= fun end_ctx -> return end_ctx
        | false -> eval_finally after_try_ctx >>= fun end_ctx -> return end_ctx
        )
    | Print print_expr ->
        interprete_expr print_expr input_ctx class_table
        >>= fun new_ctx ->
        let printer = function
          | VInt value -> return (printf "%d\n" value)
          | VBool value -> return (printf "%b\n" value)
          | VString value -> return (printf "%s\n" value)
          | VClass value -> (
            match value with
            | ObjNull -> error "NullReferenceException"
            | ObjRef ob -> return (printf "%s\n" ob.class_key) )
          | VVoid -> error "Impossible to print void"
          | VNull -> error "Impossible to print null" in
        printer new_ctx.last_expr_result
        >> (* with ex ->
              let print_ex = Printexc.to_string ex in
              Printf.eprintf "There was an error: %s" print_ex ) ; *)
        return new_ctx
    | If (condit, body, else_body) -> (
        interprete_expr condit input_ctx class_table
        >>= fun new_ctx ->
        let was_main = new_ctx.is_main in
        match new_ctx.last_expr_result with
        | VBool true -> (
          match body with
          | StatementBlock _ ->
              interprete_stat body
                (inc_visibility_level {new_ctx with is_main= false})
                class_table
              >>= fun in_ctx ->
              return (dec_visibility_level {in_ctx with is_main= was_main})
          | _ -> interprete_stat body new_ctx class_table )
        | VBool false -> (
          match else_body with
          | Some (StatementBlock _ as else_st) ->
              interprete_stat else_st
                (inc_visibility_level {new_ctx with is_main= false})
                class_table
              >>= fun el_ctx ->
              return (dec_visibility_level {el_ctx with is_main= was_main})
          | Some else_st -> interprete_stat else_st new_ctx class_table
          | None -> return input_ctx )
        | _ -> error "Incorrect type for condition statement" )
    | For (dec_stat_o, expr_o, after_list, body) ->
        let was_main = input_ctx.is_main in
        ( match dec_stat_o with
        | None -> return (inc_visibility_level {input_ctx with is_main= false})
        | Some dec_stat ->
            interprete_stat dec_stat
              (inc_visibility_level {input_ctx with is_main= false})
              class_table )
        >>= fun new_ctx ->
        let rec loop body_stat af_list ctx =
          if ctx.runtime_flag = WasBreak then
            delete_var_visibility
              { ctx with
                runtime_flag= NoFlag
              ; count_of_nested_cycle= ctx.count_of_nested_cycle - 1
              ; visibility_level= ctx.visibility_level - 1
              ; is_main= was_main }
          else
            ( match expr_o with
            | None -> return {ctx with last_expr_result= VBool true}
            | Some expr_t -> interprete_expr expr_t ctx class_table )
            >>= fun cond_ctx ->
            match cond_ctx.last_expr_result with
            (*проверка не прошла, значит с циклом все*)
            | VBool false ->
                delete_var_visibility
                  { cond_ctx with
                    count_of_nested_cycle= cond_ctx.count_of_nested_cycle - 1
                  ; visibility_level= cond_ctx.visibility_level - 1
                  ; is_main= was_main }
            | VBool true -> (
                let rec inter_expr_list e_list as_ctx =
                  match e_list with
                  | [] -> return as_ctx
                  | x :: xs ->
                      if expr_in_stat x then
                        interprete_expr x as_ctx class_table
                        >>= fun z_ctx -> inter_expr_list xs z_ctx
                      else error "Incorrect expression for after body list"
                in
                interprete_stat body_stat
                  {cond_ctx with visibility_level= new_ctx.visibility_level + 1}
                  class_table
                >>= fun body_ctx ->
                match body_ctx.runtime_flag with
                | WasReturn -> return {body_ctx with is_main= was_main}
                | WasContinue ->
                    inter_expr_list af_list body_ctx
                    >>= fun after_ctx ->
                    loop body_stat af_list {after_ctx with runtime_flag= NoFlag}
                | _ ->
                    inter_expr_list af_list body_ctx
                    >>= fun after_ctx -> loop body_stat af_list after_ctx )
            | _ -> error "Incorrect condition type in for statement" in
        loop body after_list
          { new_ctx with
            count_of_nested_cycle= input_ctx.count_of_nested_cycle + 1 }
    | Return None when input_ctx.current_meth_type = Void ->
        return {input_ctx with last_expr_result= VVoid; runtime_flag= WasReturn}
    | Return None -> error "Return value error"
    | Return (Some result) ->
        expression_check result input_ctx class_table
        >>= fun ret_type ->
        if ret_type <> input_ctx.current_meth_type then
          error "Return value error"
        else
          interprete_expr result input_ctx class_table
          >>= fun new_ctx -> return {new_ctx with runtime_flag= WasReturn}
    | Expression s_expr ->
        if expr_in_stat s_expr then
          interprete_expr s_expr input_ctx class_table
          >>= fun new_ctx -> return new_ctx
        else error "Incorrect expression for statement"
    | VarDeclare (modifier, vars_type, var_list) ->
        let check_const : modifier option -> bool = function
          | Some Const -> true
          | _ -> false in
        let rec var_declarator v_list var_ctx =
          match v_list with
          | [] -> return var_ctx
          | (var_name, var_expr_o) :: tail -> (
            match var_ctx.current_o with
            | ObjNull -> error "Impossible to execute in null object"
            | ObjRef {class_table= table; _} ->
                ( if
                  Hashtbl.mem var_ctx.variable_table var_name
                  || Hashtbl.mem table var_name
                then error "Variable with this name is already defined"
                else
                  match var_expr_o with
                  | None ->
                      Hashtbl.add var_ctx.variable_table var_name
                        { var_key= var_name
                        ; var_type= vars_type
                        ; var_value= get_default_value vars_type
                        ; is_const= check_const modifier
                        ; assignment_count= 0
                        ; visibility_level= var_ctx.visibility_level } ;
                      return var_ctx
                  | Some var_expr_e -> (
                      expression_check var_expr_e var_ctx class_table
                      >>= fun var_expr_type ->
                      let add_helper new_var =
                        interprete_expr new_var var_ctx class_table
                        >>= fun ctx_aft_ad ->
                        Hashtbl.add ctx_aft_ad.variable_table var_name
                          { var_key= var_name
                          ; var_type= var_expr_type
                          ; var_value= ctx_aft_ad.last_expr_result
                          ; is_const= check_const modifier
                          ; assignment_count= 1
                          ; visibility_level= ctx_aft_ad.visibility_level } ;
                        return ctx_aft_ad in
                      match var_expr_type with
                      | CsClass "null" -> (
                        match vars_type with
                        | CsClass _ -> add_helper var_expr_e
                        | _ ->
                            error
                              "Incorrect assign type in variable declaration" )
                      | CsClass class_right -> (
                        match vars_type with
                        | CsClass class_left ->
                            assign_verify_polymorphic class_left class_right
                              class_table
                            >>= fun _ -> add_helper var_expr_e
                        | _ ->
                            error
                              "Incorrect assign type in variable declaration" )
                      | _ when var_expr_type = vars_type ->
                          add_helper var_expr_e
                      | _ ->
                          error
                            ( "Incorrect value type for declared variable:"
                            ^ show_data_type var_expr_type ) ) )
                >>= fun head_ctx -> var_declarator tail head_ctx ) in
        var_declarator var_list input_ctx

  and interprete_expr in_expr in_ctx class_table =
    let eval_expr e_expr ctx =
      let eval_bin left_e right_e operator =
        interprete_expr left_e ctx class_table
        >>= fun left_ctx ->
        interprete_expr right_e left_ctx class_table
        >>= fun right_ctx ->
        let get_left_value = left_ctx.last_expr_result in
        let get_right_value = right_ctx.last_expr_result in
        try
          let cal_value = operator get_left_value get_right_value in
          return {right_ctx with last_expr_result= cal_value}
        with
        | Invalid_argument m -> error m
        | Division_by_zero -> error "Division by zero!" in
      let eval_unar ex_operand operator =
        interprete_expr ex_operand ctx class_table
        >>= fun new_ctx ->
        let get_value = new_ctx.last_expr_result in
        try
          let cal_unar_v = operator get_value in
          return {new_ctx with last_expr_result= cal_unar_v}
        with Invalid_argument m -> error m in
      match e_expr with
      | Add (left, right) -> eval_bin left right ( ++ )
      | Sub (left, right) -> eval_bin left right ( -- )
      | Mul (left, right) -> eval_bin left right ( ** )
      | Div (left, right) -> eval_bin left right ( // )
      | Mod (left, right) -> eval_bin left right ( %% )
      | And (left, right) -> eval_bin left right ( &&& )
      | Or (left, right) -> eval_bin left right ( ||| )
      | Not not_exp -> eval_unar not_exp not_op
      | Less (left, right) -> eval_bin left right ( <<< )
      | More (left, right) -> eval_bin left right ( >>> )
      | LessOrEqual (left, right) -> eval_bin left right ( <<== )
      | MoreOrEqual (left, right) -> eval_bin left right ( >>== )
      | Equal (left, right) -> eval_bin left right ( === )
      | NotEqual (left, right) -> eval_bin left right ( !=! )
      | ConstExpr value -> return {ctx with last_expr_result= value}
      | IdentVar var_id -> (
        match get_value_option ctx.variable_table var_id with
        | Some id -> return {ctx with last_expr_result= id.var_value}
        | None -> (
          try
            get_obj_info ctx.current_o
            |> fun (_, table, _) ->
            match get_value_option table var_id with
            | Some field -> return {ctx with last_expr_result= field.f_value}
            | None -> error "Field not found"
          with Failure m | Invalid_argument m -> error m ) )
      | Null -> return {ctx with last_expr_result= VClass ObjNull}
      | Access (obj_expr, IdentVar field_key) -> (
          interprete_expr obj_expr ctx class_table
          >>= fun ob_ctx ->
          let get_obj = ob_ctx.last_expr_result in
          match get_obj with
          | VClass (ObjRef {class_table= table; _}) ->
              get_elem table field_key
              >>= fun get_field ->
              return {ob_ctx with last_expr_result= get_field.f_value}
          | _ -> error "Cannot access a field of non-reference type" )
      | Access (obj_expr, CallMethod (m_name, args)) -> (
          interprete_expr obj_expr ctx class_table
          >>= fun ob_ctx ->
          match ob_ctx.last_expr_result with
          | VClass ObjNull -> error "NullReferenceException"
          | VClass
              (ObjRef
                { class_key= cl_k
                ; class_table= table
                ; number= num
                ; parent_key= par_k }) -> (
            match get_value_option class_table cl_k with
            | None -> error "Class not found"
            | Some found_class -> (
                method_verify found_class m_name args ob_ctx class_table
                >>= fun meth ->
                let create_var_table : (table_key, variable) Hashtbl_der.t =
                  Hashtbl.create 128 in
                ( try
                    refresh_table create_var_table args meth.args ob_ctx
                      class_table
                  with Invalid_argument m -> error m )
                >>= fun (new_table, new_ctx) ->
                interprete_stat meth.body
                  { current_o=
                      ObjRef
                        { class_key= cl_k
                        ; class_table= table
                        ; number= num
                        ; parent_key= par_k }
                  ; variable_table= new_table
                  ; current_meth_type= meth.method_type
                  ; last_expr_result= VVoid
                  ; runtime_flag= NoFlag
                  ; is_main= false
                  ; curr_constructor= None
                  ; count_of_nested_cycle= 0
                  ; visibility_level= 0
                  ; prev_ctx= Some ctx
                  ; count_of_obj= ctx.count_of_obj
                  ; is_creation= false }
                  class_table
                >>= fun res_ctx ->
                match res_ctx.runtime_flag with
                | WasThrown ->
                    return
                      { new_ctx with
                        last_expr_result= res_ctx.last_expr_result
                      ; count_of_obj= res_ctx.count_of_obj
                      ; runtime_flag= WasThrown
                      ; is_creation= false }
                | _ ->
                    return
                      { new_ctx with
                        last_expr_result=
                          ( if meth.method_type = Void then VVoid
                          else res_ctx.last_expr_result )
                      ; count_of_obj= res_ctx.count_of_obj
                      ; is_creation= false } ) )
          | _ -> error "Cannot access a field of non-reference type" )
      | CallMethod (m_name, args) -> (
          let get_curr_o =
            return {ctx with last_expr_result= VClass ctx.current_o} in
          get_curr_o
          >>= fun ob_ctx ->
          match ob_ctx.last_expr_result with
          | VClass ObjNull -> error "NullReferenceException"
          | VClass
              (ObjRef
                { class_key= cl_k
                ; class_table= table
                ; number= num
                ; parent_key= par_k }) -> (
            match get_value_option class_table cl_k with
            | None -> error "Class not found"
            | Some found_class -> (
                method_verify found_class m_name args ob_ctx class_table
                >>= fun meth ->
                let create_var_table : (table_key, variable) Hashtbl_der.t =
                  Hashtbl.create 128 in
                ( try
                    refresh_table create_var_table args meth.args ob_ctx
                      class_table
                  with Invalid_argument m -> error m )
                >>= fun (new_table, new_ctx) ->
                interprete_stat meth.body
                  { current_o=
                      ObjRef
                        { class_key= cl_k
                        ; class_table= table
                        ; number= num
                        ; parent_key= par_k }
                  ; variable_table= new_table
                  ; current_meth_type= meth.method_type
                  ; last_expr_result= VVoid
                  ; runtime_flag= NoFlag
                  ; is_main= false
                  ; curr_constructor= None
                  ; count_of_nested_cycle= 0
                  ; visibility_level= 0
                  ; prev_ctx= Some ctx
                  ; count_of_obj= ctx.count_of_obj
                  ; is_creation= false }
                  class_table
                >>= fun res_ctx ->
                match res_ctx.runtime_flag with
                | WasThrown ->
                    return
                      { new_ctx with
                        last_expr_result= res_ctx.last_expr_result
                      ; count_of_obj= res_ctx.count_of_obj
                      ; runtime_flag= WasThrown
                      ; is_creation= false }
                | _ ->
                    return
                      { new_ctx with
                        last_expr_result=
                          ( if meth.method_type = Void then VVoid
                            (*чтобы не сохранять в last_expr_result значение не типа VVoid, если метод Void  *)
                          else res_ctx.last_expr_result )
                      ; count_of_obj= res_ctx.count_of_obj
                      ; is_creation= false } ) )
          | _ -> error "Cannot access a field of non-reference type" )
      | Assign (IdentVar var_key, val_expr) ->
          interprete_expr val_expr ctx class_table
          >>= fun eval_ctx ->
          update_identifier var_key eval_ctx.last_expr_result eval_ctx
      | Assign (Access (obj, IdentVar field_name), val_expr) ->
          interprete_expr val_expr ctx class_table
          >>= fun eval_ctx -> update_field obj field_name eval_ctx class_table
      | PostInc (Access (obj, IdentVar field))
       |PrefInc (Access (obj, IdentVar field)) ->
          interprete_expr
            (Assign
               ( Access (obj, IdentVar field)
               , Add (Access (obj, IdentVar field), ConstExpr (VInt 1)) ))
            ctx class_table
      | PostInc (IdentVar var_key) | PrefInc (IdentVar var_key) ->
          interprete_expr
            (Assign
               (IdentVar var_key, Add (IdentVar var_key, ConstExpr (VInt 1))))
            ctx class_table
      | PostDec (Access (obj_expr, IdentVar field))
       |PrefDec (Access (obj_expr, IdentVar field)) ->
          interprete_expr
            (Assign
               ( Access (obj_expr, IdentVar field)
               , Sub (Access (obj_expr, IdentVar field), ConstExpr (VInt 1)) ))
            ctx class_table
      | PostDec (IdentVar var_key) | PrefDec (IdentVar var_key) ->
          interprete_expr
            (Assign
               (IdentVar var_key, Sub (IdentVar var_key, ConstExpr (VInt 1))))
            ctx class_table
      | ClassCreate (class_name, c_args) ->
          get_elem class_table class_name
          >>= fun get_obj ->
          constructor_verify get_obj c_args ctx class_table
          >>= fun found_constr ->
          let rec create_obj par_class init_ctx =
            let find_p_field = Extractors.get_field_pairs par_class.dec_tree in
            let rec helper inhelp_ht rec_ctx = function
              | [] -> return rec_ctx
              | (cur_field_type, field_name, field_expr_o) :: tail ->
                  let is_const f_key =
                    get_elem get_obj.field_table f_key
                    >>= fun find_t_field -> return find_t_field.is_const in
                  ( match field_expr_o with
                  | Some f_expr -> (
                      expression_check f_expr rec_ctx class_table
                      >>= fun expr_type ->
                      is_const field_name
                      >>= fun const_res ->
                      let add_field fe =
                        interprete_expr fe rec_ctx class_table
                        >>= fun fe_ctx ->
                        Hashtbl.add inhelp_ht field_name
                          { key= field_name
                          ; f_type= cur_field_type
                          ; f_value= fe_ctx.last_expr_result
                          ; is_const= const_res
                          ; assignment_count= 1 } ;
                        return (fe_ctx, inhelp_ht) in
                      match expr_type with
                      | CsClass "null" -> (
                        match cur_field_type with
                        | CsClass _ -> add_field f_expr
                        | _ -> error "Incorrect type of variable assignment" )
                      | CsClass cright -> (
                        match cur_field_type with
                        | CsClass cleft ->
                            assign_verify_polymorphic cleft cright class_table
                            >>= fun _ -> add_field f_expr
                        | _ -> error "Incorrect type of variable assignment" )
                      | _ when expr_type = cur_field_type -> add_field f_expr
                      | _ -> error "Incorrect type of variable assignment" )
                  | None ->
                      is_const field_name
                      >>= fun const_res ->
                      Hashtbl.add inhelp_ht field_name
                        { key= field_name
                        ; f_type= cur_field_type
                        ; f_value= get_default_value cur_field_type
                        ; is_const= const_res
                        ; assignment_count= 0 } ;
                      return (rec_ctx, inhelp_ht) )
                  >>= fun (head_ctx, head_ht) ->
                  obj_num head_ctx.current_o
                  >>= fun num ->
                  helper head_ht
                    { head_ctx with
                      current_o=
                        ObjRef
                          { class_key= class_name
                          ; parent_key= get_obj.parent_key
                          ; class_table= head_ht
                          ; number= num } }
                    tail in
            match par_class.parent_key with
            | None -> helper (Hashtbl.create 128) init_ctx find_p_field
            | Some par_key ->
                get_elem class_table par_key
                >>= fun parent_r ->
                create_obj parent_r init_ctx
                >>= fun par_ctx ->
                helper (get_obj_fields par_ctx.current_o) par_ctx find_p_field
          in
          let new_object =
            ObjRef
              { class_key= class_name
              ; parent_key= get_obj.parent_key
              ; class_table= Hashtbl.create 100
              ; number= ctx.count_of_obj + 1 } in
          create_obj get_obj
            { current_o= new_object
            ; variable_table= Hashtbl.create 100
            ; last_expr_result= VVoid
            ; runtime_flag= NoFlag
            ; current_meth_type= Void
            ; is_main= false
            ; count_of_nested_cycle= 0
            ; visibility_level= 0
            ; prev_ctx= Some ctx
            ; count_of_obj= ctx.count_of_obj + 1
            ; curr_constructor= None
            ; is_creation= false }
          >>= fun initres_ctx ->
          let get_new_var_table =
            try
              refresh_table (Hashtbl.create 128) c_args found_constr.args ctx
                class_table
            with Invalid_argument m -> error m in
          get_new_var_table
          >>= fun (vt, _) ->
          prepare_constructor found_constr.body get_obj
          >>= fun c_body ->
          interprete_stat c_body
            { initres_ctx with
              variable_table= vt
            ; is_creation= true
            ; is_main= false
            ; curr_constructor= Some found_constr.key }
            class_table
          >>= fun c_ctx ->
          return
            { ctx with
              last_expr_result= VClass c_ctx.current_o
            ; runtime_flag= NoFlag
            ; count_of_obj= c_ctx.count_of_obj }
      | _ -> error "Incorrect expression!" in
    expression_check in_expr in_ctx class_table
    >>= fun _ -> eval_expr in_expr in_ctx

  and refresh_table hashtable args meth_args ctx class_table =
    fold_left2
      (fun (new_ht, hctx) arg pair ->
        match pair with
        | h_type, h_name ->
            interprete_expr arg hctx class_table
            >>= fun new_ctx ->
            Hashtbl.add new_ht h_name
              { var_type= h_type
              ; var_key= h_name
              ; is_const= false
              ; assignment_count= 1
              ; var_value= new_ctx.last_expr_result
              ; visibility_level= 0 } ;
            return (new_ht, new_ctx))
      (hashtable, ctx) args meth_args

  and update_identifier var_key value var_ctx =
    if Hashtbl.mem var_ctx.variable_table var_key then (
      get_elem var_ctx.variable_table var_key
      >>= fun get_old ->
      check_assign_variable get_old
      >>= fun _ ->
      Hashtbl.replace var_ctx.variable_table var_key
        { get_old with
          var_value= value
        ; assignment_count= get_old.assignment_count + 1 } ;
      return var_ctx )
    else
      match var_ctx.current_o with
      | ObjNull -> error "NullReferenceException"
      | ObjRef {class_table= table; _} ->
          if Hashtbl.mem table var_key then
            get_elem table var_key
            >>= fun get_old ->
            check_assign_field get_old
            >>= fun _ ->
            if var_ctx.is_creation then
              Hashtbl.replace table var_key
                {get_old with f_value= var_ctx.last_expr_result}
              |> fun _ -> return var_ctx
            else
              try
                update_object var_ctx.current_o var_key var_ctx.last_expr_result
                  var_ctx
                |> fun _ -> return var_ctx
              with
              | Invalid_argument m -> error m
              | Not_found -> error "Variable not found"
          else error "Variable not found"

  and update_field obj_expr field_name field_ctx class_table =
    interprete_expr obj_expr field_ctx class_table
    >>= fun obj_evaled_ctx ->
    let get_obj = get_obj_value obj_evaled_ctx.last_expr_result in
    let cal_new_val = field_ctx.last_expr_result in
    try
      get_obj_info get_obj
      |> fun (_, fl_table, _) ->
      if Hashtbl.mem fl_table field_name then
        get_elem fl_table field_name
        >>= fun old_field ->
        check_assign_field old_field
        >>= fun _ ->
        if obj_evaled_ctx.is_creation then
          Hashtbl.replace fl_table field_name
            {old_field with f_value= field_ctx.last_expr_result}
          |> fun _ -> return obj_evaled_ctx
        else
          update_object get_obj field_name cal_new_val obj_evaled_ctx
          |> fun _ -> return obj_evaled_ctx
      else error "Field not found in class!"
    with
    | Invalid_argument m | Failure m -> error m
    | Not_found -> error "Field not found!"

  and update_object obj field_key value up_ctx =
    let rec refresh field_hashtable f_key new_val ref_num count_assign =
      Hashtbl.iter
        (fun _ field_ref ->
          match field_ref with
          | {f_value= f_val; _} -> (
            match f_val with
            | VClass (ObjRef {class_table= fl_table; number= fnum; _}) ->
                ( if fnum = ref_num then
                  match get_value_option fl_table f_key with
                  | None -> raise Not_found
                  | Some old_field ->
                      Hashtbl.replace fl_table f_key
                        { old_field with
                          f_value= new_val
                        ; assignment_count= count_assign } ) ;
                refresh fl_table f_key new_val ref_num count_assign
            | _ -> () ))
        field_hashtable in
    let rec helper_update f_key new_val upd_ctx up_num assign_cnt =
      Hashtbl.iter
        (fun _ var ->
          match var.var_value with
          | VClass (ObjRef {class_table= field_table; number= fnum; _}) ->
              if up_num = fnum then (
                match get_value_option field_table f_key with
                | None -> raise Not_found
                | Some old_field ->
                    Hashtbl.replace field_table f_key
                      { old_field with
                        f_value= new_val
                      ; assignment_count= assign_cnt } ;
                    refresh field_table f_key new_val up_num assign_cnt )
              else refresh field_table f_key new_val up_num assign_cnt
          | _ -> ())
        upd_ctx.variable_table
      |> fun () ->
      match upd_ctx.prev_ctx with
      | None -> ()
      | Some prev_ctx -> helper_update f_key new_val prev_ctx up_num assign_cnt
    in
    get_obj_info obj
    |> fun (_, object_frt, object_number) ->
    ( match get_value_option object_frt field_key with
    | None -> raise Not_found
    | Some f_assign -> f_assign.assignment_count + 1 )
    |> fun assign_cnt ->
    helper_update field_key value up_ctx object_number assign_cnt

  and prepare_constructor curr_body curr_class =
    match (curr_body, curr_class.parent_key) with
    | StatementBlock _, _ -> return curr_body
    | _ -> error "Must be statement block!"

  let interprete_program : (table_key, table_class) Hashtbl.t -> context M.t =
   fun hashtable ->
    find_main_class hashtable
    >>= fun main_class ->
    context_init
      (ObjRef
         { class_key= main_class.class_key
         ; parent_key= None
         ; class_table= Hashtbl.create 32
         ; number= 0 })
      (Hashtbl.create 32)
    >>= fun ctx ->
    let main = Hashtbl.find main_class.method_table "Main" in
    interprete_stat main.body ctx hashtable
    >>= fun final_ctx ->
    match final_ctx.runtime_flag = WasThrown with
    | false -> return final_ctx
    | true -> error "Unhandled exception"
end
