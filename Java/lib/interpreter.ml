open Ast
open Parser
open Hashtbl_p

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

type key_t = string [@@deriving show]

type constructor_r = { key : key_t; args : (type_t * name) list; body : stmt }
[@@deriving show { with_path = false }]

let startswith test_str sub_str =
  if String.length sub_str > String.length test_str then false
  else
    let sub = String.sub test_str 0 (String.length sub_str) in
    String.equal sub sub_str

let convert_table_to_seq = Hashtbl.to_seq_values

let seq_hd_exn s =
  match s () with Seq.Nil -> raise Not_found | Seq.Cons (x, _) -> x

type field_r = {
  f_type : type_t;
  key : key_t;
  is_not_mutable : bool;
  sub_tree : expr option;
}
[@@deriving show { with_path = false }]

type method_r = {
  m_type : type_t;
  is_abstract : bool;
  is_overridable : bool;
  has_override_annotation : bool;
  args : (type_t * name) list;
  key : key_t;
  body : stmt option;
  is_overriden : bool;
}
[@@deriving show { with_path = false }]

type class_r = {
  this_key : key_t;
  field_table : (key_t, field_r) Hashtbl_p.t;
  method_table : (key_t, method_r) Hashtbl_p.t;
  constructor_table : (key_t, constructor_r) Hashtbl_p.t;
  children_keys : key_t list;
  is_abstract : bool;
  is_inheritable : bool;
  parent_key : key_t option;
  dec_tree : class_dec;
}
[@@deriving show { with_path = false }]

let convert_name_to_key = function Some (Name x) -> Some x | None -> None

let get_type_list = List.map fst

let make_method_key m_name args =
  String.concat "" (m_name :: List.map show_type_t (get_type_list args)) ^ "@@"

let make_constr_key c_name args =
  String.concat "" (c_name :: List.map show_type_t (get_type_list args)) ^ "$$"

let to_string_key = make_method_key "toString" []

let equals_key = make_method_key "equals" [ (ClassName "Object", Name "obj") ]

module ClassLoader (M : MONADERROR) = struct
  open M

  let is_abstract = List.mem Abstract

  let is_final = List.mem Final

  let is_override = List.mem Override

  let is_static = List.mem Static

  let is_public = List.mem Public

  let get_elem_if_present_m ht key =
    match Hashtbl_p.get_elem_if_present ht key with
    | None -> error "No such element in table!"
    | Some el -> return el

  let monadic_update_hash_table ht old_key new_val =
    Hashtbl.replace ht old_key new_val;
    return ht

  let rec monadic_list_iter list action base =
    match list with
    | [] -> return base
    | x :: xs -> action x >> monadic_list_iter xs action base

  let rec monadic_seq_iter seq action base =
    match seq () with
    | Seq.Nil -> return base
    | Seq.Cons (x, next) -> action x >> monadic_seq_iter next action base

  let prepare_object ht =
    let constructor_table = Hashtbl.create 20 in
    let field_table = Hashtbl.create 20 in
    let method_table = Hashtbl.create 20 in

    let equals : method_r =
      {
        m_type = Bool;
        is_abstract = false;
        is_overridable = true;
        has_override_annotation = false;
        args = [ (ClassName "Object", Name "obj") ];
        key = equals_key;
        is_overriden = false;
        body =
          apply Stmt.stat_block
            {| 
          {
              return this == obj;
          }
    |};
      }
    in

    let to_string : method_r =
      {
        m_type = ClassName "String";
        is_abstract = false;
        is_overridable = true;
        has_override_annotation = false;
        is_overriden = false;
        args = [];
        key = to_string_key;
        body =
          apply Stmt.stat_block
            {|
          {
            return "Object";
          }
        |};
      }
    in
    let get_dec_tree =
      match
        apply class_declaration
          {|public class Object {
                        public boolean equals(Object obj) {
                            return this == obj;
                        }
                        
                        public String toString() {
                          return "Object";
                        }
                    }
      |}
      with
      | Some dt -> return dt
      | None -> error "Error in parsing Object class!"
    in

    get_dec_tree >>= fun dec_tree ->
    Hashtbl.add method_table equals_key equals;
    Hashtbl.add method_table to_string_key to_string;
    Hashtbl.add ht "Object"
      {
        this_key = "Object";
        field_table;
        method_table;
        constructor_table;
        children_keys = [];
        is_abstract = false;
        is_inheritable = true;
        parent_key = None;
        dec_tree;
      };
    return ht

  (*Функция для проверки полей, методов и конструкторов на наличие бредовых модификаторов*)
  let check_modifiers_f pair =
    match pair with
    | l, f -> (
        match f with
        (*public static void main ()*)
        | Method (Void, Name "main", [], _)
          when is_static l && is_public l
               && (not (is_abstract l))
               && (not (is_final l))
               && not (is_override l) ->
            return ()
        | Method (_, Name "main", _, _) ->
            error "Only one main method can be in program!"
        (*Простые методы - не статичные, не могут быть абстрактными и финальными одновременно*)
        | Method (_, _, _, _) when is_abstract l && is_final l ->
            error "Wrong method modifiers"
        | Method (_, _, _, _) when is_static l -> error "Wrong method modifiers"
        | Method (_, _, _, _) -> return ()
        (*Поля - не статичные, не абстрактные, не override*)
        | VarField (_, _)
          when (not (is_static l))
               && (not (is_abstract l))
               && not (is_override l) ->
            return ()
        | VarField (_, _) -> error "Wrong field modifiers"
        (*Конструкторы - могут быть либо публичными, либо дефолтными*)
        | Constructor (_, _, _)
          when (not (is_static l))
               && (not (is_abstract l))
               && (not (is_final l))
               && not (is_override l) ->
            return ()
        | Constructor (_, _, _) -> error "Wrong constructor modifiers" )

  (*Функция для проверки класса на наличие бредовых модификаторов*)
  let check_modifiers_c = function
    | Class (ml, _, _, _) when is_abstract ml && is_final ml ->
        error "Wrong class modifiers"
    | Class (ml, _, _, _) when (not (is_static ml)) && not (is_override ml) ->
        return ()
    | Class (_, _, _, _) -> error "Wrong class modifiers"

  (* Отдельная функция для добавления в таблицу с проверкой на существование *)
  let add_with_check ht key value e_message =
    match get_elem_if_present ht key with
    | None ->
        Hashtbl.add ht key value;
        return ht
    | _ -> error e_message

  (* Сначала надо просто заполнить таблицу классов *)
  let add_to_class_table class_ht class_d =
    match class_d with
    | Class (ml, Name this_key, parent_o, fields) ->
        (* Инициализируем таблицы *)
        let method_table = Hashtbl.create 1024 in
        let field_table = Hashtbl.create 1024 in
        let constructor_table = Hashtbl.create 1024 in
        check_modifiers_c class_d
        >>
        (* Функция добавления элемента класса в соответствующую таблицу *)
        let add_field : modifier list * field -> unit M.t =
         fun field_elem ->
          match field_elem with
          | f_ms, VarField (f_type, pairs) ->
              let rec helper = function
                | [] -> return ()
                | (Name key, sub_tree) :: ps ->
                    let is_not_mutable = is_final f_ms in
                    (* В качестве ключа выступает имя поля *)
                    add_with_check field_table key
                      { f_type; key; is_not_mutable; sub_tree }
                      "Similar fields"
                    >> helper ps
              in
              check_modifiers_f field_elem >> helper pairs
          | m_ms, Method (m_type, Name name, args, body) ->
              (* Формирование ключа: method_key = name ++ type1 ++ type2 ++ ... ++ typen *)
              let key = make_method_key name args in
              let is_class_abstract = is_abstract ml in
              (* Является ли метод абстрактным *)
              let is_abstract = is_abstract m_ms in
              (*Перед добавлением стоит проверять, чтобы у абстрактного метода не было тела и прочие ошибки*)
              let check_abstract_body_syntax =
                match is_abstract with
                | true -> (
                    if not is_class_abstract then
                      error "Abstract method in non-abstract class"
                    else
                      match body with
                      | Some _ -> error "Abstract method cannot have body"
                      | None -> return () )
                | false -> (
                    match body with
                    | Some _ -> return ()
                    | None -> error "No body of non-abstract method" )
              in
              let is_overridable = not (is_final m_ms) in
              let has_override_annotation = is_override m_ms in
              check_modifiers_f field_elem
              >> check_abstract_body_syntax
              >> add_with_check method_table key
                   {
                     m_type;
                     is_abstract;
                     is_overridable;
                     is_overriden = false;
                     has_override_annotation;
                     args;
                     key;
                     body;
                   }
                   "Method with this type exists"
              >> return ()
          | _, Constructor (Name name, args, body) ->
              let constr_key = make_constr_key name args in
              (*Смотрим, чтобы имя конструктора совпадало с классом*)
              let check_names_match =
                if name = this_key then return ()
                else error "Constructor name error"
              in
              check_names_match
              >> check_modifiers_f field_elem
              >> add_with_check constructor_table constr_key
                   { key = constr_key; args; body }
                   "Constructor with this type exists"
              >> return ()
        in
        let add_parent p =
          match p with None -> Some "Object" | _ -> convert_name_to_key p
        in
        let is_abstract = is_abstract ml in
        let is_inheritable = not (is_final ml) in
        let parent_key = add_parent parent_o in
        monadic_list_iter fields add_field ()
        >> add_with_check class_ht this_key
             {
               this_key;
               field_table;
               method_table;
               constructor_table;
               children_keys = [];
               is_abstract;
               is_inheritable;
               parent_key;
               dec_tree = class_d;
             }
             "Similar Classes"

  let c_table_add cd_list cl_ht =
    monadic_list_iter cd_list (add_to_class_table cl_ht) cl_ht

  (* Если у класса нет конструкторов - надо добавить дефолтный *)
  let add_default_constructors_if_needed ht =
    Hashtbl.iter
      (fun k cr ->
        if Hashtbl.length cr.constructor_table = 0 then
          let c_key = make_constr_key k [] in
          Hashtbl.add cr.constructor_table c_key
            { key = c_key; args = []; body = StmtBlock [] })
      ht;
    return ht

  let update_child_keys ht =
    let update : class_r -> class_r M.t =
     fun cr ->
      match cr.parent_key with
      (* Ключа родителя нет - идем дальше *)
      | None -> return cr
      (* Есть - пытаемся получить родителя по ключу (или грохаемся с ошибкой), если можно наследоваться - обновляем хеш-таблицу *)
      | Some p_key -> (
          let parent_o = get_elem_if_present ht p_key in
          match parent_o with
          | None -> error "No parent class found"
          | Some parent when parent.is_inheritable ->
              let new_val =
                {
                  parent with
                  children_keys = cr.this_key :: parent.children_keys;
                }
              in
              monadic_update_hash_table ht p_key new_val >> return new_val
          | Some _ -> error "Final class cannot be inherited" )
    in
    monadic_seq_iter (convert_table_to_seq ht) update ht

  (* Мелкие функции по обработке отдельных частей для transfert *)

  (* Обработка поля родителя *)
  let process_field : class_r -> field_r -> unit t =
   fun ch cur_field ->
    (* Смотрим, есть ли такое поле в таблице ребенка*)
    match get_elem_if_present ch.field_table cur_field.key with
    (* Нет - просто добавляем в таблицу ребенка *)
    | None -> return (Hashtbl.add ch.field_table cur_field.key cur_field)
    (* Есть - ну и ладно, пропускаем *)
    | _ -> return ()

  let process_fields par ch =
    monadic_seq_iter
      (convert_table_to_seq par.field_table)
      (process_field ch) ()

  let is_this : stmt -> bool = function
    | Expression (CallMethod (This, _)) -> true
    | _ -> false

  let body_starts_with_this : constructor_r -> bool = function
    | { body = StmtBlock (Expression (CallMethod (This, _)) :: _); _ } -> true
    | _ -> false

  (* Надо у текущего класса проверять, чтобы если в его конструкторах есть вызов this(...), то он должен быть первым и единственным *)
  let check_cur_constructors cur =
    let check_constructor : constructor_r -> unit t =
     fun constr_r ->
      match constr_r.body with
      | StmtBlock stlist -> (
          match List.filter is_this stlist with
          | [] -> return ()
          | [ _ ] ->
              if body_starts_with_this constr_r then return ()
              else error "This constructor call must be in the beginning"
          | _ -> error "More then one constructor calls!" )
      | _ -> error "Constructor body must be in block!"
    in
    monadic_seq_iter
      (convert_table_to_seq cur.constructor_table)
      check_constructor ()

  (* Перенос метода. Тут надо много всего проверять на абстрактность *)
  let process_method : class_r -> method_r -> unit t =
   fun ch cur_method ->
    match get_elem_if_present ch.method_table cur_method.key with
    | None when cur_method.is_abstract ->
        (* Наш абстрактный. Если ребенок абстрактный, то просто переносим метод, иначе бросаем ошибку *)
        if ch.is_abstract then
          return (Hashtbl.add ch.method_table cur_method.key cur_method)
        else error "Abstract method must be overriden"
    | None when not cur_method.is_abstract ->
        (*Наш не абстрактный. Если наш не final - переносим в ребенка *)
        if cur_method.is_overridable then
          return (Hashtbl.add ch.method_table cur_method.key cur_method)
        else return ()
    | Some child_overriden_method ->
        Hashtbl.replace ch.method_table cur_method.key
          { child_overriden_method with is_overriden = true };
        return ()
    | _ -> return ()

  let process_methods par ch =
    monadic_seq_iter
      (convert_table_to_seq par.method_table)
      (process_method ch) ()

  (* Проверка на то, что аннотации @Override у ребенка стоят только у переопределенных методов*)
  let check_override_annotations par ch =
    let check_override_ann : class_r -> method_r -> unit t =
     fun par ch_mr ->
      match ch_mr.has_override_annotation with
      (* Нет аннотации - пропускаем *)
      | false -> return ()
      (* Есть - смотрим, есть ли такой метод в родителе, если есть - все ок, если нет - ошибка*)
      | true -> (
          match get_elem_if_present par.method_table ch_mr.key with
          | None -> error "@Override annotation on not overriden method"
          | _ -> return () )
    in
    monadic_seq_iter
      (convert_table_to_seq ch.method_table)
      (check_override_ann par) ()

  (* Отдельная функция, которая берет родителя и ребенка,
        свойства родителя передает ребенку с необходимыми проверками, далее обрабатывает рекурсивно все дерево наследования от родителя *)
  let rec transfert parent child ht =
    process_fields parent child
    >> process_methods parent child
    >> check_override_annotations parent child
    >> check_cur_constructors parent
    >>= fun _ -> run_transfert_on_children ht child

  (* Запуск transfert на ребенке и детях ребенка *)
  and run_transfert_on_children ht ch =
    let childs_of_child = ch.children_keys in
    monadic_list_iter childs_of_child
      (fun ch_ch_key ->
        get_elem_if_present_m ht ch_ch_key >>= fun child_of_child ->
        transfert ch child_of_child ht)
      ()

  let do_inheritance ht =
    get_elem_if_present_m ht "Object" >>= fun obj_r ->
    let processing ch_key =
      get_elem_if_present_m ht ch_key >>= fun ch -> transfert obj_r ch ht
    in
    monadic_list_iter obj_r.children_keys processing ht

  let load cd_list class_table =
    ( match
        apply parser
          {| 
final class String {
    public final char[] value;

    public String() {
        this.value = new char[0];
    }

    public String(String original) {
        this.value = original.value;
    }

    public String(char[] value) {
        this.value = new char[value.length];
        for (int i = 0; i < value.length; i++) {
            this.value[i] = value[i];
        }
    }

    public int length() {
        return value.length;
    }

    public String concat(String str) {
        int otherLen = str.length();
        if (str.length() == 0) {
            return this;
        }
        int len = value.length;
        char[] newValue = new char[len + otherLen];
        for (int i = 0; i < len; i++) {
            newValue[i] = value[i];
        }
        for (int j = len; j < len + otherLen; j++) {
            newValue[j] = str.value[j - len];
        }
        return new String(newValue);
    }

    public boolean startsWith(String prefix, int toffset) {
        char[] ta = value;
        char[] pa = prefix.value;
        int pc = prefix.length();
        if ((toffset < 0) || (toffset > value.length - pc)) {
            return false;
        }
        for (int i = toffset; i < toffset + pc; i++) {
            if (ta[i] != pa[i - toffset]) {
                return false;
            }
        }
        return true;
    }

    public boolean startsWith(String prefix) {
        return startsWith(prefix, 0);
    }
}
    |}
      with
    | None | Some [] -> error "Syntax error"
    | Some str_d -> return (cd_list @ str_d) )
    >>= fun cds_list ->
    match cds_list with
    | [] -> error "Syntax error or empty file"
    | _ ->
        prepare_object class_table >>= fun table_with_object ->
        c_table_add cds_list table_with_object
        >>= fun table_with_added_classes ->
        add_default_constructors_if_needed table_with_added_classes
        >>= fun with_defaults ->
        update_child_keys with_defaults >>= fun updated_table ->
        do_inheritance updated_table
end

module Main (M : MONADERROR) = struct
  open M

  type variable = {
    v_type : type_t;
    v_key : key_t;
    is_not_mutable : bool;
    assignment_count : int;
    v_value : value;
    scope_level : int;
  }
  [@@deriving show { with_path = false }]

  type signal = WasBreak | WasContinue | WasReturn | NoSignal
  [@@deriving show { with_path = false }]

  type context = {
    cur_object : obj_ref;
    var_table : (key_t, variable) Hashtbl_p.t;
    last_expr_result : value;
    runtime_signal : signal;
    curr_method_type : type_t;
    is_main_scope : bool;
    (* Считаем вложенные циклы *)
    nested_loops_cnt : int;
    scope_level : int;
    (* None - не в конструкторе, Some k -> в конструкторе с ключом k *)
    cur_constr_key : key_t option;
    prev_context : context option;
    obj_created_cnt : int;
    is_creation : bool;
    constr_affilation : key_t option;
  }
  [@@deriving show { with_path = false }]

  let make_context cur_object var_table =
    return
      {
        cur_object;
        var_table;
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

  let rec mfold_left2 f acc l1 l2 =
    match (l1, l2) with
    | [], [] -> return acc
    | x :: xs, y :: ys -> f acc x y >>= fun res -> mfold_left2 f res xs ys
    | _, _ -> error "Wrong lists for fold_left2"

  let rec mfold_right f l acc =
    match l with
    | [] -> return acc
    | x :: xs -> mfold_right f xs acc >>= fun r -> f x r

  let rec mfold_left f acc l =
    match l with
    | [] -> return acc
    | x :: xs -> f acc x >>= fun r -> mfold_left f r xs

  let get_elem_if_present_m ht key =
    match Hashtbl_p.get_elem_if_present ht key with
    | None -> error "No such element in table!"
    | Some el -> return el

  let obj_num obj =
    try get_obj_number_exn obj |> fun n -> return n
    with Invalid_argument m -> error m

  let find_class_with_main ht =
    Hashtbl_p.filter ht (fun _ c -> Hashtbl.mem c.method_table "main@@")
    |> fun fht ->
    match Hashtbl.length fht with
    | 0 -> error "Must be one main method"
    | 1 -> return (seq_hd_exn (convert_table_to_seq fht))
    | _ -> error "Must be one main method"

  let rec add_args buf = function
    | [ x ] -> Printf.bprintf buf "'%c'" x
    | x :: xs ->
        Printf.bprintf buf "'%c'," x;
        add_args buf xs
    | [] -> ()

  let rec expr_type_check t_expr ctx class_table =
    match t_expr with
    | Add (left, right) -> (
        expr_type_check left ctx class_table >>= fun lt ->
        match lt with
        | Int -> (
            expr_type_check right ctx class_table >>= fun rt ->
            match rt with
            | Int | Char -> return Int
            | ClassName "String" -> return (ClassName "String")
            | _ -> error "Wrong type: must be Int, Char or String" )
        | Char -> (
            expr_type_check right ctx class_table >>= fun rt ->
            match rt with
            | Int | Char -> return Int
            | ClassName "String" -> return (ClassName "String")
            | _ -> error "Wrong type: must be Int, Char or String" )
        | ClassName "String" -> (
            expr_type_check right ctx class_table >>= fun rt ->
            match rt with
            | Int | ClassName "String" | Char -> return (ClassName "String")
            | _ -> error "Wrong type: must be Int or String" )
        | _ -> error "Wrong type: must be Int or String" )
    | Sub (left, right)
    | Div (left, right)
    | Mod (left, right)
    | Mult (left, right) -> (
        expr_type_check left ctx class_table >>= fun lt ->
        match lt with
        | Int -> (
            expr_type_check right ctx class_table >>= fun rt ->
            match rt with
            | Int -> return Int
            | _ -> error "Wrong type: must be Int" )
        | _ -> error "Wrong type: must be Int" )
    | PostDec value | PostInc value | PrefDec value | PrefInc value -> (
        expr_type_check value ctx class_table >>= fun vt ->
        match vt with Int -> return Int | _ -> error "Wrong type: must be Int" )
    | And (left, right) | Or (left, right) -> (
        expr_type_check left ctx class_table >>= fun lt ->
        match lt with
        | Bool -> (
            expr_type_check right ctx class_table >>= fun rt ->
            match rt with
            | Bool -> return Bool
            | _ -> error "Wrong type: must be Bool" )
        | _ -> error "Wrong type: must be Bool" )
    | Not value -> (
        expr_type_check value ctx class_table >>= fun vt ->
        match vt with
        | Bool -> return Bool
        | _ -> error "Wrong type: must be Bool" )
    | Less (left, right)
    | More (left, right)
    | LessOrEqual (left, right)
    | MoreOrEqual (left, right) -> (
        expr_type_check left ctx class_table >>= fun lt ->
        match lt with
        | Int -> (
            expr_type_check right ctx class_table >>= fun rt ->
            match rt with
            | Int -> return Bool
            | _ -> error "Wrong type: must be Int" )
        | _ -> error "Wrong type: must be Int" )
    | Equal (left, right) | NotEqual (left, right) -> (
        expr_type_check left ctx class_table >>= fun lt ->
        match lt with
        | Int -> (
            expr_type_check right ctx class_table >>= fun rt ->
            match rt with
            | Int -> return Bool
            | _ -> error "Wrong type: must be Int" )
        | Char -> (
            expr_type_check right ctx class_table >>= fun rt ->
            match rt with
            | Char -> return Bool
            | _ -> error "Wrong type: must be Char" )
        | Bool -> (
            expr_type_check right ctx class_table >>= fun rt ->
            match rt with
            | Bool -> return Bool
            | _ -> error "Wrong type: must be Bool" )
        | Array arr_lt -> (
            expr_type_check right ctx class_table >>= fun rt ->
            match rt with
            | Array arr_rt when arr_lt = arr_rt -> return Bool
            | _ -> error "Wrong type: must be Array with right type!" )
        | ClassName ls -> (
            expr_type_check right ctx class_table >>= fun rt ->
            match rt with
            | ClassName rs when ls = rs -> return Bool
            | ClassName "null" -> return Bool
            | _ -> error "Wrong class type!" )
        | _ -> error "Wrong type in equal-expression!" )
    | Null -> return (ClassName "null")
    | This -> (
        match ctx.cur_object with
        | RObj { class_key = k; _ } -> return (ClassName k)
        | RNull -> error "Null object in context!" )
    | Super -> (
        match ctx.cur_object with
        | RObj { class_key = k; _ } ->
            get_elem_if_present_m class_table k >>= fun k_clr ->
            ( match k_clr.parent_key with
            | Some k -> return k
            | None -> error "Error: must be parent!" )
            >>= fun par_k -> return (ClassName par_k)
        | RNull -> error "Null object in context!" )
    | CallMethod (Super, _) -> return Void
    | CallMethod (This, _) -> return Void
    (* По нашей модели такой вызов мог произойти только внутри какого-то объекта *)
    | CallMethod (Identifier m_ident, args) ->
        let curr_obj_key =
          match ctx.cur_object with
          | RNull -> "null"
          | RObj { class_key = key; _ } -> key
        in
        get_elem_if_present_m class_table curr_obj_key >>= fun curr_class ->
        check_method curr_class m_ident args ctx class_table >>= fun mr ->
        return mr.m_type
    | FieldAccess (obj_expr, Identifier f_key) -> (
        expr_type_check obj_expr ctx class_table >>= fun obj_c ->
        match obj_c with
        | ClassName "null" -> error "NullPointerException"
        | ClassName obj_key -> (
            get_elem_if_present_m class_table obj_key >>= fun obj_class ->
            let var_field_o = get_elem_if_present obj_class.field_table f_key in
            match var_field_o with
            | None -> error "No such field in class!"
            | Some var_field -> return var_field.f_type )
        | Array _ ->
            if f_key = "length" then return Int
            else error "Wrong type: must be object reference"
        | _ -> error "Wrong type: must be object reference" )
    | FieldAccess (obj_expr, CallMethod (Identifier m_ident, args)) -> (
        (* Чекаем то, что перед точкой - это должен быть объект *)
        expr_type_check obj_expr ctx class_table
        >>= fun obj_c ->
        match obj_c with
        | ClassName "null" -> error "NullPointerException"
        | ClassName obj_key ->
            get_elem_if_present_m class_table obj_key >>= fun obj_class ->
            check_method obj_class m_ident args ctx class_table >>= fun m_r ->
            return m_r.m_type
        | _ -> error "Wrong type: must be object reference" )
    | ArrayAccess (arr_expr, index_expr) -> (
        expr_type_check index_expr ctx class_table >>= fun ind_t ->
        match ind_t with
        | Int -> (
            expr_type_check arr_expr ctx class_table >>= fun arr_t ->
            match arr_t with
            | Array t -> return t
            | _ -> error "Wrong type, must be Array!" )
        | _ -> error "Wrong type: must be Int" )
    | ArrayCreateSized (arr_type, size) -> (
        expr_type_check size ctx class_table >>= fun size_t ->
        match size_t with
        | Int -> (
            match arr_type with
            | Void -> error "Wrong type of array"
            | Array _ -> error "Wrong type of Array"
            | _ -> return (Array arr_type) )
        | _ -> error "Wrong type: size must be Int" )
    | ArrayCreateElements (arr_type, elems) ->
        let rec process_list_el = function
          | [] -> return (Array arr_type)
          | el :: els -> (
              expr_type_check el ctx class_table >>= fun el_type ->
              match arr_type with
              | ClassName cleft -> (
                  match el_type with
                  | ClassName "null" -> process_list_el els
                  | ClassName cright ->
                      check_classname_assign cleft cright class_table
                      >> process_list_el els
                  | _ -> error "Wrong type of array element: creation" )
              | _ ->
                  if arr_type = el_type then process_list_el els
                  else error "Wrong type of array element: creation" )
        in
        process_list_el elems
    | ClassCreate (Name class_name, args) -> (
        match get_elem_if_present class_table class_name with
        | None -> error ("No such class : " ^ class_name ^ "\n")
        | Some cl_elem -> (
            (* Если аргументов у конструктора нет - все норм, пустой конструктор всегда имеется *)
            match args with
            | [] -> return (ClassName class_name)
            | _ ->
                (* Если отработал нормально - значит, все окей, просто возвращаем тип созданный *)
                check_constructor cl_elem args ctx class_table
                >> return (ClassName class_name) ) )
    | Identifier key -> (
        (* Смотрим среди локальных переменных контекста *)
        let var_o = get_elem_if_present ctx.var_table key in
        match var_o with
        (* Нет, а вдруг есть среди полей объекта текущего класса? *)
        | None -> (
            match ctx.cur_object with
            | RObj { field_ref_table = ft; _ } -> (
                (* Смотрим в таблице полей конкретного объекта *)
                match get_elem_if_present ft key with
                | None ->
                    error ("No such variable or field with this name : " ^ key)
                | Some fr -> return fr.f_type )
            | _ -> error "NullPointerException" )
        (* Нашли в таблице локальных переменных - возвращаем его тип *)
        | Some v -> return v.v_type )
    | Const value -> (
        match value with
        | VBool _ -> return Bool
        | VInt _ -> return Int
        | VChar _ -> return Char
        | VString _ -> return (ClassName "String")
        | VObjectRef RNull -> return (ClassName "null")
        | VObjectRef (RObj { class_key = ck; _ }) -> return (ClassName ck)
        | VArray ANull -> return (Array Void)
        | VArray (Arr { a_type = t; _ }) -> return (Array t)
        | _ -> error "Wrong const value" )
    | Assign (left, right) -> (
        expr_type_check left ctx class_table >>= fun lt ->
        match lt with
        | Void -> error "Wrong assign type"
        | ClassName cleft_key -> (
            expr_type_check right ctx class_table >>= fun rt ->
            match rt with
            (* Null можно присвоить объекту любого класса *)
            | ClassName "null" -> return (ClassName cleft_key)
            | ClassName cright_key ->
                (* Среди родителей правого надо найти левый *)
                check_classname_assign cleft_key cright_key class_table
            | _ -> error "Wrong assign types!" )
        | Array (ClassName cleft_key) -> (
            expr_type_check right ctx class_table >>= fun rt ->
            match rt with
            | Array (ClassName cright_key) ->
                (* Массивы ковариантны, значит с ними надо проверять деревья родителей также, как и в предыдущем случае *)
                check_classname_assign cleft_key cright_key class_table
            | _ -> error "Wrong assign types" )
        | _ ->
            expr_type_check right ctx class_table >>= fun rt ->
            if lt = rt then return rt else error "Wrong assign types!" )
    | _ -> error "Wrong_expression"

  and check_classname_assign cleft_key cright_key class_table =
    (* Среди родителей правого надо найти левый. key - текущий ключ правого *)
    let rec check_parent_tree key =
      get_elem_if_present_m class_table key >>= fun clr_by_key ->
      (* Смотрим, чтобы было совпадение: смотрим текущий ключ с левым *)
      if clr_by_key.this_key = cleft_key then return (ClassName cright_key)
      else
        match clr_by_key.parent_key with
        | None -> error "Wrong assign type!"
        | Some par_k -> check_parent_tree par_k
    in
    (* Иерархию перебираем у правого *)
    check_parent_tree cright_key

  and check_classname_bool cleft_key cright_key class_table =
    (* Среди родителей правого надо найти левый. key - текущий ключ правого *)
    let rec check_parent_tree key =
      match get_elem_if_present class_table key with
      | None -> false
      | Some clr_by_key -> (
          if
            (* Смотрим, чтобы было совпадение: смотрим текущий ключ с левым *)
            clr_by_key.this_key = cleft_key
          then true
          else
            match clr_by_key.parent_key with
            | None -> false
            | Some par_k -> check_parent_tree par_k )
    in
    (* Иерархию перебираем у правого *)
    check_parent_tree cright_key

  and make_type_string elist ctx class_table =
    let rec helper_type lst acc =
      match lst with
      | [] -> return acc
      | e :: es ->
          expr_type_check e ctx class_table >>= fun cur_t ->
          helper_type es (acc ^ show_type_t cur_t)
    in
    helper_type elist ""

  and check_method cl_r m_name expr_list check_ctx class_table =
    (* смотрим совпадение типов на определенной позиции *)
    let check_type_m : int -> type_t -> key_t -> method_r -> bool =
     fun pos curr_type _ value ->
      match List.nth_opt value.args pos with
      | None -> false
      | Some (found_type, _) -> (
          match curr_type with
          | ClassName "null" -> (
              match found_type with ClassName _ -> true | _ -> false )
          | ClassName curr_key -> (
              match found_type with
              | ClassName found_key ->
                  check_classname_bool found_key curr_key class_table
              | _ -> false )
          | _ -> found_type = curr_type )
    in
    let rec helper_checker curr_ht pos e_list ctx =
      match Hashtbl.length curr_ht with
      (* 0 осталось методов - значит ни один не подошел. Ошибка *)
      | 0 -> error "No such method!"
      | other -> (
          match e_list with
          (* Если перебрали все аргументы - смотрим на кол-во элементов хеш-таблицы *)
          | [] -> (
              match other with
              (* 1 - берем единственный подходящий *)
              | 1 -> return (seq_hd_exn (convert_table_to_seq curr_ht))
              (* Методов с точно такими или полиморфными типами больше одного. Значит cannot resolve *)
              | _ -> error "Cannot resolve method" )
          | e :: es ->
              expr_type_check e ctx class_table >>= fun e_type ->
              helper_checker
                (Hashtbl_p.filter curr_ht (check_type_m pos e_type))
                (pos + 1) es ctx )
    in
    (* Вначале фильтруем по имени, а потом по количеству аргументов *)
    Hashtbl_p.filter cl_r.method_table (fun _ mr -> startswith mr.key m_name)
    |> fun filtered_by_name ->
    helper_checker
      (Hashtbl_p.filter filtered_by_name (fun _ mr ->
           List.length mr.args = List.length expr_list))
      0 expr_list check_ctx

  and check_constructor cl_r expr_list check_ctx class_table =
    let check_type_c : int -> type_t -> key_t -> constructor_r -> bool =
     fun pos curr_type _ value ->
      match List.nth_opt value.args pos with
      | None -> false
      | Some (found_type, _) -> (
          match curr_type with
          | ClassName "null" -> (
              match found_type with ClassName _ -> true | _ -> false )
          | ClassName curr_key -> (
              match found_type with
              | ClassName found_key ->
                  check_classname_bool found_key curr_key class_table
              | _ -> false )
          | _ -> found_type = curr_type )
    in
    let rec helper_checker_c curr_ht pos e_list ctx =
      match Hashtbl.length curr_ht with
      | 0 -> error "No such constructor!"
      | other -> (
          match e_list with
          | [] -> (
              match other with
              | 1 -> return (seq_hd_exn (convert_table_to_seq curr_ht))
              | _ -> error "Cannot resolve constructor!" )
          | e :: es ->
              expr_type_check e ctx class_table >>= fun e_type ->
              helper_checker_c
                (Hashtbl_p.filter curr_ht (check_type_c pos e_type))
                (pos + 1) es ctx )
    in
    helper_checker_c
      (Hashtbl_p.filter cl_r.constructor_table (fun _ cr ->
           List.length cr.args = List.length expr_list))
      0 expr_list check_ctx

  let get_obj_value = function VObjectRef o -> o | _ -> RNull

  let make_list_of_elem el size =
    let rec helper acc curr =
      match curr with 0 -> acc | x -> helper (el :: acc) (x - 1)
    in
    helper [] size

  let inc_scope_level ctx = { ctx with scope_level = ctx.scope_level + 1 }

  let dec_scope_level ctx = { ctx with scope_level = ctx.scope_level - 1 }

  let check_assign_cnt_f : field_ref -> unit M.t =
   fun fld ->
    match fld.assignment_count with
    | 0 -> return ()
    | _ when not fld.is_not_mutable -> return ()
    | _ -> error "Assignment to a constant field"

  let check_assign_cnt_v var =
    match var.assignment_count with
    | 0 -> return ()
    | _ when not var.is_not_mutable -> return ()
    | _ -> error "Assignment to a constant variable"

  let delete_scope_var : context -> context M.t =
   fun ctx ->
    let delete : key_t -> variable -> unit =
     fun key el ->
      if el.scope_level = ctx.scope_level then Hashtbl.remove ctx.var_table key
    in
    Hashtbl.iter delete ctx.var_table;
    return ctx

  let is_good_for_stmt = function
    | PostDec _ | PostInc _ | PrefDec _ | PrefInc _
    | CallMethod (_, _)
    | FieldAccess (_, CallMethod (_, _))
    | Assign (_, _) ->
        true
    | _ -> false

  let rec eval_stmt stmt sctx class_table =
    match stmt with
    | StmtBlock st_list ->
        let rec helper_eval : stmt list -> context -> context M.t =
         fun stl hctx ->
          match stl with
          | [] -> return hctx
          | st :: sts -> (
              match st with
              | (Break | Continue | Return _) when sts <> [] ->
                  error "There are unreachable statements"
              | _
                when hctx.nested_loops_cnt >= 1
                     && hctx.runtime_signal = WasBreak ->
                  return hctx
              | _
                when hctx.nested_loops_cnt >= 1
                     && hctx.runtime_signal = WasContinue ->
                  return hctx
              | _ when hctx.runtime_signal = WasReturn ->
                  return hctx
                  (*Счетчик return обновляется после выхода из метода*)
              | _ ->
                  eval_stmt st hctx class_table >>= fun head_ctx ->
                  helper_eval sts head_ctx )
        in
        helper_eval st_list sctx >>= fun sbctx ->
        if sbctx.is_main_scope then return sbctx else delete_scope_var sbctx
    | While (bexpr, lstmt) -> (
        let was_main = sctx.is_main_scope in
        let rec loop s ctx =
          (* Сразу проверяем брейк, случился ли он, случился - выходим из цикла*)
          if ctx.runtime_signal = WasBreak then
            match s with
            (* Если был блок - то еще надо уровень видимости понизить *)
            | StmtBlock _ ->
                return
                  (dec_scope_level
                     {
                       ctx with
                       runtime_signal = NoSignal;
                       nested_loops_cnt = ctx.nested_loops_cnt - 1;
                     })
            | _ ->
                return
                  {
                    ctx with
                    runtime_signal = NoSignal;
                    nested_loops_cnt = ctx.nested_loops_cnt - 1;
                  }
          else
            eval_expr bexpr ctx class_table >>= fun bectx ->
            match bectx.last_expr_result with
            | VBool false -> (
                match s with
                | StmtBlock _ ->
                    return
                      (dec_scope_level
                         {
                           bectx with
                           nested_loops_cnt = ctx.nested_loops_cnt - 1;
                           is_main_scope = was_main;
                         })
                | _ ->
                    return
                      { bectx with nested_loops_cnt = ctx.nested_loops_cnt - 1 }
                )
            | VBool true -> (
                eval_stmt s bectx class_table >>= fun lctx ->
                match lctx.runtime_signal with
                (* Вылетел return - все прерываем, возвращаем контекст *)
                | WasReturn -> return lctx
                (*Может вылететь continue - значит циклимся заново*)
                | WasContinue -> loop s { lctx with runtime_signal = NoSignal }
                | _ -> loop s lctx )
            | _ -> error "Wrong expression type for while stametent"
        in
        match lstmt with
        | StmtBlock _ ->
            loop lstmt
              (inc_scope_level
                 {
                   sctx with
                   nested_loops_cnt = sctx.nested_loops_cnt + 1;
                   is_main_scope = false;
                 })
        | _ ->
            loop lstmt
              { sctx with nested_loops_cnt = sctx.nested_loops_cnt + 1 } )
    | Break ->
        (* Break не может быть в цикле - проверяем это, если все ок - то просто возвращаем контекст с установленным флагом *)
        if sctx.nested_loops_cnt <= 0 then error "No loop for break"
        else return { sctx with runtime_signal = WasBreak }
    | Continue ->
        (* Continue не может быть в цикле - проверяем это, если все ок - то просто возвращаем контекст с установленным флагом *)
        if sctx.nested_loops_cnt <= 0 then error "No loop for continue"
        else return { sctx with runtime_signal = WasContinue }
    | If (bexpr, then_stmt, else_stmt_o) -> (
        eval_expr bexpr sctx class_table >>= fun bectx ->
        let was_main = bectx.is_main_scope in
        match bectx.last_expr_result with
        | VBool true -> (
            match then_stmt with
            | StmtBlock _ ->
                eval_stmt then_stmt
                  (inc_scope_level { bectx with is_main_scope = false })
                  class_table
                >>= fun tctx ->
                return (dec_scope_level { tctx with is_main_scope = was_main })
            | _ -> eval_stmt then_stmt bectx class_table )
        | VBool false -> (
            match else_stmt_o with
            | Some (StmtBlock _ as else_stmt) ->
                eval_stmt else_stmt
                  (inc_scope_level { bectx with is_main_scope = false })
                  class_table
                >>= fun ectx ->
                return (dec_scope_level { ectx with is_main_scope = was_main })
            | Some else_stmt -> eval_stmt else_stmt bectx class_table
            | None -> return sctx )
        | _ -> error "Wrong type for condition statement" )
    | For (dec_stmt_o, bexpr_o, after_expr_list, body_stmt) ->
        (* С циклом for scope_level всегда увеличивается, не смотря на наличие/отсутствие блока тела *)
        let was_main = sctx.is_main_scope in
        ( match dec_stmt_o with
        | None -> return (inc_scope_level { sctx with is_main_scope = false })
        | Some dec_stmt ->
            eval_stmt dec_stmt
              (inc_scope_level { sctx with is_main_scope = false })
              class_table )
        >>= fun dec_ctx ->
        let rec loop bs afs ctx =
          (* Сразу проверяем брейк, случился ли он, случился - выходим из цикла*)
          if ctx.runtime_signal = WasBreak then
            delete_scope_var
              {
                ctx with
                runtime_signal = NoSignal;
                nested_loops_cnt = ctx.nested_loops_cnt - 1;
                scope_level = ctx.scope_level - 1;
                is_main_scope = was_main;
              }
          else
            (*Стандартно: смотрим результат бул-выражения, если true - вычислить тело и инкременты после*)
            ( match bexpr_o with
            | None -> return { ctx with last_expr_result = VBool true }
            | Some bexpr -> eval_expr bexpr ctx class_table )
            >>= fun bectx ->
            match bectx.last_expr_result with
            (* Увидели false - значит уже не циклимся, возвращаем контекст с уменьшенным счетчиком вложенных циклов и scope*)
            | VBool false ->
                delete_scope_var
                  {
                    bectx with
                    nested_loops_cnt = bectx.nested_loops_cnt - 1;
                    scope_level = bectx.scope_level - 1;
                    is_main_scope = was_main;
                  }
            | VBool true -> (
                let rec eval_inc_expr_list e_list c =
                  match e_list with
                  | [] -> return c
                  | e :: es ->
                      if is_good_for_stmt e then
                        eval_expr e c class_table >>= fun ehctx ->
                        eval_inc_expr_list es ehctx
                      else error "Wrong expression for after body list"
                in
                (* Переменные внутри самого блока будут находиться в большем scope level, чем из инициализатора *)
                eval_stmt bs
                  { bectx with scope_level = dec_ctx.scope_level + 1 }
                  class_table
                >>= fun bdctx ->
                match bdctx.runtime_signal with
                (*Может вылететь continue - значит циклимся заново, инкременты не вычисляем*)
                | WasReturn -> return { bdctx with is_main_scope = was_main }
                (*Может вылететь continue - значит циклимся заново, но инкременты вычисляем*)
                | WasContinue ->
                    eval_inc_expr_list afs bdctx >>= fun after_ctx ->
                    loop bs afs { after_ctx with runtime_signal = NoSignal }
                | _ ->
                    eval_inc_expr_list afs bdctx >>= fun after_ctx ->
                    loop bs afs after_ctx )
            | _ -> error "Wrong condition type in for statement"
        in
        loop body_stmt after_expr_list
          { dec_ctx with nested_loops_cnt = sctx.nested_loops_cnt + 1 }
    | Return None when sctx.curr_method_type = Void ->
        (* Если тип Void - выходим со значением VVoid поставленным флагом, что был return *)
        return
          { sctx with last_expr_result = VVoid; runtime_signal = WasReturn }
    | Return None -> error "Return value type mismatch"
    | Return (Some rexpr) ->
        expr_type_check rexpr sctx class_table >>= fun rexpr_type ->
        if rexpr_type <> sctx.curr_method_type then
          error "Return value type mismatch"
        else
          (* Возвращаем контекст, в котором есть результат выражения, но не забываем поставить флаг, что был return *)
          eval_expr rexpr sctx class_table >>= fun rectx ->
          return { rectx with runtime_signal = WasReturn }
    | Expression sexpr ->
        if is_good_for_stmt sexpr then
          eval_expr sexpr sctx class_table >>= fun ectx -> return ectx
        else error "Wrong expression for statement"
    | VarDec (final_mod_o, vars_type, var_list) ->
        let is_final : modifier option -> bool = function
          | Some Final -> true
          | _ -> false
        in
        let rec helper_vardec v_list vctx =
          match v_list with
          | [] -> return vctx
          | (Name name, var_expr_o) :: vs -> (
              match vctx.cur_object with
              | RNull -> error "NullPointerException"
              | RObj { field_ref_table = frt; _ } ->
                  ( if
                    (* Смотрим, чтобы подобного имени не было ни среди локальных переменных, ни среди полей класса *)
                    Hashtbl.mem vctx.var_table name || Hashtbl.mem frt name
                  then error "Variable with this name is already defined"
                  else
                    match var_expr_o with
                    (* Если ничего нет - инициализируем базовым значением *)
                    | None ->
                        Hashtbl.add vctx.var_table name
                          {
                            v_type = vars_type;
                            v_key = name;
                            is_not_mutable = is_final final_mod_o;
                            assignment_count = 0;
                            v_value = get_init_value_of_type vars_type;
                            scope_level = vctx.scope_level;
                          };
                        return vctx
                        (* Если что-то есть - присваиваем значение, вычисленное справа *)
                    | Some var_expr -> (
                        expr_type_check var_expr vctx class_table
                        >>= fun var_expr_type ->
                        (* Добавить в таблицу переменных контекста то, что в выражении переменной справа *)
                        let add_var ve =
                          eval_expr ve vctx class_table >>= fun vare_ctx ->
                          Hashtbl.add vare_ctx.var_table name
                            {
                              v_type = var_expr_type;
                              v_key = name;
                              is_not_mutable = is_final final_mod_o;
                              assignment_count = 1;
                              v_value = vare_ctx.last_expr_result;
                              scope_level = vare_ctx.scope_level;
                            };
                          return vare_ctx
                        in
                        match var_expr_type with
                        (* Null можно присвоить любому объекту *)
                        | ClassName "null" -> (
                            match vars_type with
                            | ClassName _ -> add_var var_expr
                            | _ -> error "Wrong assign type in declaration" )
                        (* Если тип справа - класс, то надо аккуратно проверить тип, соблюдая наследование *)
                        | ClassName cright -> (
                            match vars_type with
                            | ClassName cleft ->
                                check_classname_assign cleft cright class_table
                                (* Тип проверится нормально - тогда просто добавим *)
                                >>=
                                fun _ -> add_var var_expr
                            | _ -> error "Wrong assign type in declaration" )
                        (* Если тип справа - массив объектов класса, то тоже надо проверять наследование, т.к. есть ковариантность *)
                        | Array (ClassName cright) -> (
                            match vars_type with
                            | Array (ClassName cleft) ->
                                check_classname_assign cleft cright class_table
                                >>= fun _ -> add_var var_expr
                            | _ -> error "Wrong assign type in declaration" )
                        | _ when var_expr_type = vars_type -> add_var var_expr
                        | _ ->
                            error
                              ( "Wrong value type for declared variable:"
                              ^ show_type_t var_expr_type ) ) )
                  >>= fun head_ctx -> helper_vardec vs head_ctx )
        in
        helper_vardec var_list sctx

  and eval_expr expr ectx class_table =
    let eval_e e_expr ctx =
      let eval_op left right op adding =
        eval_expr left ctx class_table >>= fun lctx ->
        eval_expr right lctx class_table >>= fun rctx ->
        let l_value = lctx.last_expr_result in
        let r_value = rctx.last_expr_result in
        match (l_value, r_value) with
        | ( VObjectRef (RObj { class_key = "String"; _ }),
            VObjectRef (RObj { class_key = "String"; _ }) )
          when adding ->
            eval_expr
              (FieldAccess (left, CallMethod (Identifier "concat", [ right ])))
              rctx class_table
        | VObjectRef (RObj { class_key = "String"; _ }), v2 when adding ->
            eval_expr
              (FieldAccess
                 ( left,
                   CallMethod (Identifier "concat", [ Const (val_to_str v2) ])
                 ))
              rctx class_table
        | v1, VObjectRef (RObj { class_key = "String"; _ }) when adding ->
            eval_expr
              (FieldAccess
                 ( Const (val_to_str v1),
                   CallMethod (Identifier "concat", [ right ]) ))
              rctx class_table
        | v1, v2 -> (
            try
              let new_val = op v1 v2 in
              return { rctx with last_expr_result = new_val }
            with
            | Invalid_argument m -> error m
            | Division_by_zero -> error "ArithmeticException: division by zero"
            )
      in
      let eval_un v_expr op =
        eval_expr v_expr ctx class_table >>= fun vctx ->
        let v = vctx.last_expr_result in
        let new_v = op v in
        try return { vctx with last_expr_result = new_v }
        with Invalid_argument m -> error m
      in
      match e_expr with
      | Add (left, right) -> eval_op left right ( ++ ) true
      | Sub (left, right) -> eval_op left right ( -- ) false
      | Mult (left, right) -> eval_op left right ( ** ) false
      | Div (left, right) -> eval_op left right ( // ) false
      | Mod (left, right) -> eval_op left right ( %% ) false
      | And (left, right) -> eval_op left right ( &&& ) false
      | Or (left, right) -> eval_op left right ( ||| ) false
      | Not bexp -> eval_un bexp not_v
      | Less (left, right) -> eval_op left right ( <<< ) false
      | More (left, right) -> eval_op left right ( >>> ) false
      | LessOrEqual (left, right) -> eval_op left right ( <<== ) false
      | MoreOrEqual (left, right) -> eval_op left right ( >>== ) false
      | Equal (left, right) -> (
          expr_type_check left ctx class_table >>= fun l_type ->
          match l_type with
          (* Если тип - объект, то если метод equals переопределен, надо вызвать его *)
          | ClassName l_key ->
              get_elem_if_present_m class_table l_key >>= fun left_clr ->
              get_elem_if_present_m left_clr.method_table equals_key
              >>= fun left_eqr ->
              if left_eqr.is_overriden then
                eval_expr
                  (FieldAccess
                     (left, CallMethod (Identifier "equals", [ right ])))
                  ctx class_table
              else eval_op left right ( === ) false
          | _ -> eval_op left right ( === ) false )
      | NotEqual (left, right) -> eval_op left right ( !=! ) false
      | Const v -> (
          match v with
          | VString str -> (
              let buf = Buffer.create 1000 in
              Buffer.add_string buf "new String(new char[]{";
              add_args buf (Opal.explode str);
              Buffer.add_string buf "})";
              match apply Expr.expression (Buffer.contents buf) with
              | None -> error "Syntax error"
              | Some str_expr -> eval_expr str_expr ctx class_table )
          | _ -> return { ctx with last_expr_result = v } )
      | Identifier id -> (
          (* Пытаемся найти среди переменных в таблице контекста *)
          match get_elem_if_present ctx.var_table id with
          | Some var_by_id ->
              return { ctx with last_expr_result = var_by_id.v_value }
              (* Не нашли - ищем среди полей текущего класса *)
          | None -> (
              try
                get_obj_info_exn ctx.cur_object |> fun (_, frt, _) ->
                match get_elem_if_present frt id with
                | Some f -> return { ctx with last_expr_result = f.f_value }
                | None -> error "No such variable or field!"
              with Invalid_argument m | Failure m -> error m ) )
      | Null -> return { ctx with last_expr_result = VObjectRef RNull }
      (* Эта штука исполняется в стейтмент-блоке *)
      | CallMethod (This, args) ->
          (* Должна быть проверка на то, что мы в конструкторе!*)
          ( match ctx.cur_constr_key with
          | None -> error "this(...) call must be in constructor!"
          | Some k -> return k )
          >>= fun external_constr_key ->
          let get_cur_class_key =
            match ctx.cur_object with
            | RNull -> error "NullPointerException"
            | RObj { class_key = key; _ } -> return key
          in
          get_cur_class_key >>= fun curr_class_key ->
          get_elem_if_present_m class_table curr_class_key
          >>= fun cur_class_r ->
          check_constructor cur_class_r args ctx class_table >>= fun constr_r ->
          if constr_r.key = external_constr_key then
            error "Constructor recursion!"
          else
            prepare_constructor_block constr_r.body cur_class_r
            >>= fun c_body ->
            (*Подготавливаем таблицу аргументов в контексте без локальных переменных, остальное остается прежним*)
            ( try
                prepare_table_with_args_exn (Hashtbl.create 100) args
                  constr_r.args ctx class_table
              with Invalid_argument m -> error m )
            >>= fun (vt, vctx) ->
            (* Контекст работы с телом: текущий объект, но посчитаны все аргументы и распиханы по таблице переменных *)
            eval_stmt c_body
              { vctx with var_table = vt; is_creation = true }
              class_table
            >>= fun res_ctx ->
            return
              {
                res_ctx with
                last_expr_result = VVoid;
                var_table = ctx.var_table;
                constr_affilation = ctx.constr_affilation;
                is_creation = true;
              }
      | CallMethod (Super, args) -> (
          ( match ctx.cur_constr_key with
          | None -> error "super(...) call must be in constructor!"
          | Some k -> return k )
          >>= fun _ ->
          ( match ctx.constr_affilation with
          | None -> error "super(...) call must be in constructor!"
          | Some c_aff -> return c_aff )
          >>= fun curr_class_key ->
          get_elem_if_present_m class_table curr_class_key
          >>= fun cur_class_r ->
          match cur_class_r.parent_key with
          | None ->
              error "Bad super(...) call usage : this class has no parent!"
          | Some par_key ->
              get_elem_if_present_m class_table par_key >>= fun par_r ->
              check_constructor par_r args ctx class_table >>= fun constr_r ->
              prepare_constructor_block constr_r.body par_r
              >>= fun par_constr_body ->
              ( try
                  prepare_table_with_args_exn (Hashtbl.create 100) args
                    constr_r.args ctx class_table
                with Invalid_argument m -> error m )
              (* ctx - включает в себя текущий объект внешнего конструктора, таблицу переменных в аргументах внешнего конструктора *)
              >>=
              fun (vt, vctx) ->
              eval_stmt par_constr_body
                {
                  vctx with
                  var_table = vt;
                  is_creation = true;
                  constr_affilation = Some par_key;
                  cur_constr_key = Some constr_r.key;
                }
                class_table
              (* возвращается контекст с тем же объектом, но у него уже что-то проинициализировано *)
              >>=
              fun res_ctx ->
              return
                {
                  res_ctx with
                  last_expr_result = VVoid;
                  var_table = ctx.var_table;
                  constr_affilation = ctx.constr_affilation;
                  is_creation = true;
                } )
      | This -> return { ctx with last_expr_result = VObjectRef ctx.cur_object }
      | FieldAccess (obj_expr, Identifier f_key) -> (
          eval_expr obj_expr ctx class_table >>= fun octx ->
          let obj = octx.last_expr_result in
          match obj with
          | VObjectRef (RObj { field_ref_table = frt; _ }) ->
              get_elem_if_present_m frt f_key >>= fun fld ->
              return { octx with last_expr_result = fld.f_value }
          | VArray (Arr { length = alen; _ }) when f_key = "length" ->
              return { octx with last_expr_result = VInt alen }
          | VObjectRef RNull | VArray ANull -> error "NullPointerException"
          | _ -> error "Must be non-null object or array with length call!" )
      | FieldAccess (obj_expr, CallMethod (Identifier m_name, args)) -> (
          eval_expr obj_expr ctx class_table >>= fun octx ->
          let obj = octx.last_expr_result in
          match obj with
          | VObjectRef RNull -> error "NullPointerException"
          | VObjectRef
              (RObj { class_key = cl_k; field_ref_table = c_frt; number = c_n })
            -> (
              (* Смотрим класс, к которому принадлежит объект, с проверкой на существование *)
              match get_elem_if_present class_table cl_k with
              | None -> error "No such object in class!"
              | Some obj_class ->
                  (* Смотрим метод у этого класса, опять с проверкой на существование *)
                  check_method obj_class m_name args octx class_table
                  >>= fun mr ->
                  ( match mr.body with
                  | None -> error "Error: abstract class creation!"
                  | Some b -> return b )
                  >>= fun m_body ->
                  let new_var_table : (key_t, variable) Hashtbl_p.t =
                    Hashtbl.create 100
                  in
                  ( try
                      prepare_table_with_args_exn new_var_table args mr.args
                        octx class_table
                    with Invalid_argument m -> error m )
                  >>= fun (new_vt, new_ctx) ->
                  (* Тело метода исполняется в новом контексте с переменными из переданных аргументов *)
                  eval_stmt m_body
                    {
                      cur_object =
                        RObj
                          {
                            class_key = cl_k;
                            field_ref_table = c_frt;
                            number = c_n;
                          };
                      var_table = new_vt;
                      last_expr_result = VVoid;
                      runtime_signal = NoSignal;
                      curr_method_type = mr.m_type;
                      is_main_scope = false;
                      nested_loops_cnt = 0;
                      scope_level = 0;
                      cur_constr_key = None;
                      prev_context = Some ctx;
                      obj_created_cnt = ctx.obj_created_cnt;
                      is_creation = false;
                      constr_affilation = None;
                    }
                    class_table
                  (* От контекста нас интересует только результат работы метода *)
                  >>=
                  fun m_res_ctx ->
                  (* После отработки метода возвращаем контекст с результатом работы метода
                     Если внутри метода менялись какие-то состояния объектов -
                     они должны поменяться исходя из мутабельности хеш-таблиц в результате присваиваний *)
                  return
                    {
                      new_ctx with
                      last_expr_result =
                        ( if mr.m_type = Void then VVoid
                        else m_res_ctx.last_expr_result );
                      obj_created_cnt = m_res_ctx.obj_created_cnt;
                      is_creation = false;
                    } )
          | _ -> error "Must be non-null object!" )
      (* Это в случае, если вызываем внутри какого-то объекта. This нам вернет этот объект при вычислении *)
      | CallMethod (Identifier m, args) ->
          eval_expr
            (FieldAccess (This, CallMethod (Identifier m, args)))
            ctx class_table
      | ArrayAccess (arr_expr, index_expr) -> (
          eval_expr arr_expr ctx class_table >>= fun arrctx ->
          eval_expr index_expr ctx class_table >>= fun indctx ->
          let arr_v = arrctx.last_expr_result in
          let ind_v = indctx.last_expr_result in
          match arr_v with
          | VArray (Arr { values = a_values; _ }) -> (
              match ind_v with
              | VInt i when i < 0 || i >= List.length a_values ->
                  error "ArrayOutOfBoundsException"
              | VInt i ->
                  return { indctx with last_expr_result = List.nth a_values i }
              | _ -> error "Index must be int!" )
          | VArray ANull -> error "NullPointerException"
          | _ -> error "Must be array!" )
      | ArrayCreateSized (arr_type, size_expr) -> (
          eval_expr size_expr ctx class_table >>= fun szctx ->
          let size_v = szctx.last_expr_result in
          let init_v = get_init_value_of_type arr_type in
          match size_v with
          | VInt size ->
              return
                {
                  szctx with
                  last_expr_result =
                    VArray
                      (Arr
                         {
                           a_type = arr_type;
                           values = make_list_of_elem init_v size;
                           number = szctx.obj_created_cnt + 1;
                           length = size;
                         });
                  obj_created_cnt = szctx.obj_created_cnt + 1;
                }
          | _ -> error "Size must be int!" )
      | ArrayCreateElements (a_type, expr_list) ->
          let make_val_list ex_list fctx =
            mfold_left
              (fun (a_lst, hctx) e ->
                eval_expr e hctx class_table >>= fun ectx ->
                let head_val = ectx.last_expr_result in
                return (a_lst @ [ head_val ], ectx))
              ([], fctx) ex_list
          in

          make_val_list expr_list ctx >>= fun (values, r_ctx) ->
          return
            {
              ctx with
              last_expr_result =
                VArray
                  (Arr
                     {
                       a_type;
                       values;
                       number = r_ctx.obj_created_cnt + 1;
                       length = List.length values;
                     });
              obj_created_cnt = r_ctx.obj_created_cnt + 1;
            }
      | ClassCreate (Name class_name, c_args) ->
          (* В проверке типов наличие уже проверялось наличие *)
          get_elem_if_present_m class_table class_name >>= fun obj_class ->
          if obj_class.is_abstract then
            error "This class is abstract! No object creation allowed"
          else
            (* Проверяем конструктор, чтобы был *)
            check_constructor obj_class c_args ctx class_table
            >>= fun constr_r ->
            let new_field_table : (key_t, field_ref) Hashtbl_p.t =
              Hashtbl.create 1020
            in
            (* В контексте этой функции должен быть пустой объект с пустой таблицей *)
            let rec init_object cl_r init_ctx =
              let field_tuples = get_var_field_pairs_list_typed cl_r.dec_tree in
              let rec helper_init acc_ht help_ctx = function
                | [] -> return help_ctx
                | (curr_f_type, Name f_name, f_expr_o) :: tps ->
                    let is_not_mutable_field f_key =
                      get_elem_if_present_m obj_class.field_table f_key
                      >>= fun test_field -> return test_field.is_not_mutable
                    in
                    ( match f_expr_o with
                    (* Сверяем, если есть выражение - сверяем типы, вычисляем, если нет, берем значение по умолчанию *)
                    | Some f_expr -> (
                        expr_type_check f_expr help_ctx class_table
                        >>= fun expr_type ->
                        is_not_mutable_field f_name >>= fun is_not_mutable ->
                        let add_field fe =
                          eval_expr fe help_ctx class_table >>= fun fe_ctx ->
                          Hashtbl.add acc_ht f_name
                            {
                              key = f_name;
                              f_type = curr_f_type;
                              f_value = fe_ctx.last_expr_result;
                              is_not_mutable;
                              assignment_count = 1;
                            };
                          return (fe_ctx, acc_ht)
                        in
                        match expr_type with
                        | ClassName "null" -> (
                            match curr_f_type with
                            | ClassName _ -> add_field f_expr
                            | _ ->
                                error "Wrong assign type in field declaration" )
                        | ClassName cright -> (
                            match curr_f_type with
                            | ClassName cleft ->
                                check_classname_assign cleft cright class_table
                                >>= fun _ -> add_field f_expr
                            | _ ->
                                error "Wrong assign type in field declaration" )
                        | Array (ClassName cright) -> (
                            match curr_f_type with
                            | Array (ClassName cleft) ->
                                check_classname_assign cleft cright class_table
                                >>= fun _ -> add_field f_expr
                            | _ -> error "Wrong assign type in declaration" )
                        | _ when expr_type = curr_f_type -> add_field f_expr
                        | _ -> error "Wrong assign type in declaration" )
                    | None ->
                        is_not_mutable_field f_name >>= fun is_not_mutable ->
                        Hashtbl.add acc_ht f_name
                          {
                            key = f_name;
                            f_type = curr_f_type;
                            f_value = get_init_value_of_type curr_f_type;
                            is_not_mutable;
                            assignment_count = 0;
                          };
                        return (help_ctx, acc_ht) )
                    >>= fun (head_ctx, head_ht) ->
                    (* Обрабатывать хвост идем уже с контекстом, в котором лежит обновленный объект *)
                    obj_num head_ctx.cur_object >>= fun num ->
                    helper_init head_ht
                      {
                        head_ctx with
                        cur_object =
                          RObj
                            {
                              class_key = class_name;
                              field_ref_table = head_ht;
                              number = num;
                            };
                      }
                      tps
              in
              (* Просто пройтись по полям текущего class_dec - недостаточно, надо пройтись и по иерархии родителей *)
              match cl_r.parent_key with
              | None -> helper_init (Hashtbl.create 100) init_ctx field_tuples
              | Some par_key ->
                  get_elem_if_present_m class_table par_key >>= fun parent_r ->
                  init_object parent_r init_ctx >>= fun par_ctx ->
                  helper_init
                    (get_obj_fields_exn par_ctx.cur_object)
                    par_ctx field_tuples
              (* Инициализация переменных, которая происходит до конструктора - в пустом контексте *)
            in
            let new_object =
              RObj
                {
                  class_key = class_name;
                  field_ref_table = Hashtbl.create 100;
                  number = ctx.obj_created_cnt + 1;
                }
            in
            init_object obj_class
              {
                cur_object = new_object;
                var_table = Hashtbl.create 100;
                last_expr_result = VVoid;
                runtime_signal = NoSignal;
                curr_method_type = Void;
                is_main_scope = false;
                nested_loops_cnt = 0;
                scope_level = 0;
                prev_context = Some ctx;
                obj_created_cnt = ctx.obj_created_cnt + 1;
                cur_constr_key = None;
                is_creation = false;
                constr_affilation = None;
              }
            (* Внутри initres_ctx лежит объект с проинициализированными полями, готовый к обработке конструктором *)
            >>=
            fun initres_ctx ->
            (* Контекст, в котором должна готовиться таблица аргументов - текущий!!!!*)
            let get_new_var_table =
              try
                prepare_table_with_args_exn (Hashtbl.create 100) c_args
                  constr_r.args ctx class_table
              with Invalid_argument m -> error m
            in
            get_new_var_table >>= fun (vt, _) ->
            prepare_constructor_block constr_r.body obj_class >>= fun c_body ->
            (* Контекст, в котором исполняется блок - получившийся объект + таблица переменных - аргументы конструктора *)
            eval_stmt c_body
              {
                initres_ctx with
                var_table = vt;
                is_creation = true;
                is_main_scope = false;
                constr_affilation = Some obj_class.this_key;
                cur_constr_key = Some constr_r.key;
              }
              class_table
            >>= fun c_ctx ->
            (* В итоге возвращем тот же контекст, что и был, с получившимся объектом после исполнения конструктора *)
            return
              {
                ctx with
                last_expr_result = VObjectRef c_ctx.cur_object;
                runtime_signal = NoSignal;
                obj_created_cnt = c_ctx.obj_created_cnt;
              }
      | Assign (Identifier var_key, val_expr) ->
          eval_expr val_expr ctx class_table >>= fun val_evaled_ctx ->
          update_identifier_v var_key val_evaled_ctx.last_expr_result
            val_evaled_ctx
      | Assign (FieldAccess (obj_expr, Identifier f_name), val_expr) ->
          eval_expr val_expr ctx class_table >>= fun val_evaled_ctx ->
          update_field_v obj_expr f_name val_evaled_ctx class_table
      | Assign (ArrayAccess (arr_expr, index_expr), val_expr) -> (
          eval_expr val_expr ctx class_table >>= fun val_evaled_ctx ->
          eval_expr arr_expr val_evaled_ctx class_table
          >>= fun arr_evaled_ctx ->
          eval_expr index_expr arr_evaled_ctx class_table
          >>= fun index_evaled_ctx ->
          match arr_evaled_ctx.last_expr_result with
          | VArray arr -> (
              match index_evaled_ctx.last_expr_result with
              | VInt i -> (
                  let new_val = val_evaled_ctx.last_expr_result in
                  try
                    if index_evaled_ctx.is_creation then (
                      let arr_n =
                        get_arr_info_exn arr |> fun (_, _, _, n) -> n
                      in
                      let cur_frt =
                        get_obj_fields_exn index_evaled_ctx.cur_object
                      in
                      Hashtbl.iter
                        (fun k v ->
                          match v.v_value with
                          | VArray (Arr { number = n; _ }) when n = arr_n ->
                              Hashtbl.replace index_evaled_ctx.var_table k
                                {
                                  v with
                                  v_value =
                                    update_array_val_exn v.v_value i new_val;
                                }
                          | _ -> ())
                        index_evaled_ctx.var_table;
                      Hashtbl.iter
                        (fun k f ->
                          match f.f_value with
                          | VArray (Arr { number = n; _ }) when n = arr_n ->
                              Hashtbl.replace cur_frt k
                                {
                                  f with
                                  f_value =
                                    update_array_val_exn f.f_value i new_val;
                                }
                          | _ -> ())
                        cur_frt;
                      return
                        { index_evaled_ctx with last_expr_result = new_val } )
                    else
                      update_array_state_exn arr i new_val index_evaled_ctx
                      |> fun _ ->
                      return
                        { index_evaled_ctx with last_expr_result = new_val }
                  with Invalid_argument m | Failure m -> error m )
              | _ -> error "Wrong type for array index!" )
          | _ -> error "Wrong type for array asssignment!" )
      | PostInc (FieldAccess (obj_expr, Identifier f))
      | PrefInc (FieldAccess (obj_expr, Identifier f)) ->
          eval_expr
            (Assign
               ( FieldAccess (obj_expr, Identifier f),
                 Add (FieldAccess (obj_expr, Identifier f), Const (VInt 1)) ))
            ctx class_table
      | PostInc (Identifier var_key) | PrefInc (Identifier var_key) ->
          eval_expr
            (Assign
               (Identifier var_key, Add (Identifier var_key, Const (VInt 1))))
            ctx class_table
      | PostInc (ArrayAccess (arr_expr, index_expr))
      | PrefInc (ArrayAccess (arr_expr, index_expr)) ->
          eval_expr
            (Assign
               ( ArrayAccess (arr_expr, index_expr),
                 Add (ArrayAccess (arr_expr, index_expr), Const (VInt 1)) ))
            ctx class_table
      | PostDec (FieldAccess (obj_expr, Identifier f))
      | PrefDec (FieldAccess (obj_expr, Identifier f)) ->
          eval_expr
            (Assign
               ( FieldAccess (obj_expr, Identifier f),
                 Sub (FieldAccess (obj_expr, Identifier f), Const (VInt 1)) ))
            ctx class_table
      | PostDec (Identifier var_key) | PrefDec (Identifier var_key) ->
          eval_expr
            (Assign
               (Identifier var_key, Sub (Identifier var_key, Const (VInt 1))))
            ctx class_table
      | PostDec (ArrayAccess (arr_expr, index_expr))
      | PrefDec (ArrayAccess (arr_expr, index_expr)) ->
          eval_expr
            (Assign
               ( ArrayAccess (arr_expr, index_expr),
                 Sub (ArrayAccess (arr_expr, index_expr), Const (VInt 1)) ))
            ctx class_table
      | _ -> error "Wrong expression construction!"
    in
    expr_type_check expr ectx class_table >>= fun _ -> eval_e expr ectx

  and get_old_arr arr_n g_ctx =
    match get_elem_if_present g_ctx.var_table arr_n with
    | Some v -> return v.v_value
    | None -> (
        try
          let curr_obj_fields = get_obj_fields_exn g_ctx.cur_object in
          match get_elem_if_present curr_obj_fields arr_n with
          | Some field -> return field.f_value
          | None -> error "No such array field or variable"
        with Invalid_argument m -> error m )

  and get_index v =
    match v with VInt x -> return x | _ -> error "Wrong value type!"

  and update_identifier_v var_key new_val val_evaled_ctx =
    if Hashtbl.mem val_evaled_ctx.var_table var_key then (
      get_elem_if_present_m val_evaled_ctx.var_table var_key >>= fun old_var ->
      check_assign_cnt_v old_var >>= fun _ ->
      Hashtbl.replace val_evaled_ctx.var_table var_key
        {
          old_var with
          v_value = new_val;
          assignment_count = old_var.assignment_count + 1;
        };
      return val_evaled_ctx
      (* Если не получилось найти среди переменных - ищем среди полей текущего объекта *)
      )
    else
      match val_evaled_ctx.cur_object with
      | RNull -> error "NullPointerException"
      | RObj { field_ref_table = cur_frt; _ } ->
          if Hashtbl.mem cur_frt var_key then
            (* Мы обновляем состояние объекта во всей системе + помним про final*)
            get_elem_if_present_m cur_frt var_key >>= fun old_field ->
            check_assign_cnt_f old_field >>= fun _ ->
            if val_evaled_ctx.is_creation then
              Hashtbl.replace cur_frt var_key
                { old_field with f_value = val_evaled_ctx.last_expr_result }
              |> fun _ -> return val_evaled_ctx
            else
              try
                update_object_state_exn val_evaled_ctx.cur_object var_key
                  val_evaled_ctx.last_expr_result val_evaled_ctx
                |> fun _ -> return val_evaled_ctx
              with
              | Invalid_argument m -> error m
              | Not_found -> error "No such field"
          else error "No such variable"

  and update_field_v obj_expr f_name val_evaled_ctx class_table =
    eval_expr obj_expr val_evaled_ctx class_table >>= fun obj_evaled_ctx ->
    ( match obj_evaled_ctx.last_expr_result with
    | VObjectRef _ -> return ()
    | _ -> error "Wrong object value" )
    >>= fun _ ->
    let obj_r = get_obj_value obj_evaled_ctx.last_expr_result in
    let new_val = val_evaled_ctx.last_expr_result in
    try
      get_obj_info_exn obj_r |> fun (_, frt, _) ->
      if Hashtbl.mem frt f_name then
        get_elem_if_present_m frt f_name >>= fun old_field ->
        check_assign_cnt_f old_field >>= fun _ ->
        if obj_evaled_ctx.is_creation then
          Hashtbl.replace frt f_name
            { old_field with f_value = val_evaled_ctx.last_expr_result }
          |> fun _ -> return obj_evaled_ctx
        else
          update_object_state_exn obj_r f_name new_val obj_evaled_ctx
          |> fun _ -> return obj_evaled_ctx
      else error "No such field in class"
    with
    | Invalid_argument m | Failure m -> error m
    | Not_found -> error "No such field"

  and update_array_state_exn arr index new_value update_ctx =
    let rec update_states f_ht i n_val a_n =
      Hashtbl.iter
        (fun _ field_ref ->
          (* Перебираем все поля из таблицы *)
          match field_ref with
          | { key = f_key; f_value = f_val; _ } -> (
              match f_val with
              (* Если массив - смотрим, если номер совпал - обновляем элемент по индексу.
                 + смотрим, если у нас массив объектов - то надо по нему тоже пробежаться, у каждого объекта сделать рекурсивный запуск обхода *)
              | VArray
                  (Arr
                    {
                      a_type = at;
                      values = cur_values;
                      number = cur_num;
                      length = cur_len;
                    }) -> (
                  if cur_num = a_n then
                    (* Надо в текущей таблице заменить массив на новый (заменить поле) *)
                    Hashtbl.replace f_ht f_key
                      {
                        field_ref with
                        f_value =
                          update_array_val_exn
                            (VArray
                               (Arr
                                  {
                                    a_type = at;
                                    values = cur_values;
                                    number = cur_num;
                                    length = cur_len;
                                  }))
                            i n_val;
                      };
                  match at with
                  | ClassName _ ->
                      List.iter
                        (fun v ->
                          match v with
                          | VObjectRef (RObj { field_ref_table = frt; _ }) ->
                              update_states frt i n_val a_n
                          | _ -> ())
                        cur_values
                  | _ -> () )
              (* Если не-null объект - рекурсивный запуск по его таблице полей  *)
              | VObjectRef (RObj { field_ref_table = frt; _ }) ->
                  update_states frt i n_val a_n
              | _ -> () ))
        f_ht
    in

    let rec helper_update i n_val u_ctx a_n =
      Hashtbl.iter
        (fun v_key var ->
          match var.v_value with
          | VObjectRef (RObj { field_ref_table = frt; _ }) ->
              update_states frt i n_val a_n
          | VArray
              (Arr
                {
                  a_type = at;
                  values = cur_values;
                  number = cur_num;
                  length = cur_len;
                }) -> (
              if cur_num = a_n then
                (* Надо в текущей таблице заменить массив на новый (заменить значение переменной) *)
                Hashtbl.replace u_ctx.var_table v_key
                  {
                    var with
                    v_value =
                      update_array_val_exn
                        (VArray
                           (Arr
                              {
                                a_type = at;
                                values = cur_values;
                                number = cur_num;
                                length = cur_len;
                              }))
                        i n_val;
                  }
                |> fun () ->
                match at with
                | ClassName _ ->
                    List.iter
                      (fun v ->
                        match v with
                        | VObjectRef (RObj { field_ref_table = frt; _ }) ->
                            update_states frt i n_val a_n
                        | _ -> ())
                      cur_values
                | _ -> () )
          | _ -> ())
        u_ctx.var_table
      |> fun () ->
      match u_ctx.prev_context with
      | None -> ()
      | Some prev_ctx -> helper_update i n_val prev_ctx a_n
    in
    get_arr_info_exn arr |> fun (_, _, _, a_number) ->
    helper_update index new_value update_ctx a_number

  and update_object_state_exn obj field_key new_value update_ctx =
    let rec update_states f_ht f_key n_val o_num assign_cnt =
      (* Перебираем все поля из таблицы *)
      Hashtbl.iter
        (fun _ field_ref ->
          match field_ref with
          | { f_value = f_val; _ } -> (
              match f_val with
              (* Если значение поля - какой-то не-null объект, то... *)
              | VObjectRef (RObj { field_ref_table = frt; number = fnum; _ }) ->
                  (* Смотрим, если номера совпадают - обновляем поле. В любом случае делаем запуск по полям этого объекта *)
                  ( if fnum = o_num then
                    match get_elem_if_present frt f_key with
                    | None -> raise Not_found
                    | Some old_field ->
                        Hashtbl.replace frt f_key
                          {
                            old_field with
                            f_value = n_val;
                            assignment_count = assign_cnt;
                          } );
                  update_states frt f_key n_val o_num assign_cnt
              (* Массив объектов - бежим по нему, если объект - бежим по его полям. При совпадении номера еще и обновляем *)
              | VArray (Arr { a_type = ClassName _; values = v_list; _ }) ->
                  List.iter
                    (fun v ->
                      match v with
                      | VObjectRef
                          (RObj { field_ref_table = frt; number = c_num; _ }) ->
                          ( if c_num = o_num then
                            match get_elem_if_present frt f_key with
                            | None -> raise Not_found
                            | Some old_field ->
                                Hashtbl.replace frt f_key
                                  {
                                    old_field with
                                    f_value = n_val;
                                    assignment_count = assign_cnt;
                                  } );
                          update_states frt f_key n_val o_num assign_cnt
                      | _ -> ())
                    v_list
              (* Иначе пропуск, идем дальше *)
              | _ -> () ))
        f_ht
    in
    let rec helper_update f_key n_val u_ctx o_num assign_cnt =
      (* Пробегаемся по переменным контекста как по вершинам дерева, если объект с нужным номером - обновляем состояние и запускает рекурсивный алгоритм обновления *)
      Hashtbl.iter
        (fun _ var ->
          match var.v_value with
          (*  Если значение - какой-то объект *)
          | VObjectRef (RObj { field_ref_table = frt; number = fnum; _ }) ->
              if o_num = fnum then (
                match get_elem_if_present frt f_key with
                | None -> raise Not_found
                | Some old_field ->
                    Hashtbl.replace frt f_key
                      {
                        old_field with
                        f_value = n_val;
                        assignment_count = assign_cnt;
                      };
                    update_states frt f_key n_val o_num assign_cnt
                    (* Номер не совпал - запускаем рекурсивный обход полей *)
                )
              else update_states frt f_key n_val o_num assign_cnt
          (* Массив объектов - пробегаемся по элементам, видим объект - запускаем обход по его таблице полей с обновлением при совпадении номера *)
          | VArray (Arr { a_type = ClassName _; values = v_list; _ }) ->
              List.iter
                (fun v ->
                  match v with
                  | VObjectRef
                      (RObj { field_ref_table = frt; number = c_num; _ }) ->
                      ( if c_num = o_num then
                        match get_elem_if_present frt f_key with
                        | None -> raise Not_found
                        | Some old_field ->
                            Hashtbl.replace frt f_key
                              {
                                old_field with
                                f_value = n_val;
                                assignment_count = assign_cnt;
                              } )
                      |> fun () ->
                      update_states frt f_key n_val o_num assign_cnt
                  | _ -> ())
                v_list
          | _ -> ())
        u_ctx.var_table
      |> fun () ->
      match u_ctx.prev_context with
      | None -> ()
      | Some prev_ctx -> helper_update f_key n_val prev_ctx o_num assign_cnt
    in
    get_obj_info_exn obj |> fun (_, object_frt, object_number) ->
    ( match get_elem_if_present object_frt field_key with
    | None -> raise Not_found
    | Some f -> f.assignment_count + 1 )
    |> fun assign_cnt ->
    helper_update field_key new_value update_ctx object_number assign_cnt

  and prepare_table_with_args_exn ht args_l m_arg_list pr_ctx class_table =
    (* Одновременно бежим по двум спискам: списку выражений, переданных в метод и списку параметров в записи метода у класса.
           Гарантируется из предыдущих проверок, что длина списков будет одинакова *)
    mfold_left2
      (fun (h_ht, hctx) arg tn_pair ->
        match tn_pair with
        | head_type, Name head_name ->
            eval_expr arg hctx class_table >>= fun he_ctx ->
            Hashtbl.add h_ht head_name
              {
                v_type = head_type;
                v_key = head_name;
                is_not_mutable = false;
                assignment_count = 1;
                v_value = he_ctx.last_expr_result;
                scope_level = 0;
              };
            return (h_ht, he_ctx))
      (ht, pr_ctx) args_l m_arg_list

  and prepare_constructor_block curr_body curr_class_r =
    match (curr_body, curr_class_r.parent_key) with
    | StmtBlock (Expression (CallMethod (Super, _)) :: _), None ->
        error "Super call in constructor on not child class"
    | StmtBlock (Expression (CallMethod (Super, _)) :: _), Some _ ->
        return curr_body
    (* Если увидели, что начало не на супер - то надо просмотреть, есть ли конструкторы у родителя *)
    (* Родителя нет - тогда просто возвращаем тот-же конструктор *)
    | StmtBlock _, None -> return curr_body
    (* Есть родитель - делаем вставку super в начало, если нет в начале вызова this(...) *)
    | StmtBlock other, Some _ -> (
        match curr_body with
        | StmtBlock (Expression (CallMethod (This, _)) :: _) -> return curr_body
        | _ -> return (StmtBlock (Expression (CallMethod (Super, [])) :: other))
        )
    | _ -> error "Must be statement block!"

  let execute : (key_t, class_r) Hashtbl.t -> context M.t =
   fun ht ->
    find_class_with_main ht >>= fun cl ->
    make_context
      (RObj
         {
           class_key = cl.this_key;
           field_ref_table = Hashtbl.create 10;
           number = 0;
         })
      (Hashtbl.create 10)
    >>= fun ctx ->
    let main = Hashtbl.find cl.method_table "main@@" in
    match main.body with
    | None -> error "main() method cannot be abstract!"
    | Some body_main -> eval_stmt body_main ctx ht
end
