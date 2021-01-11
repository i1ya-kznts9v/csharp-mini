  $ (cd ../../../../default && demos/demoInterpreter.exe) 
  -_-_-_-_-_-_-_-_-_-_- Assign test -_-_-_-_-_-_-_-_-_-_-
  
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["a" ->
       { var_key = "a"; var_type = Int; var_value = VInt (1); is_const = false;
         assignment_count = 1; visibility_level = 0 }
    
  "b" ->
   { var_key = "b"; var_type = Int; var_value = VInt (2); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "c" ->
   { var_key = "c"; var_type = Int; var_value = VInt (3); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  ]]
  ; current_meth_type = Void; last_expr_result = VInt (3);
  runtime_flag = NoFlag; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  -_-_-_-_-_-_-_-_-_-_- Arithmetic test -_-_-_-_-_-_-_-_-_-_-
  
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["a" ->
       { var_key = "a"; var_type = Int; var_value = VInt (2); is_const = false;
         assignment_count = 2; visibility_level = 0 }
    
  "val7" ->
   { var_key = "val7"; var_type = Int; var_value = VInt (124);
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "b" ->
   { var_key = "b"; var_type = Int; var_value = VInt (2); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "s5" ->
   { var_key = "s5"; var_type = String; var_value = VString ("2b");
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "val2" ->
   { var_key = "val2"; var_type = Int; var_value = VInt (3); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "s1" ->
   { var_key = "s1"; var_type = String; var_value = VString ("a");
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "s3" ->
   { var_key = "s3"; var_type = String; var_value = VString ("ab");
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "val1" ->
   { var_key = "val1"; var_type = Int; var_value = VInt (15); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "s4" ->
   { var_key = "s4"; var_type = String; var_value = VString ("a2");
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "val3" ->
   { var_key = "val3"; var_type = Int; var_value = VInt (101);
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "val4" ->
   { var_key = "val4"; var_type = Int; var_value = VInt (5); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "s2" ->
   { var_key = "s2"; var_type = String; var_value = VString ("b");
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "val5" ->
   { var_key = "val5"; var_type = Int; var_value = VInt (0); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "val6" ->
   { var_key = "val6"; var_type = Int; var_value = VInt (300);
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "c" ->
   { var_key = "c"; var_type = Int; var_value = VInt (3); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  ]]
  ; current_meth_type = Void; last_expr_result = VString ("2b");
  runtime_flag = NoFlag; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  -_-_-_-_-_-_-_-_-_-_- Bool expression test -_-_-_-_-_-_-_-_-_-_-
  
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["a" ->
       { var_key = "a"; var_type = Int; var_value = VInt (10);
         is_const = false; assignment_count = 1; visibility_level = 0 }
    
  "b" ->
   { var_key = "b"; var_type = Int; var_value = VInt (50); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "objNEq" ->
   { var_key = "objNEq"; var_type = Int; var_value = VInt (1);
     is_const = false; assignment_count = 2; visibility_level = 0 }
  
  "eVal" ->
   { var_key = "eVal"; var_type = Int; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "p3" ->
   { var_key = "p3"; var_type = CsClass ("Person");
     var_value =
     VClass (ObjRef ({ class_key = "Person"; parent_key = None;
                       class_table =
                       [["age" ->
                          { key = "age"; f_type = Int; f_value = VInt (20);
                            is_const = false; assignment_count = 0 }
                       
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Bob");
     is_const = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); is_const = false; assignment_count = 1;
  visibility_level = 0
  }
  
  "lVal" ->
   { var_key = "lVal"; var_type = Int; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "sEq" ->
   { var_key = "sEq"; var_type = Int; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "sNEQ" ->
   { var_key = "sNEQ"; var_type = Int; var_value = VInt (1); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "neVal" ->
   { var_key = "neVal"; var_type = Int; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "s1" ->
   { var_key = "s1"; var_type = String; var_value = VString ("a");
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "d" ->
   { var_key = "d"; var_type = Int; var_value = VInt (10); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "meVal" ->
   { var_key = "meVal"; var_type = Int; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "p2" ->
   { var_key = "p2"; var_type = CsClass ("Person");
     var_value =
     VClass (ObjRef ({ class_key = "Person"; parent_key = None;
                       class_table =
                       [["age" ->
                          { key = "age"; f_type = Int; f_value = VInt (30);
                            is_const = false; assignment_count = 0 }
                       
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Alice");
     is_const = false; assignment_count = 0 }
  
  ]]
  ; number = 2 })); is_const = false; assignment_count = 1;
  visibility_level = 0
  }
  
  "p1" ->
   { var_key = "p1"; var_type = CsClass ("Person");
     var_value =
     VClass (ObjRef ({ class_key = "Person"; parent_key = None;
                       class_table =
                       [["age" ->
                          { key = "age"; f_type = Int; f_value = VInt (20);
                            is_const = false; assignment_count = 0 }
                       
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Bob");
     is_const = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); is_const = false; assignment_count = 1;
  visibility_level = 0
  }
  
  "s2" ->
   { var_key = "s2"; var_type = String; var_value = VString ("b");
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "notVal" ->
   { var_key = "notVal"; var_type = Int; var_value = VInt (1);
     is_const = false; assignment_count = 2; visibility_level = 0 }
  
  "objEq" ->
   { var_key = "objEq"; var_type = Int; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "leVal" ->
   { var_key = "leVal"; var_type = Int; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "mVal" ->
   { var_key = "mVal"; var_type = Int; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "orVal" ->
   { var_key = "orVal"; var_type = Int; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "c" ->
   { var_key = "c"; var_type = Int; var_value = VInt (100); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "andVal" ->
   { var_key = "andVal"; var_type = Int; var_value = VInt (1);
     is_const = false; assignment_count = 2; visibility_level = 0 }
  
  ]]
  ; current_meth_type = Void; last_expr_result = VInt (1);
  runtime_flag = NoFlag; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 2; is_creation = false
  }
  
  -_-_-_-_-_-_-_-_-_-_- Static method call test -_-_-_-_-_-_-_-_-_-_-
  
  Hello
  GoodBye
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table = [[]]
    ; current_meth_type = Void; last_expr_result = VVoid;
    runtime_flag = NoFlag; is_main = true; curr_constructor = None;
    count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
    count_of_obj = 0; is_creation = false }
  
  -_-_-_-_-_-_-_-_-_-_- Class method call test -_-_-_-_-_-_-_-_-_-_-
  
  0
  Tom34
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["tom" ->
       { var_key = "tom"; var_type = CsClass ("Person");
         var_value =
         VClass (ObjRef ({ class_key = "Person"; parent_key = None;
                           class_table =
                           [["age" ->
                              { key = "age"; f_type = Int; f_value = VInt (34);
                                is_const = false; assignment_count = 1 }
                           
  "name" ->
   { key = "name"; f_type = String; f_value = VString ("Tom");
     is_const = false; assignment_count = 1 }
  
  ]]
  ; number = 1 })); is_const = false; assignment_count = 1;
  visibility_level = 0
  }
  
  ]]
  ; current_meth_type = Void; last_expr_result = VVoid; runtime_flag = NoFlag;
  is_main = true; curr_constructor = None; count_of_nested_cycle = 0;
  visibility_level = 0; prev_ctx = None; count_of_obj = 1; is_creation = false
  }
  
  -_-_-_-_-_-_-_-_-_-_- Update object state -_-_-_-_-_-_-_-_-_-_-
  
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["p3" ->
       { var_key = "p3"; var_type = CsClass ("Person");
         var_value =
         VClass (ObjRef ({ class_key = "Person"; parent_key = None;
                           class_table =
                           [["age" ->
                              { key = "age"; f_type = Int; f_value = VInt (55);
                                is_const = false; assignment_count = 1 }
                           
  "name" ->
   { key = "name"; f_type = String; f_value = VString (""); is_const = false;
     assignment_count = 0 }
  
  ]]
  ; number = 1 })); is_const = false; assignment_count = 1;
  visibility_level = 0
  }
  
  "p2" ->
   { var_key = "p2"; var_type = CsClass ("Person");
     var_value =
     VClass (ObjRef ({ class_key = "Person"; parent_key = None;
                       class_table =
                       [["age" ->
                          { key = "age"; f_type = Int; f_value = VInt (55);
                            is_const = false; assignment_count = 1 }
                       
  "name" ->
   { key = "name"; f_type = String; f_value = VString (""); is_const = false;
     assignment_count = 0 }
  
  ]]
  ; number = 1 })); is_const = false; assignment_count = 1;
  visibility_level = 0
  }
  
  "res" ->
   { var_key = "res"; var_type = Int; var_value = VInt (55); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "p1" ->
   { var_key = "p1"; var_type = CsClass ("Person");
     var_value =
     VClass (ObjRef ({ class_key = "Person"; parent_key = None;
                       class_table =
                       [["age" ->
                          { key = "age"; f_type = Int; f_value = VInt (55);
                            is_const = false; assignment_count = 1 }
                       
  "name" ->
   { key = "name"; f_type = String; f_value = VString (""); is_const = false;
     assignment_count = 0 }
  
  ]]
  ; number = 1 })); is_const = false; assignment_count = 1;
  visibility_level = 0
  }
  
  "person" ->
   { var_key = "person"; var_type = CsClass ("Person");
     var_value =
     VClass (ObjRef ({ class_key = "Person"; parent_key = None;
                       class_table =
                       [["age" ->
                          { key = "age"; f_type = Int; f_value = VInt (55);
                            is_const = false; assignment_count = 1 }
                       
  "name" ->
   { key = "name"; f_type = String; f_value = VString (""); is_const = false;
     assignment_count = 0 }
  
  ]]
  ; number = 1 })); is_const = false; assignment_count = 1;
  visibility_level = 0
  }
  
  ]]
  ; current_meth_type = Void; last_expr_result = VInt (55);
  runtime_flag = NoFlag; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 1; is_creation = false
  }
  
  -_-_-_-_-_-_-_-_-_-_- Visibility level test -_-_-_-_-_-_-_-_-_-_-
  
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["a" ->
       { var_key = "a"; var_type = Int; var_value = VInt (1000);
         is_const = false; assignment_count = 5; visibility_level = 0 }
    
  "b" ->
   { var_key = "b"; var_type = Int; var_value = VInt (2000); is_const = false;
     assignment_count = 4; visibility_level = 0 }
  
  "c" ->
   { var_key = "c"; var_type = Int; var_value = VInt (3000); is_const = false;
     assignment_count = 4; visibility_level = 0 }
  
  "i" ->
   { var_key = "i"; var_type = Int; var_value = VInt (3); is_const = false;
     assignment_count = 4; visibility_level = 0 }
  
  ]]
  ; current_meth_type = Void; last_expr_result = VInt (3000);
  runtime_flag = NoFlag; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 1; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  -_-_-_-_-_-_-_-_-_-_- Cycles test -_-_-_-_-_-_-_-_-_-_-
  
  0
  0
  0
  1
  0
  2
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["i" ->
       { var_key = "i"; var_type = Int; var_value = VInt (3); is_const = false;
         assignment_count = 4; visibility_level = 1 }
    
  ]]
  ; current_meth_type = Void; last_expr_result = VBool (false);
  runtime_flag = NoFlag; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 2; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  -_-_-_-_-_-_-_-_-_-_- Break test -_-_-_-_-_-_-_-_-_-_-
  
  0
  1
  2
  3
  4
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table = [[]]
    ; current_meth_type = Void; last_expr_result = VInt (6);
    runtime_flag = NoFlag; is_main = true; curr_constructor = None;
    count_of_nested_cycle = 0; visibility_level = 1; prev_ctx = None;
    count_of_obj = 0; is_creation = false }
  
  -_-_-_-_-_-_-_-_-_-_- Continue test -_-_-_-_-_-_-_-_-_-_-
  
  0
  1
  2
  3
  4
  6
  7
  8
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table = [[]]
    ; current_meth_type = Void; last_expr_result = VBool (false);
    runtime_flag = NoFlag; is_main = true; curr_constructor = None;
    count_of_nested_cycle = 0; visibility_level = 1; prev_ctx = None;
    count_of_obj = 0; is_creation = false }
  
  -_-_-_-_-_-_-_-_-_-_- While test -_-_-_-_-_-_-_-_-_-_-
  
  6
  5
  4
  3
  2
  1
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["i" ->
       { var_key = "i"; var_type = Int; var_value = VInt (0); is_const = false;
         assignment_count = 7; visibility_level = 0 }
    
  ]]
  ; current_meth_type = Void; last_expr_result = VBool (false);
  runtime_flag = NoFlag; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  -_-_-_-_-_-_-_-_-_-_- If test -_-_-_-_-_-_-_-_-_-_-
  
  num1
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["num2" ->
       { var_key = "num2"; var_type = Int; var_value = VInt (6);
         is_const = false; assignment_count = 1; visibility_level = 0 }
    
  "num1" ->
   { var_key = "num1"; var_type = Int; var_value = VInt (8); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  ]]
  ; current_meth_type = Void; last_expr_result = VString ("num1");
  runtime_flag = NoFlag; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  -_-_-_-_-_-_-_-_-_-_- If else if test -_-_-_-_-_-_-_-_-_-_-
  
  3
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["num2" ->
       { var_key = "num2"; var_type = Int; var_value = VInt (6);
         is_const = false; assignment_count = 1; visibility_level = 0 }
    
  "num1" ->
   { var_key = "num1"; var_type = Int; var_value = VInt (6); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  ]]
  ; current_meth_type = Void; last_expr_result = VInt (3);
  runtime_flag = NoFlag; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  -_-_-_-_-_-_-_-_-_-_- Recursion test FACTORIAL -_-_-_-_-_-_-_-_-_-_-
  
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["res" ->
       { var_key = "res"; var_type = Int; var_value = VInt (120);
         is_const = false; assignment_count = 1; visibility_level = 0 }
    
  ]]
  ; current_meth_type = Void; last_expr_result = VInt (120);
  runtime_flag = NoFlag; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  -_-_-_-_-_-_-_-_-_-_- Recursion test Fibonachi -_-_-_-_-_-_-_-_-_-_-
  
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["res" ->
       { var_key = "res"; var_type = Int; var_value = VInt (2584);
         is_const = false; assignment_count = 1; visibility_level = 0 }
    
  ]]
  ; current_meth_type = Void; last_expr_result = VInt (2584);
  runtime_flag = NoFlag; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  -_-_-_-_-_-_-_-_-_-_- Const fields test -_-_-_-_-_-_-_-_-_-_-
  
  Assigment to a constant field
  -_-_-_-_-_-_-_-_-_-_- Const variables test -_-_-_-_-_-_-_-_-_-_-
  
  Assigment to a constant variable
  -_-_-_-_-_-_-_-_-_-_- Overloading test -_-_-_-_-_-_-_-_-_-_-
  
  Hello
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["res" ->
       { var_key = "res"; var_type = String; var_value = VString ("Hello");
         is_const = false; assignment_count = 1; visibility_level = 0 }
    
  "ex" ->
   { var_key = "ex"; var_type = CsClass ("ShittyExn");
     var_value =
     VClass (ObjRef ({ class_key = "ShittyExn";
                       parent_key = Some ("Exception");
                       class_table =
                       [["message" ->
                          { key = "message"; f_type = String;
                            f_value = VString (""); is_const = false;
                            assignment_count = 0 }
                       
  ]]
  ; number = 1 })); is_const = false; assignment_count = 1;
  visibility_level = 0
  }
  
  ]]
  ; current_meth_type = Void; last_expr_result = VString ("Hello");
  runtime_flag = NoFlag; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 1; is_creation = false
  }
  
  -_-_-_-_-_-_-_-_-_-_- Print test -_-_-_-_-_-_-_-_-_-_-
  
  true
  3
  Hello World
  ShittyExn
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["a" ->
       { var_key = "a"; var_type = CsClass ("ShittyExn");
         var_value =
         VClass (ObjRef ({ class_key = "ShittyExn";
                           parent_key = Some ("Exception");
                           class_table =
                           [["message" ->
                              { key = "message"; f_type = String;
                                f_value = VString (""); is_const = false;
                                assignment_count = 0 }
                           
  ]]
  ; number = 1 })); is_const = false; assignment_count = 1;
  visibility_level = 0
  }
  
  "b" ->
   { var_key = "b"; var_type = Int; var_value = VInt (3); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  ]]
  ; current_meth_type = Void;
  last_expr_result =
  VClass (ObjRef ({ class_key = "ShittyExn"; parent_key = Some ("Exception");
                    class_table =
                    [["message" ->
                       { key = "message"; f_type = String;
                         f_value = VString (""); is_const = false;
                         assignment_count = 0 }
                    
  ]]
  ; number = 1 })); runtime_flag = NoFlag; is_main = true;
  curr_constructor = None; count_of_nested_cycle = 0; visibility_level = 0;
  prev_ctx = None; count_of_obj = 1; is_creation = false
  }
  
