  $ (cd ../../../../default && demos/interpretationTest.exe)
  -_-_-_-_-_-_-_-_-_-_- "God, let it work!" testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; field_references_table = [[]]
                                              ;
                       number = 0 });
    var_table =
    [["a" ->
       { v_type = TInt; v_key = "a"; is_const = false; assignment_count = 1;
         v_value = VInt (1); scope_level = 0 }
    
  "b" ->
   { v_type = TInt; v_key = "b"; is_const = false; assignment_count = 1;
     v_value = VInt (2); scope_level = 0 }
  
  "c" ->
   { v_type = TInt; v_key = "c"; is_const = false; assignment_count = 1;
     v_value = VInt (3); scope_level = 0 }
  
  ]]
  ; last_expr_result = VInt (3); runtime_signal = NoSignal;
  curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 0; is_creation = false; constr_affilation = None
  }
  
  -_-_-_-_-_-_-_-_-_-_- Arithmetic testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; field_references_table = [[]]
                                              ;
                       number = 0 });
    var_table =
    [["val2" ->
       { v_type = TInt; v_key = "val2"; is_const = false; assignment_count = 1;
         v_value = VInt (3); scope_level = 0 }
    
  "s1" ->
   { v_type = TString; v_key = "s1"; is_const = false; assignment_count = 1;
     v_value = VString ("a"); scope_level = 0 }
  
  "a" ->
   { v_type = TInt; v_key = "a"; is_const = false; assignment_count = 2;
     v_value = VInt (2); scope_level = 0 }
  
  "s3" ->
   { v_type = TString; v_key = "s3"; is_const = false; assignment_count = 1;
     v_value = VString ("ab"); scope_level = 0 }
  
  "val7" ->
   { v_type = TInt; v_key = "val7"; is_const = false; assignment_count = 1;
     v_value = VInt (124); scope_level = 0 }
  
  "val1" ->
   { v_type = TInt; v_key = "val1"; is_const = false; assignment_count = 1;
     v_value = VInt (15); scope_level = 0 }
  
  "s4" ->
   { v_type = TString; v_key = "s4"; is_const = false; assignment_count = 1;
     v_value = VString ("a2"); scope_level = 0 }
  
  "val3" ->
   { v_type = TInt; v_key = "val3"; is_const = false; assignment_count = 1;
     v_value = VInt (101); scope_level = 0 }
  
  "b" ->
   { v_type = TInt; v_key = "b"; is_const = false; assignment_count = 1;
     v_value = VInt (2); scope_level = 0 }
  
  "s5" ->
   { v_type = TString; v_key = "s5"; is_const = false; assignment_count = 1;
     v_value = VString ("2b"); scope_level = 0 }
  
  "val4" ->
   { v_type = TInt; v_key = "val4"; is_const = false; assignment_count = 1;
     v_value = VInt (5); scope_level = 0 }
  
  "s2" ->
   { v_type = TString; v_key = "s2"; is_const = false; assignment_count = 1;
     v_value = VString ("b"); scope_level = 0 }
  
  "val5" ->
   { v_type = TInt; v_key = "val5"; is_const = false; assignment_count = 1;
     v_value = VInt (0); scope_level = 0 }
  
  "val6" ->
   { v_type = TInt; v_key = "val6"; is_const = false; assignment_count = 1;
     v_value = VInt (300); scope_level = 0 }
  
  "c" ->
   { v_type = TInt; v_key = "c"; is_const = false; assignment_count = 1;
     v_value = VInt (3); scope_level = 0 }
  
  ]]
  ; last_expr_result = VString ("2b"); runtime_signal = NoSignal;
  curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 0; is_creation = false; constr_affilation = None
  }
  
  -_-_-_-_-_-_-_-_-_-_- Bool expressions testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; field_references_table = [[]]
                                              ;
                       number = 0 });
    var_table =
    [["s1" ->
       { v_type = TString; v_key = "s1"; is_const = false;
         assignment_count = 1; v_value = VString ("a"); scope_level = 0 }
    
  "a" ->
   { v_type = TInt; v_key = "a"; is_const = false; assignment_count = 1;
     v_value = VInt (10); scope_level = 0 }
  
  "d" ->
   { v_type = TInt; v_key = "d"; is_const = false; assignment_count = 1;
     v_value = VInt (10); scope_level = 0 }
  
  "meVal" ->
   { v_type = TInt; v_key = "meVal"; is_const = false; assignment_count = 2;
     v_value = VInt (1); scope_level = 0 }
  
  "p2" ->
   { v_type = TClass ("Person"); v_key = "p2"; is_const = false;
     assignment_count = 1;
     v_value =
     VObjectReference (ObjectReference ({ class_key = "Person";
                                          field_references_table =
                                          [["age" ->
                                             { key = "age"; field_type = TInt;
                                               field_value = VInt (30);
                                               is_const = false;
                                               assignments_count = 0 }
                                          
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("Alice");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 2 })); scope_level = 0
  }
  
  "b" ->
   { v_type = TInt; v_key = "b"; is_const = false; assignment_count = 1;
     v_value = VInt (50); scope_level = 0 }
  
  "objNEq" ->
   { v_type = TInt; v_key = "objNEq"; is_const = false; assignment_count = 2;
     v_value = VInt (1); scope_level = 0 }
  
  "eVal" ->
   { v_type = TInt; v_key = "eVal"; is_const = false; assignment_count = 2;
     v_value = VInt (1); scope_level = 0 }
  
  "p3" ->
   { v_type = TClass ("Person"); v_key = "p3"; is_const = false;
     assignment_count = 1;
     v_value =
     VObjectReference (ObjectReference ({ class_key = "Person";
                                          field_references_table =
                                          [["age" ->
                                             { key = "age"; field_type = TInt;
                                               field_value = VInt (20);
                                               is_const = false;
                                               assignments_count = 0 }
                                          
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("Bob");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  "p1" ->
   { v_type = TClass ("Person"); v_key = "p1"; is_const = false;
     assignment_count = 1;
     v_value =
     VObjectReference (ObjectReference ({ class_key = "Person";
                                          field_references_table =
                                          [["age" ->
                                             { key = "age"; field_type = TInt;
                                               field_value = VInt (20);
                                               is_const = false;
                                               assignments_count = 0 }
                                          
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("Bob");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  "s2" ->
   { v_type = TString; v_key = "s2"; is_const = false; assignment_count = 1;
     v_value = VString ("b"); scope_level = 0 }
  
  "lVal" ->
   { v_type = TInt; v_key = "lVal"; is_const = false; assignment_count = 2;
     v_value = VInt (1); scope_level = 0 }
  
  "sEq" ->
   { v_type = TInt; v_key = "sEq"; is_const = false; assignment_count = 2;
     v_value = VInt (1); scope_level = 0 }
  
  "notVal" ->
   { v_type = TInt; v_key = "notVal"; is_const = false; assignment_count = 2;
     v_value = VInt (1); scope_level = 0 }
  
  "objEq" ->
   { v_type = TInt; v_key = "objEq"; is_const = false; assignment_count = 2;
     v_value = VInt (1); scope_level = 0 }
  
  "leVal" ->
   { v_type = TInt; v_key = "leVal"; is_const = false; assignment_count = 2;
     v_value = VInt (1); scope_level = 0 }
  
  "mVal" ->
   { v_type = TInt; v_key = "mVal"; is_const = false; assignment_count = 2;
     v_value = VInt (1); scope_level = 0 }
  
  "orVal" ->
   { v_type = TInt; v_key = "orVal"; is_const = false; assignment_count = 2;
     v_value = VInt (1); scope_level = 0 }
  
  "c" ->
   { v_type = TInt; v_key = "c"; is_const = false; assignment_count = 1;
     v_value = VInt (100); scope_level = 0 }
  
  "sNEQ" ->
   { v_type = TInt; v_key = "sNEQ"; is_const = false; assignment_count = 1;
     v_value = VInt (1); scope_level = 0 }
  
  "neVal" ->
   { v_type = TInt; v_key = "neVal"; is_const = false; assignment_count = 2;
     v_value = VInt (1); scope_level = 0 }
  
  "andVal" ->
   { v_type = TInt; v_key = "andVal"; is_const = false; assignment_count = 2;
     v_value = VInt (1); scope_level = 0 }
  
  ]]
  ; last_expr_result = VInt (1); runtime_signal = NoSignal;
  curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 2; is_creation = false; constr_affilation = None
  }
  
  -_-_-_-_-_-_-_-_-_-_- Method call testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; field_references_table = [[]]
                                              ;
                       number = 0 });
    var_table =
    [["a2" ->
       { v_type = TInt; v_key = "a2"; is_const = false; assignment_count = 1;
         v_value = VInt (30); scope_level = 0 }
    
  "a1" ->
   { v_type = TInt; v_key = "a1"; is_const = false; assignment_count = 1;
     v_value = VInt (25); scope_level = 0 }
  
  "res" ->
   { v_type = TInt; v_key = "res"; is_const = false; assignment_count = 1;
     v_value = VInt (125); scope_level = 0 }
  
  "person" ->
   { v_type = TClass ("Person"); v_key = "person"; is_const = false;
     assignment_count = 1;
     v_value =
     VObjectReference (ObjectReference ({ class_key = "Person";
                                          field_references_table =
                                          [["age" ->
                                             { key = "age"; field_type = TInt;
                                               field_value = VInt (30);
                                               is_const = false;
                                               assignments_count = 1 }
                                          
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("Bob");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  ]]
  ; last_expr_result = VInt (30); runtime_signal = NoSignal;
  curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 1; is_creation = false; constr_affilation = None
  }
  
  -_-_-_-_-_-_-_-_-_-_- Update object state testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; field_references_table = [[]]
                                              ;
                       number = 0 });
    var_table =
    [["p2" ->
       { v_type = TClass ("Person"); v_key = "p2"; is_const = false;
         assignment_count = 1;
         v_value =
         VObjectReference (ObjectReference ({ class_key = "Person";
                                              field_references_table =
                                              [["age" ->
                                                 { key = "age";
                                                   field_type = TInt;
                                                   field_value = VInt (55);
                                                   is_const = false;
                                                   assignments_count = 1 }
                                              
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("Bob");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  "p3" ->
   { v_type = TClass ("Person"); v_key = "p3"; is_const = false;
     assignment_count = 1;
     v_value =
     VObjectReference (ObjectReference ({ class_key = "Person";
                                          field_references_table =
                                          [["age" ->
                                             { key = "age"; field_type = TInt;
                                               field_value = VInt (55);
                                               is_const = false;
                                               assignments_count = 1 }
                                          
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("Bob");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  "res" ->
   { v_type = TInt; v_key = "res"; is_const = false; assignment_count = 1;
     v_value = VInt (55); scope_level = 0 }
  
  "p1" ->
   { v_type = TClass ("Person"); v_key = "p1"; is_const = false;
     assignment_count = 1;
     v_value =
     VObjectReference (ObjectReference ({ class_key = "Person";
                                          field_references_table =
                                          [["age" ->
                                             { key = "age"; field_type = TInt;
                                               field_value = VInt (55);
                                               is_const = false;
                                               assignments_count = 1 }
                                          
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("Bob");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  "person" ->
   { v_type = TClass ("Person"); v_key = "person"; is_const = false;
     assignment_count = 1;
     v_value =
     VObjectReference (ObjectReference ({ class_key = "Person";
                                          field_references_table =
                                          [["age" ->
                                             { key = "age"; field_type = TInt;
                                               field_value = VInt (55);
                                               is_const = false;
                                               assignments_count = 1 }
                                          
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("Bob");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  ]]
  ; last_expr_result = VInt (55); runtime_signal = NoSignal;
  curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 1; is_creation = false; constr_affilation = None
  }
  
  -_-_-_-_-_-_-_-_-_-_- Inheritance testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; field_references_table = [[]]
                                              ;
                       number = 0 });
    var_table =
    [["childSecond" ->
       { v_type = TClass ("Child"); v_key = "childSecond"; is_const = false;
         assignment_count = 1;
         v_value =
         VObjectReference (ObjectReference ({ class_key = "Child";
                                              field_references_table =
                                              [["parent" ->
                                                 { key = "parent";
                                                   field_type =
                                                   TClass ("Person");
                                                   field_value =
                                                   VObjectReference (ObjectReference (
                                                                      { class_key =
                                                                      "Person";
                                                                      field_references_table =
                                                                      [[
                                                                      "age" ->
                                                                      { key =
                                                                      "age";
                                                                      field_type =
                                                                      TInt;
                                                                      field_value =
                                                                      VInt (27);
                                                                      is_const =
                                                                      false;
                                                                      assignments_count =
                                                                      1 }
                                                                      
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("Bob");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 1 })); is_const = false; assignments_count = 0
  }
  
  "age" ->
   { key = "age"; field_type = TInt; field_value = VInt (20); is_const = false;
     assignments_count = 1 }
  
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 4 })); scope_level = 0
  }
  
  "person" ->
   { v_type = TClass ("Person"); v_key = "person"; is_const = false;
     assignment_count = 1;
     v_value =
     VObjectReference (ObjectReference ({ class_key = "Person";
                                          field_references_table =
                                          [["age" ->
                                             { key = "age"; field_type = TInt;
                                               field_value = VInt (27);
                                               is_const = false;
                                               assignments_count = 1 }
                                          
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("Bob");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  "childFirst" ->
   { v_type = TClass ("Child"); v_key = "childFirst"; is_const = false;
     assignment_count = 1;
     v_value =
     VObjectReference (ObjectReference ({ class_key = "Child";
                                          field_references_table =
                                          [["parent" ->
                                             { key = "parent";
                                               field_type = TClass ("Person");
                                               field_value =
                                               VObjectReference (ObjectReference (
                                                                  { class_key =
                                                                    "Person";
                                                                    field_references_table =
                                                                    [["age" ->
                                                                      { key =
                                                                      "age";
                                                                      field_type =
                                                                      TInt;
                                                                      field_value =
                                                                      VInt (40);
                                                                      is_const =
                                                                      false;
                                                                      assignments_count =
                                                                      0 }
                                                                    
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("Flexer");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 3 })); is_const = false; assignments_count = 0
  }
  
  "age" ->
   { key = "age"; field_type = TInt; field_value = VInt (4); is_const = false;
     assignments_count = 1 }
  
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("Alice");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 2 })); scope_level = 0
  }
  
  ]]
  ; last_expr_result = VVoid; runtime_signal = NoSignal;
  curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 4; is_creation = false; constr_affilation = None
  }
  
  -_-_-_-_-_-_-_-_-_-_- Scope testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; field_references_table = [[]]
                                              ;
                       number = 0 });
    var_table =
    [["a" ->
       { v_type = TInt; v_key = "a"; is_const = false; assignment_count = 5;
         v_value = VInt (1000); scope_level = 0 }
    
  "b" ->
   { v_type = TInt; v_key = "b"; is_const = false; assignment_count = 4;
     v_value = VInt (2000); scope_level = 0 }
  
  "c" ->
   { v_type = TInt; v_key = "c"; is_const = false; assignment_count = 4;
     v_value = VInt (3000); scope_level = 0 }
  
  "i" ->
   { v_type = TInt; v_key = "i"; is_const = false; assignment_count = 4;
     v_value = VInt (3); scope_level = 0 }
  
  ]]
  ; last_expr_result = VInt (3000); runtime_signal = NoSignal;
  curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 1; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 0; is_creation = false; constr_affilation = None
  }
  
  -_-_-_-_-_-_-_-_-_-_- Many loops and array bubble sorting testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; field_references_table = [[]]
                                              ;
                       number = 0 });
    var_table =
    [["n" ->
       { v_type = TInt; v_key = "n"; is_const = false; assignment_count = 1;
         v_value = VInt (11); scope_level = 0 }
    
  "arr" ->
   { v_type = TArray (TInt); v_key = "arr"; is_const = false;
     assignment_count = 1;
     v_value =
     VArray (ArrayReference ({ array_type = TInt;
                               array_values =
                               [VInt (0); VInt (1); VInt (2); VInt (3);
                                VInt (4); VInt (5); VInt (6); VInt (7);
                                VInt (8); VInt (9); VInt (10)];
                               number = 1 }));
     scope_level = 0 }
  
  "i" ->
   { v_type = TInt; v_key = "i"; is_const = false; assignment_count = 11;
     v_value = VInt (10); scope_level = 1 }
  
  ]]
  ; last_expr_result = VBool (false); runtime_signal = NoSignal;
  curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 2; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 1; is_creation = false; constr_affilation = None
  }
  
  -_-_-_-_-_-_-_-_-_-_- Break and continue testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; field_references_table = [[]]
                                              ;
                       number = 0 });
    var_table =
    [["b" ->
       { v_type = TArray (TInt); v_key = "b"; is_const = false;
         assignment_count = 1;
         v_value =
         VArray (ArrayReference ({ array_type = TInt;
                                   array_values =
                                   [VInt (0); VInt (1); VInt (2); VInt (3);
                                    VInt (4); VInt (5); VInt (6); VInt (7);
                                    VInt (8); VInt (9); VInt (10); VInt (11);
                                    VInt (12); VInt (13); VInt (14); VInt (0);
                                    VInt (0); VInt (0); VInt (0); VInt (0)];
                                   number = 2 }));
         scope_level = 1 }
    
  "arr" ->
   { v_type = TArray (TInt); v_key = "arr"; is_const = false;
     assignment_count = 1;
     v_value =
     VArray (ArrayReference ({ array_type = TInt;
                               array_values =
                               [VInt (0); VInt (1); VInt (0); VInt (1);
                                VInt (0); VInt (1); VInt (0); VInt (1);
                                VInt (0); VInt (1)];
                               number = 1 }));
     scope_level = 0 }
  
  ]]
  ; last_expr_result = VInt (16); runtime_signal = NoSignal;
  curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 2; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 2; is_creation = false; constr_affilation = None
  }
  
  -_-_-_-_-_-_-_-_-_-_- Array sorting as a function, where array state change in another context testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; field_references_table = [[]]
                                              ;
                       number = 0 });
    var_table =
    [["bubbleSorter" ->
       { v_type = TClass ("BubbleSorter"); v_key = "bubbleSorter";
         is_const = false; assignment_count = 1;
         v_value =
         VObjectReference (ObjectReference ({ class_key = "BubbleSorter";
                                              field_references_table = [[]]
                                              ; number = 2 }));
         scope_level = 0 }
    
  "arr" ->
   { v_type = TArray (TInt); v_key = "arr"; is_const = false;
     assignment_count = 1;
     v_value =
     VArray (ArrayReference ({ array_type = TInt;
                               array_values =
                               [VInt (0); VInt (1); VInt (2); VInt (3);
                                VInt (4); VInt (5); VInt (6); VInt (7);
                                VInt (8); VInt (9); VInt (10); VInt (11);
                                VInt (12); VInt (13); VInt (14); VInt (15)];
                               number = 1 }));
     scope_level = 0 }
  
  ]]
  ; last_expr_result = VVoid; runtime_signal = NoSignal;
  curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 2; is_creation = false; constr_affilation = None
  }
  
  -_-_-_-_-_-_-_-_-_-_- Object state changing in another context testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; field_references_table = [[]]
                                              ;
                       number = 0 });
    var_table =
    [["child" ->
       { v_type = TClass ("Child"); v_key = "child"; is_const = false;
         assignment_count = 1;
         v_value =
         VObjectReference (ObjectReference ({ class_key = "Child";
                                              field_references_table =
                                              [["parent" ->
                                                 { key = "parent";
                                                   field_type =
                                                   TClass ("Person");
                                                   field_value =
                                                   VObjectReference (ObjectReference (
                                                                      { class_key =
                                                                      "Person";
                                                                      field_references_table =
                                                                      [[
                                                                      "age" ->
                                                                      { key =
                                                                      "age";
                                                                      field_type =
                                                                      TInt;
                                                                      field_value =
                                                                      VInt (30);
                                                                      is_const =
                                                                      false;
                                                                      assignments_count =
                                                                      1 }
                                                                      
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("Bob");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 1 })); is_const = false; assignments_count = 0
  }
  
  "age" ->
   { key = "age"; field_type = TInt; field_value = VInt (0); is_const = false;
     assignments_count = 0 }
  
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 2 })); scope_level = 0
  }
  
  "person" ->
   { v_type = TClass ("Person"); v_key = "person"; is_const = false;
     assignment_count = 1;
     v_value =
     VObjectReference (ObjectReference ({ class_key = "Person";
                                          field_references_table =
                                          [["age" ->
                                             { key = "age"; field_type = TInt;
                                               field_value = VInt (30);
                                               is_const = false;
                                               assignments_count = 1 }
                                          
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("Bob");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  ]]
  ; last_expr_result = VVoid; runtime_signal = NoSignal;
  curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 2; is_creation = false; constr_affilation = None
  }
  
  -_-_-_-_-_-_-_-_-_-_- Pattern Visitor testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; field_references_table = [[]]
                                              ;
                       number = 0 });
    var_table =
    [["resPerimeter" ->
       { v_type = TArray (TInt); v_key = "resPerimeter"; is_const = false;
         assignment_count = 1;
         v_value =
         VArray (ArrayReference ({ array_type = TInt;
                                   array_values =
                                   [VInt (75); VInt (8); VInt (0)]; number = 7
                                   }));
         scope_level = 0 }
    
  "areaVisitor" ->
   { v_type = TClass ("AreaVisitor"); v_key = "areaVisitor"; is_const = false;
     assignment_count = 1;
     v_value =
     VObjectReference (ObjectReference ({ class_key = "AreaVisitor";
                                          field_references_table = [[]]
                                          ; number = 5 }));
     scope_level = 0 }
  
  "list" ->
   { v_type = TArray (TClass ("Figure")); v_key = "list"; is_const = false;
     assignment_count = 1;
     v_value =
     VArray (ArrayReference ({ array_type = TClass ("Figure");
                               array_values =
                               [VObjectReference (ObjectReference ({ class_key =
                                                                     "Circle";
                                                                     field_references_table =
                                                                     [[
                                                                     "radius"
                                                                      ->
                                                                      { key =
                                                                      "radius";
                                                                      field_type =
                                                                      TInt;
                                                                      field_value =
                                                                      VInt (5);
                                                                      is_const =
                                                                      false;
                                                                      assignments_count =
                                                                      0 }
                                                                     
  ]]
  ; number = 1 }));
  VObjectReference (ObjectReference ({ class_key = "Rectangle";
                                       field_references_table =
                                       [["a" ->
                                          { key = "a"; field_type = TInt;
                                            field_value = VInt (2);
                                            is_const = false;
                                            assignments_count = 0 }
                                       
  "b" ->
   { key = "b"; field_type = TInt; field_value = VInt (4); is_const = false;
     assignments_count = 0 }
  
  ]]
  ; number = 2 }));
  VObjectReference (ObjectReference ({ class_key = "Triangle";
                                       field_references_table =
                                       [["a" ->
                                          { key = "a"; field_type = TInt;
                                            field_value = VInt (1);
                                            is_const = false;
                                            assignments_count = 0 }
                                       
  "b" ->
   { key = "b"; field_type = TInt; field_value = VInt (1); is_const = false;
     assignments_count = 0 }
  
  "c" ->
   { key = "c"; field_type = TInt; field_value = VInt (1); is_const = false;
     assignments_count = 0 }
  
  ]]
  ; number = 3 }))]; number = 4 })); scope_level = 0
  }
  
  "resArea" ->
   { v_type = TArray (TInt); v_key = "resArea"; is_const = false;
     assignment_count = 1;
     v_value =
     VArray (ArrayReference ({ array_type = TInt;
                               array_values = [VInt (30); VInt (12); VInt (3)];
                               number = 8 }));
     scope_level = 0 }
  
  "perimeterVisitor" ->
   { v_type = TClass ("PerimeterVisitor"); v_key = "perimeterVisitor";
     is_const = false; assignment_count = 1;
     v_value =
     VObjectReference (ObjectReference ({ class_key = "PerimeterVisitor";
                                          field_references_table = [[]]
                                          ; number = 6 }));
     scope_level = 0 }
  
  ]]
  ; last_expr_result = VBool (false); runtime_signal = NoSignal;
  curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 2; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 8; is_creation = false; constr_affilation = None
  }
  
  -_-_-_-_-_-_-_-_-_-_- Factorial recursion testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; field_references_table = [[]]
                                              ;
                       number = 0 });
    var_table =
    [["f" ->
       { v_type = TInt; v_key = "f"; is_const = false; assignment_count = 1;
         v_value = VInt (120); scope_level = 0 }
    
  "factorial" ->
   { v_type = TClass ("Factorial"); v_key = "factorial"; is_const = false;
     assignment_count = 1;
     v_value =
     VObjectReference (ObjectReference ({ class_key = "Factorial";
                                          field_references_table = [[]]
                                          ; number = 1 }));
     scope_level = 0 }
  
  ]]
  ; last_expr_result = VInt (120); runtime_signal = NoSignal;
  curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 1; is_creation = false; constr_affilation = None
  }
  
  -_-_-_-_-_-_-_-_-_-_- Quick sort recursion testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; field_references_table = [[]]
                                              ;
                       number = 0 });
    var_table =
    [["n" ->
       { v_type = TInt; v_key = "n"; is_const = false; assignment_count = 1;
         v_value = VInt (16); scope_level = 0 }
    
  "quickSorter" ->
   { v_type = TClass ("QuickSorter"); v_key = "quickSorter"; is_const = false;
     assignment_count = 1;
     v_value =
     VObjectReference (ObjectReference ({ class_key = "QuickSorter";
                                          field_references_table = [[]]
                                          ; number = 2 }));
     scope_level = 0 }
  
  "arr" ->
   { v_type = TArray (TInt); v_key = "arr"; is_const = false;
     assignment_count = 1;
     v_value =
     VArray (ArrayReference ({ array_type = TInt;
                               array_values =
                               [VInt (0); VInt (1); VInt (2); VInt (3);
                                VInt (4); VInt (5); VInt (6); VInt (7);
                                VInt (8); VInt (9); VInt (10); VInt (11);
                                VInt (12); VInt (13); VInt (14); VInt (15)];
                               number = 1 }));
     scope_level = 0 }
  
  "high" ->
   { v_type = TInt; v_key = "high"; is_const = false; assignment_count = 1;
     v_value = VInt (15); scope_level = 0 }
  
  "low" ->
   { v_type = TInt; v_key = "low"; is_const = false; assignment_count = 1;
     v_value = VInt (0); scope_level = 0 }
  
  ]]
  ; last_expr_result = VVoid; runtime_signal = NoSignal;
  curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 2; is_creation = false; constr_affilation = None
  }
  
  -_-_-_-_-_-_-_-_-_-_- ArrayTypeMismatchException testing -_-_-_-_-_-_-_-_-_-_-
  
  ArrayTypeMismatchException
  -_-_-_-_-_-_-_-_-_-_- Constructor chaining testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; field_references_table = [[]]
                                              ;
                       number = 0 });
    var_table =
    [["cat" ->
       { v_type = TClass ("Cat"); v_key = "cat"; is_const = false;
         assignment_count = 1;
         v_value =
         VObjectReference (ObjectReference ({ class_key = "Cat";
                                              field_references_table =
                                              [["age" ->
                                                 { key = "age";
                                                   field_type = TInt;
                                                   field_value = VInt (2);
                                                   is_const = false;
                                                   assignments_count = 0 }
                                              
  "hairLevel" ->
   { key = "hairLevel"; field_type = TInt; field_value = VInt (30);
     is_const = false; assignments_count = 0 }
  
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("Mars");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 1 })); scope_level = 0
  }
  
  ]]
  ;
  last_expr_result =
  VObjectReference (ObjectReference ({ class_key = "Cat";
                                       field_references_table =
                                       [["age" ->
                                          { key = "age"; field_type = TInt;
                                            field_value = VInt (2);
                                            is_const = false;
                                            assignments_count = 0 }
                                       
  "hairLevel" ->
   { key = "hairLevel"; field_type = TInt; field_value = VInt (30);
     is_const = false; assignments_count = 0 }
  
  "name" ->
   { key = "name"; field_type = TString; field_value = VString ("Mars");
     is_const = false; assignments_count = 0 }
  
  ]]
  ; number = 1 })); runtime_signal = NoSignal; curr_method_type = TVoid;
  is_main_scope = true; nested_loops_cnt = 0; scope_level = 0;
  cur_constr_key = None; prev_context = None; obj_created_cnt = 1;
  is_creation = false; constr_affilation = None
  }
  
  -_-_-_-_-_-_-_-_-_-_- Constructor chaining recursion testing -_-_-_-_-_-_-_-_-_-_-
  
  Constructor recursion
  -_-_-_-_-_-_-_-_-_-_- Const fields testing -_-_-_-_-_-_-_-_-_-_-
  
  Assignment to a constant field
  -_-_-_-_-_-_-_-_-_-_- Const variables testing -_-_-_-_-_-_-_-_-_-_-
  
  Assignment to a constant variable
  -_-_-_-_-_-_-_-_-_-_- Ad-hoc polymorphism, specifically methods overloading, testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; field_references_table = [[]]
                                              ;
                       number = 0 });
    var_table =
    [["summator" ->
       { v_type = TClass ("Summator"); v_key = "summator"; is_const = false;
         assignment_count = 1;
         v_value =
         VObjectReference (ObjectReference ({ class_key = "Summator";
                                              field_references_table = [[]]
                                              ; number = 1 }));
         scope_level = 0 }
    
  "a" ->
   { v_type = TInt; v_key = "a"; is_const = false; assignment_count = 1;
     v_value = VInt (8); scope_level = 0 }
  
  "s" ->
   { v_type = TString; v_key = "s"; is_const = false; assignment_count = 1;
     v_value = VString ("GGWP"); scope_level = 0 }
  
  ]]
  ; last_expr_result = VString ("GGWP"); runtime_signal = NoSignal;
  curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
  scope_level = 0; cur_constr_key = None; prev_context = None;
  obj_created_cnt = 1; is_creation = false; constr_affilation = None
  }
  
