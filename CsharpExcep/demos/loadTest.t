  $ (cd ../../../../default && demos/demoLoad.exe) 
  -_-_-_-_-_-_-_-_-_-_- Inheritance testing from Exception class -_-_-_-_-_-_-_-_-_-_-
  
  [["Program" ->
     { class_key = "Program"; field_table = [[]]
                              ;
       method_table =
       [["C3" ->
          { method_type = Void; has_override = false; has_static_mod = true;
            method_key = "C3"; args = [];
            body =
            StatementBlock ([Try
                             (StatementBlock ([Expression (CallMethod
                                                           ("B3", []))]),
                              [(Some ((CsClass ("ShittyExn"),
                                       Some (IdentVar ("e")))),
                                Some (Access
                                      (IdentVar ("e"),
                                       CallMethod ("Filter", []))),
                                StatementBlock ([Print (ConstExpr (VString ("C")))]))],
                              None)])
            }
       
  "E3" ->
   { method_type = Void; has_override = false; has_static_mod = true;
     method_key = "E3"; args = [];
     body =
     StatementBlock ([Try
                      (StatementBlock ([Expression (CallMethod ("D3", []))]),
                       [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                         Some (Access
                               (IdentVar ("e"), CallMethod ("Filter", []))),
                         StatementBlock ([Print (ConstExpr (VString ("E")))]))],
                       None)])
     }
  
  "D3" ->
   { method_type = Void; has_override = false; has_static_mod = true;
     method_key = "D3"; args = [];
     body =
     StatementBlock ([Try
                      (StatementBlock ([Expression (CallMethod ("C3", []))]),
                       [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                         Some (Access
                               (IdentVar ("e"), CallMethod ("Filter", []))),
                         StatementBlock ([Print (ConstExpr (VString ("D")))]))],
                       None)])
     }
  
  "Main" ->
   { method_type = Void; has_override = false; has_static_mod = true;
     method_key = "Main"; args = [];
     body = StatementBlock ([Expression (CallMethod ("E3", []))]) }
  
  "A3IntString" ->
   { method_type = Void; has_override = false; has_static_mod = true;
     method_key = "A3IntString"; args = [(Int, "a"); (String, "b")];
     body =
     StatementBlock ([VarDeclare
                      (None, Int, [("x", Some (ConstExpr (VInt (0))))]);
                      Try
                      (StatementBlock ([Throw (ClassCreate ("cat", []))]), 
                       [], Some (StatementBlock ([Print (IdentVar ("x"))])))])
     }
  
  "B3" ->
   { method_type = Void; has_override = false; has_static_mod = true;
     method_key = "B3"; args = [];
     body =
     StatementBlock ([Try
                      (StatementBlock ([Expression (CallMethod ("A3", []))]),
                       [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                         Some (Access
                               (IdentVar ("e"), CallMethod ("Filter", []))),
                         StatementBlock ([Print (ConstExpr (VString ("B")))]))],
                       None)])
     }
  
  ]]
  ;
  constructor_table =
  [["Program" -> { key = "Program"; args = []; body = StatementBlock ([]) }
  
  ]]
  ; parent_key = None; children_keys = [];
  dec_tree =
  Class
  ([], "Program", None,
   [([Static],
     Method
     (Void, "Main", [], StatementBlock ([Expression (CallMethod ("E3", []))])));
    ([Public; Static],
     Method
     (Void, "A3", [(Int, "a"); (String, "b")],
      StatementBlock ([VarDeclare
                       (None, Int, [("x", Some (ConstExpr (VInt (0))))]);
                       Try
                       (StatementBlock ([Throw (ClassCreate ("cat", []))]), 
                        [], Some (StatementBlock ([Print (IdentVar ("x"))])))])));
    ([Public; Static],
     Method
     (Void, "B3", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("A3", []))]),
                        [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                          Some (Access
                                (IdentVar ("e"), CallMethod ("Filter", []))),
                          StatementBlock ([Print (ConstExpr (VString ("B")))]))],
                        None)])));
    ([Public; Static],
     Method
     (Void, "C3", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("B3", []))]),
                        [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                          Some (Access
                                (IdentVar ("e"), CallMethod ("Filter", []))),
                          StatementBlock ([Print (ConstExpr (VString ("C")))]))],
                        None)])));
    ([Public; Static],
     Method
     (Void, "D3", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("C3", []))]),
                        [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                          Some (Access
                                (IdentVar ("e"), CallMethod ("Filter", []))),
                          StatementBlock ([Print (ConstExpr (VString ("D")))]))],
                        None)])));
    ([Public; Static],
     Method
     (Void, "E3", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("D3", []))]),
                        [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                          Some (Access
                                (IdentVar ("e"), CallMethod ("Filter", []))),
                          StatementBlock ([Print (ConstExpr (VString ("E")))]))],
                        None)])))])
  }
  
  "Exception" ->
   { class_key = "Exception";
     field_table =
     [["message" ->
        { field_type = String; field_key = "message"; is_const = false;
          sub_tree = None }
     
  ]]
  ;
  method_table =
  [["ToString" ->
     { method_type = String; has_override = true; has_static_mod = false;
       method_key = "ToString"; args = [];
       body = StatementBlock ([Return (Some (IdentVar ("message")))]) }
  
  ]]
  ;
  constructor_table =
  [["Exception" -> { key = "Exception"; args = []; body = StatementBlock ([]) }
  
  ]]
  ; parent_key = None; children_keys = ["ShittyExn"];
  dec_tree =
  Class
  ([Public], "Exception", None,
   [([Public], VariableField (String, [("message", None)]));
    ([Public],
     Method
     (String, "ToString", [],
      StatementBlock ([Return (Some (IdentVar ("message")))])))])
  }
  
  "ShittyExn" ->
   { class_key = "ShittyExn";
     field_table =
     [["message" ->
        { field_type = String; field_key = "message"; is_const = false;
          sub_tree = None }
     
  ]]
  ;
  method_table =
  [["ToString" ->
     { method_type = String; has_override = true; has_static_mod = false;
       method_key = "ToString"; args = [];
       body = StatementBlock ([Return (Some (IdentVar ("message")))]) }
  
  "Filter" ->
   { method_type = Bool; has_override = false; has_static_mod = false;
     method_key = "Filter"; args = [];
     body = StatementBlock ([Return (Some (CallMethod ("f", [])))]) }
  
  ]]
  ;
  constructor_table =
  [["ShittyExn" ->
     { key = "ShittyExn"; args = [];
       body =
       StatementBlock ([Expression (Assign (IdentVar ("f"), IdentVar ("f")))])
       }
  
  ]]
  ; parent_key = Some ("Exception"); children_keys = [];
  dec_tree =
  Class
  ([], "ShittyExn", Some ("Exception"),
   [([Public],
     Constructor
     ("ShittyExn", [],
      StatementBlock ([Expression (Assign (IdentVar ("f"), IdentVar ("f")))])));
    ([Public],
     Method
     (Bool, "Filter", [],
      StatementBlock ([Return (Some (CallMethod ("f", [])))])))])
  }
  
  -_-_-_-_-_-_-_-_-_-_- Incorrect modifiers testing -_-_-_-_-_-_-_-_-_-_-
  
  Method can not be const
  Wrong class modifiers
  Wrong constructor modifiers
  -_-_-_-_-_-_-_-_-_-_- Similar fields error testing -_-_-_-_-_-_-_-_-_-_-
  
  Similar fields
  -_-_-_-_-_-_-_-_-_-_- Similar methods error testing -_-_-_-_-_-_-_-_-_-_-
  
  Method with this type exists
  -_-_-_-_-_-_-_-_-_-_- Constructor name error -_-_-_-_-_-_-_-_-_-_-
  
  Constructor name error
  -_-_-_-_-_-_-_-_-_-_- Similar constructor error testing -_-_-_-_-_-_-_-_-_-_-
  
  Constructor with this type exists
  -_-_-_-_-_-_-_-_-_-_- Similar Classes error -_-_-_-_-_-_-_-_-_-_-
  
  Similar Classes
  -_-_-_-_-_-_-_-_-_-_- The class can only be inherited from the Exception class error -_-_-_-_-_-_-_-_-_-_-
  
  The class can only be inherited from the Exception class!
  -_-_-_-_-_-_-_-_-_-_- Override errors testing -_-_-_-_-_-_-_-_-_-_-
  
  ]]
  [["Program" ->
     { class_key = "Program"; field_table = [[]]
                              ;
       method_table =
       [["C3" ->
          { method_type = Void; has_override = false; has_static_mod = true;
            method_key = "C3"; args = [];
            body =
            StatementBlock ([Try
                             (StatementBlock ([Expression (CallMethod
                                                           ("B3", []))]),
                              [(Some ((CsClass ("ShittyExn"),
                                       Some (IdentVar ("e")))),
                                Some (Access
                                      (IdentVar ("e"),
                                       CallMethod ("Filter", []))),
                                StatementBlock ([Print (ConstExpr (VString ("C")))]))],
                              None)])
            }
       
  "E3" ->
   { method_type = Void; has_override = false; has_static_mod = true;
     method_key = "E3"; args = [];
     body =
     StatementBlock ([Try
                      (StatementBlock ([Expression (CallMethod ("D3", []))]),
                       [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                         Some (Access
                               (IdentVar ("e"), CallMethod ("Filter", []))),
                         StatementBlock ([Print (ConstExpr (VString ("E")))]))],
                       None)])
     }
  
  "D3" ->
   { method_type = Void; has_override = false; has_static_mod = true;
     method_key = "D3"; args = [];
     body =
     StatementBlock ([Try
                      (StatementBlock ([Expression (CallMethod ("C3", []))]),
                       [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                         Some (Access
                               (IdentVar ("e"), CallMethod ("Filter", []))),
                         StatementBlock ([Print (ConstExpr (VString ("D")))]))],
                       None)])
     }
  
  "Main" ->
   { method_type = Void; has_override = false; has_static_mod = true;
     method_key = "Main"; args = [];
     body = StatementBlock ([Expression (CallMethod ("E3", []))]) }
  
  "A3IntString" ->
   { method_type = Void; has_override = false; has_static_mod = true;
     method_key = "A3IntString"; args = [(Int, "a"); (String, "b")];
     body =
     StatementBlock ([VarDeclare
                      (None, Int, [("x", Some (ConstExpr (VInt (0))))]);
                      Try
                      (StatementBlock ([Throw (ClassCreate ("cat", []))]), 
                       [], Some (StatementBlock ([Print (IdentVar ("x"))])))])
     }
  
  "B3" ->
   { method_type = Void; has_override = false; has_static_mod = true;
     method_key = "B3"; args = [];
     body =
     StatementBlock ([Try
                      (StatementBlock ([Expression (CallMethod ("A3", []))]),
                       [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                         Some (Access
                               (IdentVar ("e"), CallMethod ("Filter", []))),
                         StatementBlock ([Print (ConstExpr (VString ("B")))]))],
                       None)])
     }
  
  ]]
  ;
  constructor_table =
  [["Program" -> { key = "Program"; args = []; body = StatementBlock ([]) }
  
  ]]
  ; parent_key = None; children_keys = [];
  dec_tree =
  Class
  ([], "Program", None,
   [([Static],
     Method
     (Void, "Main", [], StatementBlock ([Expression (CallMethod ("E3", []))])));
    ([Public; Static],
     Method
     (Void, "A3", [(Int, "a"); (String, "b")],
      StatementBlock ([VarDeclare
                       (None, Int, [("x", Some (ConstExpr (VInt (0))))]);
                       Try
                       (StatementBlock ([Throw (ClassCreate ("cat", []))]), 
                        [], Some (StatementBlock ([Print (IdentVar ("x"))])))])));
    ([Public; Static],
     Method
     (Void, "B3", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("A3", []))]),
                        [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                          Some (Access
                                (IdentVar ("e"), CallMethod ("Filter", []))),
                          StatementBlock ([Print (ConstExpr (VString ("B")))]))],
                        None)])));
    ([Public; Static],
     Method
     (Void, "C3", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("B3", []))]),
                        [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                          Some (Access
                                (IdentVar ("e"), CallMethod ("Filter", []))),
                          StatementBlock ([Print (ConstExpr (VString ("C")))]))],
                        None)])));
    ([Public; Static],
     Method
     (Void, "D3", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("C3", []))]),
                        [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                          Some (Access
                                (IdentVar ("e"), CallMethod ("Filter", []))),
                          StatementBlock ([Print (ConstExpr (VString ("D")))]))],
                        None)])));
    ([Public; Static],
     Method
     (Void, "E3", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("D3", []))]),
                        [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                          Some (Access
                                (IdentVar ("e"), CallMethod ("Filter", []))),
                          StatementBlock ([Print (ConstExpr (VString ("E")))]))],
                        None)])))])
  }
  
  "Exception" ->
   { class_key = "Exception";
     field_table =
     [["message" ->
        { field_type = String; field_key = "message"; is_const = false;
          sub_tree = None }
     
  ]]
  ;
  method_table =
  [["ToString" ->
     { method_type = String; has_override = true; has_static_mod = false;
       method_key = "ToString"; args = [];
       body = StatementBlock ([Return (Some (IdentVar ("message")))]) }
  
  ]]
  ;
  constructor_table =
  [["Exception" -> { key = "Exception"; args = []; body = StatementBlock ([]) }
  
  ]]
  ; parent_key = None; children_keys = ["ShittyExn"];
  dec_tree =
  Class
  ([Public], "Exception", None,
   [([Public], VariableField (String, [("message", None)]));
    ([Public],
     Method
     (String, "ToString", [],
      StatementBlock ([Return (Some (IdentVar ("message")))])))])
  }
  
  "ShittyExn" ->
   { class_key = "ShittyExn";
     field_table =
     [["message" ->
        { field_type = String; field_key = "message"; is_const = false;
          sub_tree = None }
     
  "age" ->
   { field_type = Int; field_key = "age"; is_const = false; sub_tree = None }
  
  ]]
  ;
  method_table =
  [["ToString" ->
     { method_type = String; has_override = true; has_static_mod = false;
       method_key = "ToString"; args = []; body = StatementBlock ([]) }
  
  "Filter" ->
   { method_type = Bool; has_override = false; has_static_mod = false;
     method_key = "Filter"; args = [];
     body = StatementBlock ([Return (Some (CallMethod ("f", [])))]) }
  
  ]]
  ;
  constructor_table =
  [["ShittyExn" ->
     { key = "ShittyExn"; args = [];
       body =
       StatementBlock ([Expression (Assign (IdentVar ("f"), IdentVar ("f")))])
       }
  
  ]]
  ; parent_key = Some ("Exception"); children_keys = [];
  dec_tree =
  Class
  ([], "ShittyExn", Some ("Exception"),
   [([], VariableField (Int, [("age", None)]));
    ([Public],
     Constructor
     ("ShittyExn", [],
      StatementBlock ([Expression (Assign (IdentVar ("f"), IdentVar ("f")))])));
    ([Public; Override], Method (String, "ToString", [], StatementBlock ([])));
    ([Public],
     Method
     (Bool, "Filter", [],
      StatementBlock ([Return (Some (CallMethod ("f", [])))])))])
  }
  
  Not overriden method or parent does not exist this method!
  ]]
