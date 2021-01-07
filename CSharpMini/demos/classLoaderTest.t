  $ (cd ../../../../default && demos/classLoaderTest.exe)
  -_-_-_-_-_-_-_-_-_-_- Inheritance testing -_-_-_-_-_-_-_-_-_-_-
  
  [["Program" ->
     { this_key = "Program"; fields_table = [[]]
                             ;
       methods_table =
       [["ToString" ->
          { method_type = TString; is_abstract = false; is_virtual = true;
            is_override = false; arguments = []; key = "ToString";
            body =
            Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]));
            is_overriden = false }
       
  "EqualsTClass (\"Object\")" ->
   { method_type = TInt; is_abstract = false; is_virtual = true;
     is_override = false; arguments = [(TClass ("Object"), Name ("obj"))];
     key = "EqualsTClass (\"Object\")";
     body =
     Some (StatementBlock ([If
                            (Equal (This, Identifier ("obj")),
                             Return (Some (Value (VInt (1)))),
                             Some (Return (Some (Value (VInt (0))))))]));
     is_overriden = false }
  
  "Main" ->
   { method_type = TVoid; is_abstract = false; is_virtual = false;
     is_override = false; arguments = []; key = "Main";
     body =
     Some (StatementBlock ([VariableDecl
                            (None, TClass ("Person"),
                             [(Name ("person"),
                               Some (ClassCreation
                                     (Name ("Person"),
                                      [Value (VInt (100)); Value (VInt (50))])))])]));
     is_overriden = false }
  
  ]]
  ;
  constructors_table =
  [["Program" ->
     { key = "Program"; arguments = []; call_constructor = None;
       body = StatementBlock ([]) }
  
  ]]
  ; children_keys = []; is_abstract = false; is_sealed = false;
  parent_key = Some ("Object");
  decl_tree =
  Class
  ([Public], Name ("Program"), None,
   [([Public; Static],
     Method
     (TVoid, Name ("Main"), [],
      Some (StatementBlock ([VariableDecl
                             (None, TClass ("Person"),
                              [(Name ("person"),
                                Some (ClassCreation
                                      (Name ("Person"),
                                       [Value (VInt (100)); Value (VInt (50))])))])]))))])
  }
  
  "Child" ->
   { this_key = "Child";
     fields_table =
     [["cash" ->
        { field_type = TInt; key = "cash"; is_const = false; sub_tree = None }
     
  "weight" ->
   { field_type = TInt; key = "weight"; is_const = false; sub_tree = None }
  
  "age" ->
   { field_type = TInt; key = "age"; is_const = false; sub_tree = None }
  
  ]]
  ;
  methods_table =
  [["ToString" ->
     { method_type = TString; is_abstract = false; is_virtual = true;
       is_override = false; arguments = []; key = "ToString";
       body =
       Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]));
       is_overriden = false }
  
  "EqualsTClass (\"Object\")" ->
   { method_type = TInt; is_abstract = false; is_virtual = true;
     is_override = false; arguments = [(TClass ("Object"), Name ("obj"))];
     key = "EqualsTClass (\"Object\")";
     body =
     Some (StatementBlock ([If
                            (Equal (This, Identifier ("obj")),
                             Return (Some (Value (VInt (1)))),
                             Some (Return (Some (Value (VInt (0))))))]));
     is_overriden = false }
  
  "SetAgeTInt" ->
   { method_type = TVoid; is_abstract = false; is_virtual = false;
     is_override = false; arguments = [(TInt, Name ("age"))];
     key = "SetAgeTInt";
     body =
     Some (StatementBlock ([Expression (Assign
                                        (AccessByPoint
                                         (This, Identifier ("age")),
                                         Identifier ("age")))]));
     is_overriden = false }
  
  "SetCashTInt" ->
   { method_type = TVoid; is_abstract = false; is_virtual = false;
     is_override = false; arguments = [(TInt, Name ("cash"))];
     key = "SetCashTInt";
     body =
     Some (StatementBlock ([Expression (Assign
                                        (AccessByPoint
                                         (This, Identifier ("cash")),
                                         Identifier ("cash")))]));
     is_overriden = false }
  
  "GetAge" ->
   { method_type = TInt; is_abstract = false; is_virtual = false;
     is_override = true; arguments = []; key = "GetAge";
     body =
     Some (StatementBlock ([Return (Some (Add
                                          (Identifier ("age"),
                                           Value (VInt (1)))))]));
     is_overriden = true }
  
  "SetWeightTInt" ->
   { method_type = TVoid; is_abstract = false; is_virtual = false;
     is_override = false; arguments = [(TInt, Name ("weight"))];
     key = "SetWeightTInt";
     body =
     Some (StatementBlock ([Expression (Assign
                                        (AccessByPoint
                                         (This, Identifier ("weight")),
                                         Identifier ("weight")))]));
     is_overriden = false }
  
  "GetWeight" ->
   { method_type = TInt; is_abstract = false; is_virtual = false;
     is_override = false; arguments = []; key = "GetWeight";
     body = Some (StatementBlock ([Return (Some (Identifier ("weight")))]));
     is_overriden = false }
  
  "GetCash" ->
   { method_type = TInt; is_abstract = false; is_virtual = false;
     is_override = false; arguments = []; key = "GetCash";
     body = Some (StatementBlock ([Return (Some (Identifier ("cash")))]));
     is_overriden = false }
  
  "TellEvenNumbersTInt" ->
   { method_type = TVoid; is_abstract = false; is_virtual = false;
     is_override = false; arguments = [(TInt, Name ("count"))];
     key = "TellEvenNumbersTInt";
     body =
     Some (StatementBlock ([For
                            (Some (VariableDecl
                                   (None, TInt,
                                    [(Name ("i"), Some (Value (VInt (0))))])),
                             Some (Less
                                   (Identifier ("i"), Identifier ("count"))),
                             [PostInc (Identifier ("i"))],
                             StatementBlock ([If
                                              (And
                                               (Equal
                                                (Mod
                                                 (Identifier ("i"),
                                                  Value (VInt (2))),
                                                 Value (VInt (0))),
                                                Not (Equal
                                                     (Mod
                                                      (Identifier ("i"),
                                                       Value (VInt (2))),
                                                      Value (VInt (1))))),
                                               StatementBlock ([Expression (
                                                                 AccessByPoint
                                                                 (Identifier ("Console"),
                                                                  CallMethod
                                                                  (Identifier ("WriteLine"),
                                                                   [Identifier ("i")])))]),
                                               Some (StatementBlock ([Continue])))]))]));
     is_overriden = false }
  
  ]]
  ;
  constructors_table =
  [["ChildTIntTInt" ->
     { key = "ChildTIntTInt";
       arguments = [(TInt, Name ("weight")); (TInt, Name ("age"))];
       call_constructor =
       Some (CallMethod (Base, [Identifier ("weight"); Identifier ("age")]));
       body =
       StatementBlock ([Expression (Assign
                                    (Identifier ("cash"), Value (VInt (0))))])
       }
  
  "ChildTIntTIntTInt" ->
   { key = "ChildTIntTIntTInt";
     arguments =
     [(TInt, Name ("weight")); (TInt, Name ("age")); (TInt, Name ("cash"))];
     call_constructor =
     Some (CallMethod (Base, [Identifier ("weight"); Identifier ("age")]));
     body =
     StatementBlock ([Expression (Assign
                                  (AccessByPoint (This, Identifier ("cash")),
                                   Identifier ("cash")))])
     }
  
  ]]
  ; children_keys = []; is_abstract = false; is_sealed = false;
  parent_key = Some ("Person");
  decl_tree =
  Class
  ([Public], Name ("Child"), Some (Name ("Person")),
   [([Public], Field (TInt, [(Name ("cash"), None)]));
    ([Public],
     Constructor
     (Name ("Child"), [(TInt, Name ("weight")); (TInt, Name ("age"))],
      Some (CallMethod (Base, [Identifier ("weight"); Identifier ("age")])),
      StatementBlock ([Expression (Assign
                                   (Identifier ("cash"), Value (VInt (0))))])));
    ([Public],
     Constructor
     (Name ("Child"),
      [(TInt, Name ("weight")); (TInt, Name ("age")); (TInt, Name ("cash"))],
      Some (CallMethod (Base, [Identifier ("weight"); Identifier ("age")])),
      StatementBlock ([Expression (Assign
                                   (AccessByPoint (This, Identifier ("cash")),
                                    Identifier ("cash")))])));
    ([Public; Override],
     Method
     (TInt, Name ("GetAge"), [],
      Some (StatementBlock ([Return (Some (Add
                                           (Identifier ("age"),
                                            Value (VInt (1)))))]))));
    ([Public],
     Method
     (TInt, Name ("GetCash"), [],
      Some (StatementBlock ([Return (Some (Identifier ("cash")))]))));
    ([Public],
     Method
     (TVoid, Name ("SetCash"), [(TInt, Name ("cash"))],
      Some (StatementBlock ([Expression (Assign
                                         (AccessByPoint
                                          (This, Identifier ("cash")),
                                          Identifier ("cash")))]))));
    ([Public],
     Method
     (TVoid, Name ("TellEvenNumbers"), [(TInt, Name ("count"))],
      Some (StatementBlock ([For
                             (Some (VariableDecl
                                    (None, TInt,
                                     [(Name ("i"), Some (Value (VInt (0))))])),
                              Some (Less
                                    (Identifier ("i"), Identifier ("count"))),
                              [PostInc (Identifier ("i"))],
                              StatementBlock ([If
                                               (And
                                                (Equal
                                                 (Mod
                                                  (Identifier ("i"),
                                                   Value (VInt (2))),
                                                  Value (VInt (0))),
                                                 Not (Equal
                                                      (Mod
                                                       (Identifier ("i"),
                                                        Value (VInt (2))),
                                                       Value (VInt (1))))),
                                                StatementBlock ([Expression (
                                                                  AccessByPoint
                                                                  (Identifier ("Console"),
                                                                   CallMethod
                                                                   (Identifier ("WriteLine"),
                                                                    [Identifier ("i")])))]),
                                                Some (StatementBlock ([Continue])))]))]))))])
  }
  
  "Object" ->
   { this_key = "Object"; fields_table = [[]]
                          ;
     methods_table =
     [["EqualsTClass (\"Object\")" ->
        { method_type = TInt; is_abstract = false; is_virtual = true;
          is_override = false; arguments = [(TClass ("Object"), Name ("obj"))];
          key = "EqualsTClass (\"Object\")";
          body =
          Some (StatementBlock ([If
                                 (Equal (This, Identifier ("obj")),
                                  Return (Some (Value (VInt (1)))),
                                  Some (Return (Some (Value (VInt (0))))))]));
          is_overriden = false }
     
  "ToString" ->
   { method_type = TString; is_abstract = false; is_virtual = true;
     is_override = false; arguments = []; key = "ToString";
     body =
     Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]));
     is_overriden = false }
  
  ]]
  ;
  constructors_table =
  [["Object" ->
     { key = "Object"; arguments = []; call_constructor = None;
       body = StatementBlock ([]) }
  
  ]]
  ; children_keys = ["Program"; "Person"]; is_abstract = false;
  is_sealed = false; parent_key = None;
  decl_tree =
  Class
  ([Public], Name ("Object"), None,
   [([Public],
     Method
     (TInt, Name ("Equals"), [(TClass ("Object"), Name ("obj"))],
      Some (StatementBlock ([If
                             (Equal (This, Identifier ("obj")),
                              Return (Some (Value (VInt (1)))),
                              Some (Return (Some (Value (VInt (0))))))]))));
    ([Public],
     Method
     (TString, Name ("ToString"), [],
      Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]))))])
  }
  
  "Person" ->
   { this_key = "Person";
     fields_table =
     [["weight" ->
        { field_type = TInt; key = "weight"; is_const = false; sub_tree = None
          }
     
  "age" ->
   { field_type = TInt; key = "age"; is_const = false; sub_tree = None }
  
  ]]
  ;
  methods_table =
  [["ToString" ->
     { method_type = TString; is_abstract = false; is_virtual = true;
       is_override = false; arguments = []; key = "ToString";
       body =
       Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]));
       is_overriden = false }
  
  "EqualsTClass (\"Object\")" ->
   { method_type = TInt; is_abstract = false; is_virtual = true;
     is_override = false; arguments = [(TClass ("Object"), Name ("obj"))];
     key = "EqualsTClass (\"Object\")";
     body =
     Some (StatementBlock ([If
                            (Equal (This, Identifier ("obj")),
                             Return (Some (Value (VInt (1)))),
                             Some (Return (Some (Value (VInt (0))))))]));
     is_overriden = false }
  
  "SetAgeTInt" ->
   { method_type = TVoid; is_abstract = false; is_virtual = false;
     is_override = false; arguments = [(TInt, Name ("age"))];
     key = "SetAgeTInt";
     body =
     Some (StatementBlock ([Expression (Assign
                                        (AccessByPoint
                                         (This, Identifier ("age")),
                                         Identifier ("age")))]));
     is_overriden = false }
  
  "GetAge" ->
   { method_type = TInt; is_abstract = false; is_virtual = true;
     is_override = false; arguments = []; key = "GetAge";
     body = Some (StatementBlock ([Return (Some (Identifier ("age")))]));
     is_overriden = false }
  
  "SetWeightTInt" ->
   { method_type = TVoid; is_abstract = false; is_virtual = false;
     is_override = false; arguments = [(TInt, Name ("weight"))];
     key = "SetWeightTInt";
     body =
     Some (StatementBlock ([Expression (Assign
                                        (AccessByPoint
                                         (This, Identifier ("weight")),
                                         Identifier ("weight")))]));
     is_overriden = false }
  
  "GetWeight" ->
   { method_type = TInt; is_abstract = false; is_virtual = false;
     is_override = false; arguments = []; key = "GetWeight";
     body = Some (StatementBlock ([Return (Some (Identifier ("weight")))]));
     is_overriden = false }
  
  ]]
  ;
  constructors_table =
  [["PersonTIntTInt" ->
     { key = "PersonTIntTInt";
       arguments = [(TInt, Name ("weight")); (TInt, Name ("age"))];
       call_constructor = None;
       body =
       StatementBlock ([Expression (Assign
                                    (AccessByPoint
                                     (This, Identifier ("weight")),
                                     Identifier ("weight")));
                        Expression (Assign
                                    (AccessByPoint (This, Identifier ("age")),
                                     Identifier ("age")))])
       }
  
  ]]
  ; children_keys = ["Child"]; is_abstract = false; is_sealed = false;
  parent_key = Some ("Object");
  decl_tree =
  Class
  ([Public], Name ("Person"), None,
   [([Public], Field (TInt, [(Name ("weight"), None)]));
    ([Public], Field (TInt, [(Name ("age"), None)]));
    ([Public],
     Constructor
     (Name ("Person"), [(TInt, Name ("weight")); (TInt, Name ("age"))], 
      None,
      StatementBlock ([Expression (Assign
                                   (AccessByPoint (This, Identifier ("weight")),
                                    Identifier ("weight")));
                       Expression (Assign
                                   (AccessByPoint (This, Identifier ("age")),
                                    Identifier ("age")))])));
    ([Public],
     Method
     (TInt, Name ("GetWeight"), [],
      Some (StatementBlock ([Return (Some (Identifier ("weight")))]))));
    ([Public],
     Method
     (TVoid, Name ("SetWeight"), [(TInt, Name ("weight"))],
      Some (StatementBlock ([Expression (Assign
                                         (AccessByPoint
                                          (This, Identifier ("weight")),
                                          Identifier ("weight")))]))));
    ([Public; Virtual],
     Method
     (TInt, Name ("GetAge"), [],
      Some (StatementBlock ([Return (Some (Identifier ("age")))]))));
    ([Public],
     Method
     (TVoid, Name ("SetAge"), [(TInt, Name ("age"))],
      Some (StatementBlock ([Expression (Assign
                                         (AccessByPoint
                                          (This, Identifier ("age")),
                                          Identifier ("age")))]))))])
  }
  
  -_-_-_-_-_-_-_-_-_-_- Wrong modifiers testing -_-_-_-_-_-_-_-_-_-_-
  
  Methods cannot be const
  Wrong class modifiers
  Wrong constructor modifiers
  -_-_-_-_-_-_-_-_-_-_- Similar fields error testing -_-_-_-_-_-_-_-_-_-_-
  
  Similar fields
  -_-_-_-_-_-_-_-_-_-_- Similar methods error testing -_-_-_-_-_-_-_-_-_-_-
  
  Method with this type exists
  -_-_-_-_-_-_-_-_-_-_- Similar constructors error testing -_-_-_-_-_-_-_-_-_-_-
  
  Constructor with this type exists
  -_-_-_-_-_-_-_-_-_-_- Abstract errors testing -_-_-_-_-_-_-_-_-_-_-
  
  Abstract method in non-abstract class
  Body missing in non-abstract method
  Abstract method cannot have body
  Abstract method must be overriden
  -_-_-_-_-_-_-_-_-_-_- Sealed class errors testing -_-_-_-_-_-_-_-_-_-_-
  
  Virtual method cannot be in sealed class
  -_-_-_-_-_-_-_-_-_-_- Override errors testing -_-_-_-_-_-_-_-_-_-_-
  
  Cannot override non-existent method in parent
  Cannot override non-virtual or non-abstract method in parent
  ]]
