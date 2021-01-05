  $ (cd ../../../../default && demos/classLoaderTest.exe)
  -_-_-_-_-_-_-_-_-_-_- Inheritance testing -_-_-_-_-_-_-_-_-_-_-
  
  [[Program -> { this_key : Program; fields_table : [[]]; methods_table : [[ToString -> { method_type = TString; is_abstract = false; is_virtual = true;
    is_override = false; arguments = []; key = "ToString";
    body = Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]))
    }
  equalsTClass ("Object") -> { method_type = TInt; is_abstract = false; is_virtual = true;
    is_override = false; arguments = [(TClass ("Object"), Name ("obj"))];
    key = "equalsTClass (\"Object\")";
    body =
    Some (StatementBlock ([If
                           (Equal (This, Identifier ("obj")),
                            Return (Some (Value (VInt (1)))),
                            Some (Return (Some (Value (VInt (0))))))]))
    }
  Main -> { method_type = TVoid; is_abstract = false; is_virtual = false;
    is_override = false; arguments = []; key = "Main";
    body =
    Some (StatementBlock ([VariableDecl
                           (None, TClass ("Person"),
                            [(Name ("person"),
                              Some (ClassCreation
                                    (Name ("Person"),
                                     [Value (VInt (100)); Value (VInt (50))])))])]))
    }
  ]]; constructors_table : [[Program -> { key = "Program"; arguments = []; call_constructor = None;
    body = StatementBlock ([]) }
  ]]; children_keys : ; is_abstract : false; is_sealed : false; parent_key : Object}
  Child -> { this_key : Child; fields_table : [[cash -> { field_type = TInt; key = "cash"; is_const = false; sub_tree = None }
  weight -> { field_type = TInt; key = "weight"; is_const = false; sub_tree = None }
  age -> { field_type = TInt; key = "age"; is_const = false; sub_tree = None }
  ]]; methods_table : [[ToString -> { method_type = TString; is_abstract = false; is_virtual = true;
    is_override = false; arguments = []; key = "ToString";
    body = Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]))
    }
  equalsTClass ("Object") -> { method_type = TInt; is_abstract = false; is_virtual = true;
    is_override = false; arguments = [(TClass ("Object"), Name ("obj"))];
    key = "equalsTClass (\"Object\")";
    body =
    Some (StatementBlock ([If
                           (Equal (This, Identifier ("obj")),
                            Return (Some (Value (VInt (1)))),
                            Some (Return (Some (Value (VInt (0))))))]))
    }
  SetAgeTInt -> { method_type = TVoid; is_abstract = false; is_virtual = false;
    is_override = false; arguments = [(TInt, Name ("age"))];
    key = "SetAgeTInt";
    body =
    Some (StatementBlock ([Expression (Assign
                                       (AccessByPoint
                                        (This, Identifier ("age")),
                                        Identifier ("age")))]))
    }
  SetCashTInt -> { method_type = TVoid; is_abstract = false; is_virtual = false;
    is_override = false; arguments = [(TInt, Name ("cash"))];
    key = "SetCashTInt";
    body =
    Some (StatementBlock ([Expression (Assign
                                       (AccessByPoint
                                        (This, Identifier ("cash")),
                                        Identifier ("cash")))]))
    }
  GetAge -> { method_type = TInt; is_abstract = false; is_virtual = false;
    is_override = false; arguments = []; key = "GetAge";
    body = Some (StatementBlock ([Return (Some (Identifier ("age")))])) }
  SetWeightTInt -> { method_type = TVoid; is_abstract = false; is_virtual = false;
    is_override = false; arguments = [(TInt, Name ("weight"))];
    key = "SetWeightTInt";
    body =
    Some (StatementBlock ([Expression (Assign
                                       (AccessByPoint
                                        (This, Identifier ("weight")),
                                        Identifier ("weight")))]))
    }
  GetWeight -> { method_type = TInt; is_abstract = false; is_virtual = false;
    is_override = false; arguments = []; key = "GetWeight";
    body = Some (StatementBlock ([Return (Some (Identifier ("weight")))])) }
  GetCash -> { method_type = TInt; is_abstract = false; is_virtual = false;
    is_override = false; arguments = []; key = "GetCash";
    body = Some (StatementBlock ([Return (Some (Identifier ("cash")))])) }
  TellEvenNumbersTInt -> { method_type = TVoid; is_abstract = false; is_virtual = false;
    is_override = false; arguments = [(TInt, Name ("count"))];
    key = "TellEvenNumbersTInt";
    body =
    Some (StatementBlock ([For
                           (Some (VariableDecl
                                  (None, TInt,
                                   [(Name ("i"), Some (Value (VInt (0))))])),
                            Some (Less (Identifier ("i"), Identifier ("count"))),
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
                                              Some (StatementBlock ([Continue])))]))]))
    }
  ]]; constructors_table : [[ChildTIntTInt -> { key = "ChildTIntTInt";
    arguments = [(TInt, Name ("weight")); (TInt, Name ("age"))];
    call_constructor =
    Some (CallMethod (Base, [Identifier ("weight"); Identifier ("age")]));
    body =
    StatementBlock ([Expression (Assign (Identifier ("cash"), Value (VInt (0))))])
    }
  ChildTIntTIntTInt -> { key = "ChildTIntTIntTInt";
    arguments =
    [(TInt, Name ("weight")); (TInt, Name ("age")); (TInt, Name ("cash"))];
    call_constructor =
    Some (CallMethod (Base, [Identifier ("weight"); Identifier ("age")]));
    body =
    StatementBlock ([Expression (Assign
                                 (AccessByPoint (This, Identifier ("cash")),
                                  Identifier ("cash")))])
    }
  ]]; children_keys : ; is_abstract : false; is_sealed : false; parent_key : Person}
  Person -> { this_key : Person; fields_table : [[weight -> { field_type = TInt; key = "weight"; is_const = false; sub_tree = None }
  age -> { field_type = TInt; key = "age"; is_const = false; sub_tree = None }
  ]]; methods_table : [[ToString -> { method_type = TString; is_abstract = false; is_virtual = true;
    is_override = false; arguments = []; key = "ToString";
    body = Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]))
    }
  equalsTClass ("Object") -> { method_type = TInt; is_abstract = false; is_virtual = true;
    is_override = false; arguments = [(TClass ("Object"), Name ("obj"))];
    key = "equalsTClass (\"Object\")";
    body =
    Some (StatementBlock ([If
                           (Equal (This, Identifier ("obj")),
                            Return (Some (Value (VInt (1)))),
                            Some (Return (Some (Value (VInt (0))))))]))
    }
  SetAgeTInt -> { method_type = TVoid; is_abstract = false; is_virtual = false;
    is_override = false; arguments = [(TInt, Name ("age"))];
    key = "SetAgeTInt";
    body =
    Some (StatementBlock ([Expression (Assign
                                       (AccessByPoint
                                        (This, Identifier ("age")),
                                        Identifier ("age")))]))
    }
  GetAge -> { method_type = TInt; is_abstract = false; is_virtual = false;
    is_override = false; arguments = []; key = "GetAge";
    body = Some (StatementBlock ([Return (Some (Identifier ("age")))])) }
  SetWeightTInt -> { method_type = TVoid; is_abstract = false; is_virtual = false;
    is_override = false; arguments = [(TInt, Name ("weight"))];
    key = "SetWeightTInt";
    body =
    Some (StatementBlock ([Expression (Assign
                                       (AccessByPoint
                                        (This, Identifier ("weight")),
                                        Identifier ("weight")))]))
    }
  GetWeight -> { method_type = TInt; is_abstract = false; is_virtual = false;
    is_override = false; arguments = []; key = "GetWeight";
    body = Some (StatementBlock ([Return (Some (Identifier ("weight")))])) }
  ]]; constructors_table : [[PersonTIntTInt -> { key = "PersonTIntTInt";
    arguments = [(TInt, Name ("weight")); (TInt, Name ("age"))];
    call_constructor = None;
    body =
    StatementBlock ([Expression (Assign
                                 (AccessByPoint (This, Identifier ("weight")),
                                  Identifier ("weight")));
                     Expression (Assign
                                 (AccessByPoint (This, Identifier ("age")),
                                  Identifier ("age")))])
    }
  ]]; children_keys : Child ; is_abstract : false; is_sealed : false; parent_key : Object}
  Object -> { this_key : Object; fields_table : [[]]; methods_table : [[equalsTClass ("Object") -> { method_type = TInt; is_abstract = false; is_virtual = true;
    is_override = false; arguments = [(TClass ("Object"), Name ("obj"))];
    key = "equalsTClass (\"Object\")";
    body =
    Some (StatementBlock ([If
                           (Equal (This, Identifier ("obj")),
                            Return (Some (Value (VInt (1)))),
                            Some (Return (Some (Value (VInt (0))))))]))
    }
  ToString -> { method_type = TString; is_abstract = false; is_virtual = true;
    is_override = false; arguments = []; key = "ToString";
    body = Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]))
    }
  ]]; constructors_table : [[Object -> { key = "Object"; arguments = []; call_constructor = None;
    body = StatementBlock ([]) }
  ]]; children_keys : Person Program ; is_abstract : false; is_sealed : false; parent_key : None}
  ]]
  -_-_-_-_-_-_-_-_-_-_- Wrong modifiers testing -_-_-_-_-_-_-_-_-_-_-
  
  Methods cannot be const
  Wrong class modifiers
  Wrong constructor modifiers
  -_-_-_-_-_-_-_-_-_-_- Similar fields error testing -_-_-_-_-_-_-_-_-_-_-
  
  Similar fields
  -_-_-_-_-_-_-_-_-_-_- Similar methods error esting -_-_-_-_-_-_-_-_-_-_-
  
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
  Sealed or static class cannot be inherited
  -_-_-_-_-_-_-_-_-_-_- Override errors testing -_-_-_-_-_-_-_-_-_-_-
  
  Cannot override non-existent method in parent
  Cannot override non-virtual or non-abstract method in parent
