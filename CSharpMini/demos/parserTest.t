  $ (cd ../../../../default && demos/parserTest.exe)
  Ast.Class
  ([Ast.Public], Ast.Identifier ("Program"), None,
   [Ast.Method
    ([Ast.Public; Ast.Static], Ast.TVoid, Ast.Identifier ("Main"),
     [(Ast.TArray (Ast.TString), Ast.Identifier ("args"))],
     Some (Ast.StatementBlock ([Ast.VariableDecl
                                (Ast.TClass ("Person"),
                                 [(Ast.Identifier ("person"),
                                   Some (Ast.ClassCreation
                                         (Ast.Identifier ("Person"),
                                          [Ast.Value (Ast.VInt (100));
                                           Ast.Value (Ast.VInt (50))])))]);
                                Ast.Expression (Ast.AccessByPoint
                                                (Ast.Identifier ("person"),
                                                 Ast.CallMethod
                                                 (Ast.Identifier ("SetAge"),
                                                  [Ast.Value (Ast.VInt (45))])));
                                Ast.Expression (Ast.AccessByPoint
                                                (Ast.Identifier ("Console"),
                                                 Ast.CallMethod
                                                 (Ast.Identifier ("WriteLine"),
                                                  [Ast.AccessByPoint
                                                   (Ast.Identifier ("person"),
                                                    Ast.CallMethod
                                                    (Ast.Identifier ("GetAge"),
                                                     []))])));
                                Ast.VariableDecl
                                (Ast.TClass ("Child"),
                                 [(Ast.Identifier ("child"),
                                   Some (Ast.ClassCreation
                                         (Ast.Identifier ("Child"),
                                          [Ast.Value (Ast.VInt (50));
                                           Ast.Value (Ast.VInt (10))])))]);
                                Ast.Expression (Ast.AccessByPoint
                                                (Ast.Identifier ("child"),
                                                 Ast.CallMethod
                                                 (Ast.Identifier ("SetCash"),
                                                  [Ast.Value (Ast.VInt (1000))])));
                                Ast.Expression (Ast.AccessByPoint
                                                (Ast.Identifier ("Console"),
                                                 Ast.CallMethod
                                                 (Ast.Identifier ("WriteLine"),
                                                  [Ast.AccessByPoint
                                                   (Ast.Identifier ("child"),
                                                    Ast.CallMethod
                                                    (Ast.Identifier ("GetCash"),
                                                     []))])));
                                Ast.Expression (Ast.AccessByPoint
                                                (Ast.Identifier ("child"),
                                                 Ast.CallMethod
                                                 (Ast.Identifier ("TellEvenNumbers"),
                                                  [Ast.Value (Ast.VInt (333))])))])))])
  Ast.Class
  ([Ast.Public], Ast.Identifier ("Person"), None,
   [Ast.Field ([Ast.Public], Ast.TInt, [(Ast.Identifier ("weight"), None)]);
    Ast.Field ([Ast.Public], Ast.TInt, [(Ast.Identifier ("age"), None)]);
    Ast.Constructor
    ([Ast.Public], Ast.Identifier ("Person"),
     [(Ast.TInt, Ast.Identifier ("weight"));
      (Ast.TInt, Ast.Identifier ("age"))],
     None,
     Ast.StatementBlock ([Ast.Expression (Ast.Assign
                                          (Ast.AccessByPoint
                                           (Ast.This,
                                            Ast.Identifier ("weight")),
                                           Ast.Identifier ("weight")));
                          Ast.Expression (Ast.Assign
                                          (Ast.AccessByPoint
                                           (Ast.This, Ast.Identifier ("age")),
                                           Ast.Identifier ("age")))]));
    Ast.Method
    ([Ast.Public], Ast.TInt, Ast.Identifier ("GetWeight"), [],
     Some (Ast.StatementBlock ([Ast.Return (Some (Ast.Identifier ("weight")))])));
    Ast.Method
    ([Ast.Public], Ast.TVoid, Ast.Identifier ("SetWeight"),
     [(Ast.TInt, Ast.Identifier ("weight"))],
     Some (Ast.StatementBlock ([Ast.Expression (Ast.Assign
                                                (Ast.AccessByPoint
                                                 (Ast.This,
                                                  Ast.Identifier ("weight")),
                                                 Ast.Identifier ("weight")))])));
    Ast.Method
    ([Ast.Public], Ast.TInt, Ast.Identifier ("GetAge"), [],
     Some (Ast.StatementBlock ([Ast.Return (Some (Ast.Identifier ("age")))])));
    Ast.Method
    ([Ast.Public], Ast.TVoid, Ast.Identifier ("SetAge"),
     [(Ast.TInt, Ast.Identifier ("age"))],
     Some (Ast.StatementBlock ([Ast.Expression (Ast.Assign
                                                (Ast.AccessByPoint
                                                 (Ast.This,
                                                  Ast.Identifier ("age")),
                                                 Ast.Identifier ("age")))])))])
  Ast.Class
  ([Ast.Public], Ast.Identifier ("Child"), Some (Ast.Identifier ("Person")),
   [Ast.Field ([Ast.Public], Ast.TInt, [(Ast.Identifier ("cash"), None)]);
    Ast.Constructor
    ([Ast.Public], Ast.Identifier ("Child"),
     [(Ast.TInt, Ast.Identifier ("weight"));
      (Ast.TInt, Ast.Identifier ("age"))],
     Some (Ast.CallMethod
           (Ast.Base, [Ast.Identifier ("weight"); Ast.Identifier ("age")])),
     Ast.StatementBlock ([Ast.Expression (Ast.Assign
                                          (Ast.Identifier ("cash"),
                                           Ast.Value (Ast.VInt (0))))]));
    Ast.Constructor
    ([Ast.Public], Ast.Identifier ("Child"),
     [(Ast.TInt, Ast.Identifier ("weight"));
      (Ast.TInt, Ast.Identifier ("age")); (Ast.TInt, Ast.Identifier ("cash"))],
     Some (Ast.CallMethod
           (Ast.This, [Ast.Identifier ("weight"); Ast.Identifier ("age")])),
     Ast.StatementBlock ([Ast.Expression (Ast.Assign
                                          (Ast.AccessByPoint
                                           (Ast.This, Ast.Identifier ("cash")),
                                           Ast.Identifier ("cash")))]));
    Ast.Method
    ([Ast.Public], Ast.TInt, Ast.Identifier ("GetCash"), [],
     Some (Ast.StatementBlock ([Ast.Return (Some (Ast.Identifier ("cash")))])));
    Ast.Method
    ([Ast.Public], Ast.TVoid, Ast.Identifier ("SetCash"),
     [(Ast.TInt, Ast.Identifier ("cash"))],
     Some (Ast.StatementBlock ([Ast.Expression (Ast.Assign
                                                (Ast.AccessByPoint
                                                 (Ast.This,
                                                  Ast.Identifier ("cash")),
                                                 Ast.Identifier ("cash")))])));
    Ast.Method
    ([Ast.Public], Ast.TVoid, Ast.Identifier ("TellEvenNumbers"),
     [(Ast.TInt, Ast.Identifier ("count"))],
     Some (Ast.StatementBlock ([Ast.For
                                (Some (Ast.VariableDecl
                                       (Ast.TInt,
                                        [(Ast.Identifier ("i"),
                                          Some (Ast.Value (Ast.VInt (0))))])),
                                 Some (Ast.Less
                                       (Ast.Identifier ("i"),
                                        Ast.Identifier ("count"))),
                                 [Ast.PostInc (Ast.Identifier ("i"))],
                                 Ast.StatementBlock ([Ast.If
                                                      (Ast.And
                                                       (Ast.Equal
                                                        (Ast.Mod
                                                         (Ast.Identifier ("i"),
                                                          Ast.Value (Ast.VInt (2))),
                                                         Ast.Value (Ast.VInt (0))),
                                                        Ast.Not (Ast.Equal
                                                                 (Ast.Mod
                                                                  (Ast.Identifier ("i"),
                                                                   Ast.Value (
                                                                    Ast.VInt (2))),
                                                                  Ast.Value (
                                                                   Ast.VInt (1))))),
                                                       Ast.StatementBlock (
                                                        [Ast.Expression (
                                                          Ast.AccessByPoint
                                                          (Ast.Identifier ("Console"),
                                                           Ast.CallMethod
                                                           (Ast.Identifier ("WriteLine"),
                                                            [Ast.Identifier ("i")])))]),
                                                       Some (Ast.StatementBlock (
                                                              [Ast.Continue])))]))])))])
 
