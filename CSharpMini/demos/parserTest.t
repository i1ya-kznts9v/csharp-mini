  $ (cd ../../../../default && demos/parserTest.exe)
  Ast.Class
  ([Ast.Public], Ast.Name ("Program"), None,
   [([Ast.Public; Ast.Static],
     Ast.Method
     (Ast.TVoid, Ast.Name ("Main"),
      [(Ast.TArray (Ast.TString), Ast.Name ("args"))],
      Some (Ast.StatementBlock ([Ast.VariableDecl
                                 (Ast.TClass ("Person"),
                                  [(Ast.Name ("person"),
                                    Some (Ast.ClassCreation
                                          (Ast.Name ("Person"),
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
                                  [(Ast.Name ("child"),
                                    Some (Ast.ClassCreation
                                          (Ast.Name ("Child"),
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
                                                   [Ast.Value (Ast.VInt (333))])))]))))])
  Ast.Class
  ([Ast.Public], Ast.Name ("Person"), None,
   [([Ast.Public], Ast.Field (Ast.TInt, [(Ast.Name ("weight"), None)]));
    ([Ast.Public], Ast.Field (Ast.TInt, [(Ast.Name ("age"), None)]));
    ([Ast.Public],
     Ast.Constructor
     (Ast.Name ("Person"),
      [(Ast.TInt, Ast.Name ("weight")); (Ast.TInt, Ast.Name ("age"))], 
      None,
      Ast.StatementBlock ([Ast.Expression (Ast.Assign
                                           (Ast.AccessByPoint
                                            (Ast.This,
                                             Ast.Identifier ("weight")),
                                            Ast.Identifier ("weight")));
                           Ast.Expression (Ast.Assign
                                           (Ast.AccessByPoint
                                            (Ast.This, Ast.Identifier ("age")),
                                            Ast.Identifier ("age")))])));
    ([Ast.Public],
     Ast.Method
     (Ast.TInt, Ast.Name ("GetWeight"), [],
      Some (Ast.StatementBlock ([Ast.Return (Some (Ast.Identifier ("weight")))]))));
    ([Ast.Public],
     Ast.Method
     (Ast.TVoid, Ast.Name ("SetWeight"), [(Ast.TInt, Ast.Name ("weight"))],
      Some (Ast.StatementBlock ([Ast.Expression (Ast.Assign
                                                 (Ast.AccessByPoint
                                                  (Ast.This,
                                                   Ast.Identifier ("weight")),
                                                  Ast.Identifier ("weight")))]))));
    ([Ast.Public],
     Ast.Method
     (Ast.TInt, Ast.Name ("GetAge"), [],
      Some (Ast.StatementBlock ([Ast.Return (Some (Ast.Identifier ("age")))]))));
    ([Ast.Public],
     Ast.Method
     (Ast.TVoid, Ast.Name ("SetAge"), [(Ast.TInt, Ast.Name ("age"))],
      Some (Ast.StatementBlock ([Ast.Expression (Ast.Assign
                                                 (Ast.AccessByPoint
                                                  (Ast.This,
                                                   Ast.Identifier ("age")),
                                                  Ast.Identifier ("age")))]))))])
  Ast.Class
  ([Ast.Public], Ast.Name ("Child"), Some (Ast.Name ("Person")),
   [([Ast.Public], Ast.Field (Ast.TInt, [(Ast.Name ("cash"), None)]));
    ([Ast.Public],
     Ast.Constructor
     (Ast.Name ("Child"),
      [(Ast.TInt, Ast.Name ("weight")); (Ast.TInt, Ast.Name ("age"))],
      Some (Ast.CallMethod
            (Ast.Base, [Ast.Identifier ("weight"); Ast.Identifier ("age")])),
      Ast.StatementBlock ([Ast.Expression (Ast.Assign
                                           (Ast.Identifier ("cash"),
                                            Ast.Value (Ast.VInt (0))))])));
    ([Ast.Public],
     Ast.Constructor
     (Ast.Name ("Child"),
      [(Ast.TInt, Ast.Name ("weight")); (Ast.TInt, Ast.Name ("age"));
       (Ast.TInt, Ast.Name ("cash"))],
      Some (Ast.CallMethod
            (Ast.This, [Ast.Identifier ("weight"); Ast.Identifier ("age")])),
      Ast.StatementBlock ([Ast.Expression (Ast.Assign
                                           (Ast.AccessByPoint
                                            (Ast.This, Ast.Identifier ("cash")),
                                            Ast.Identifier ("cash")))])));
    ([Ast.Public],
     Ast.Method
     (Ast.TInt, Ast.Name ("GetCash"), [],
      Some (Ast.StatementBlock ([Ast.Return (Some (Ast.Identifier ("cash")))]))));
    ([Ast.Public],
     Ast.Method
     (Ast.TVoid, Ast.Name ("SetCash"), [(Ast.TInt, Ast.Name ("cash"))],
      Some (Ast.StatementBlock ([Ast.Expression (Ast.Assign
                                                 (Ast.AccessByPoint
                                                  (Ast.This,
                                                   Ast.Identifier ("cash")),
                                                  Ast.Identifier ("cash")))]))));
    ([Ast.Public],
     Ast.Method
     (Ast.TVoid, Ast.Name ("TellEvenNumbers"),
      [(Ast.TInt, Ast.Name ("count"))],
      Some (Ast.StatementBlock ([Ast.For
                                 (Some (Ast.VariableDecl
                                        (Ast.TInt,
                                         [(Ast.Name ("i"),
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
                                                               [Ast.Continue])))]))]))))])
