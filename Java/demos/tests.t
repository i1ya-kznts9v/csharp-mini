  $ (cd ../../../../default && demos/demoFirst.exe)
  Ast.Class
  ([Ast.Public], Ast.Name ("Main"), None,
   [Ast.Method
    ([Ast.Public; Ast.Static], Ast.Void, Ast.Name ("main"),
     [(Ast.Array (Ast.String), Ast.Name ("args"))],
     Some (Ast.StmtBlock ([Ast.VarDec
                           (Ast.ClassName ("Person"),
                            [(Ast.Name ("p"),
                              Some (Ast.ClassCreate
                                    (Ast.Name ("Person"),
                                     [Ast.Const (Ast.VInt (80));
                                      Ast.Const (Ast.VInt (45))])))]);
                           Ast.Expression (Ast.FieldAccess
                                           (Ast.FieldAccess
                                            (Ast.Identifier ("System"),
                                             Ast.Identifier ("out")),
                                            Ast.CallMethod
                                            (Ast.Identifier ("println"),
                                             [Ast.FieldAccess
                                              (Ast.Identifier ("p"),
                                               Ast.CallMethod
                                               (Ast.Identifier ("getWeight"),
                                                []))])));
                           Ast.VarDec
                           (Ast.ClassName ("Child"),
                            [(Ast.Name ("ch"),
                              Some (Ast.ClassCreate
                                    (Ast.Name ("Child"),
                                     [Ast.Const (Ast.VInt (66));
                                      Ast.Const (Ast.VInt (20))])))]);
                           Ast.Expression (Ast.FieldAccess
                                           (Ast.Identifier ("ch"),
                                            Ast.CallMethod
                                            (Ast.Identifier ("setCash"),
                                             [Ast.Const (Ast.VInt (50))])));
                           Ast.Expression (Ast.FieldAccess
                                           (Ast.Identifier ("ch"),
                                            Ast.CallMethod
                                            (Ast.Identifier ("giveEvenNumbers100"),
                                             [])))])))])
  Ast.Class
  ([], Ast.Name ("Person"), None,
   [Ast.VarField ([Ast.Public], Ast.Int, [(Ast.Name ("weight"), None)]);
    Ast.VarField ([Ast.Public], Ast.Int, [(Ast.Name ("age"), None)]);
    Ast.Constructor
    ([Ast.Public], Ast.Name ("Person"),
     [(Ast.Int, Ast.Name ("w")); (Ast.Int, Ast.Name ("a"))],
     Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                     (Ast.FieldAccess
                                      (Ast.This, Ast.Identifier ("weight")),
                                      Ast.Identifier ("w")));
                     Ast.Expression (Ast.Assign
                                     (Ast.FieldAccess
                                      (Ast.This, Ast.Identifier ("age")),
                                      Ast.Identifier ("a")))]));
    Ast.Method
    ([Ast.Public], Ast.Int, Ast.Name ("getWeight"), [],
     Some (Ast.StmtBlock ([Ast.Return (Some (Ast.Identifier ("weight")))])));
    Ast.Method
    ([Ast.Public], Ast.Int, Ast.Name ("getAge"), [],
     Some (Ast.StmtBlock ([Ast.Return (Some (Ast.Identifier ("age")))])));
    Ast.Method
    ([Ast.Public], Ast.Void, Ast.Name ("setWeight"),
     [(Ast.Int, Ast.Name ("w"))],
     Some (Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                           (Ast.FieldAccess
                                            (Ast.This,
                                             Ast.Identifier ("weight")),
                                            Ast.Identifier ("w")))])));
    Ast.Method
    ([Ast.Public], Ast.Void, Ast.Name ("setAge"), [(Ast.Int, Ast.Name ("a"))],
     Some (Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                           (Ast.FieldAccess
                                            (Ast.This, Ast.Identifier ("age")),
                                            Ast.Identifier ("a")))])))])
  Ast.Class
  ([], Ast.Name ("Child"), Some (Ast.Name ("Person")),
   [Ast.VarField ([Ast.Public], Ast.Int, [(Ast.Name ("cash"), None)]);
    Ast.Constructor
    ([Ast.Public], Ast.Name ("Child"),
     [(Ast.Int, Ast.Name ("w")); (Ast.Int, Ast.Name ("a"))],
     Ast.StmtBlock ([Ast.Expression (Ast.CallMethod
                                     (Ast.Super,
                                      [Ast.Identifier ("w");
                                       Ast.Identifier ("a")]));
                     Ast.Expression (Ast.Assign
                                     (Ast.Identifier ("cash"),
                                      Ast.Const (Ast.VInt (0))))]));
    Ast.Method
    ([Ast.Public], Ast.Int, Ast.Name ("getCash"), [],
     Some (Ast.StmtBlock ([Ast.Return (Some (Ast.Identifier ("cash")))])));
    Ast.Method
    ([Ast.Public], Ast.Void, Ast.Name ("setCash"), [(Ast.Int, Ast.Name ("c"))],
     Some (Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                           (Ast.FieldAccess
                                            (Ast.This, Ast.Identifier ("cash")),
                                            Ast.Identifier ("c")))])));
    Ast.Constructor
    ([Ast.Public], Ast.Name ("Child"),
     [(Ast.Int, Ast.Name ("w")); (Ast.Int, Ast.Name ("a"));
      (Ast.Int, Ast.Name ("c"))],
     Ast.StmtBlock ([Ast.Expression (Ast.CallMethod
                                     (Ast.Super,
                                      [Ast.Identifier ("w");
                                       Ast.Identifier ("a")]));
                     Ast.Expression (Ast.Assign
                                     (Ast.Identifier ("cash"),
                                      Ast.Identifier ("c")))]));
    Ast.Method
    ([Ast.Public], Ast.Void, Ast.Name ("giveEvenNumbers100"), [],
     Some (Ast.StmtBlock ([Ast.For
                           (Some (Ast.VarDec
                                  (Ast.Int,
                                   [(Ast.Name ("i"),
                                     Some (Ast.Const (Ast.VInt (0))))])),
                            Some (Ast.Less
                                  (Ast.Identifier ("i"),
                                   Ast.Const (Ast.VInt (100)))),
                            [Ast.PostInc (Ast.Identifier ("i"))],
                            Ast.StmtBlock ([Ast.If
                                            (Ast.And
                                             (Ast.Equal
                                              (Ast.Mod
                                               (Ast.Identifier ("i"),
                                                Ast.Const (Ast.VInt (2))),
                                               Ast.Const (Ast.VInt (0))),
                                              Ast.Not (Ast.Equal
                                                       (Ast.Mod
                                                        (Ast.Identifier ("i"),
                                                         Ast.Const (Ast.VInt (2))),
                                                        Ast.Const (Ast.VInt (1))))),
                                             Ast.StmtBlock ([Ast.Expression (
                                                              Ast.FieldAccess
                                                              (Ast.FieldAccess
                                                               (Ast.Identifier ("System"),
                                                                Ast.Identifier ("out")),
                                                               Ast.CallMethod
                                                               (Ast.Identifier ("println"),
                                                                [Ast.Identifier ("i")])))]),
                                             Some (Ast.StmtBlock ([Ast.Continue])))]))])))])
 
