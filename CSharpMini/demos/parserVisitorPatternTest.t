  $ (cd ../../../../default && demos/parserVisitorPatternTest.exe)
  Class
  ([Public], Name ("Program"), None,
   [([Public; Static],
     Method
     (TVoid, Name ("Main"), [],
      Some (StatementBlock ([VariableDecl
                             (None, TArray (TClass ("Figure")),
                              [(Name ("figures"),
                                Some (ArrayCreationWithElements
                                      (TClass ("Figure"),
                                       [ClassCreation
                                        (Name ("Circle"), [Value (VInt (5))]);
                                        ClassCreation
                                        (Name ("Rectangle"),
                                         [Value (VInt (2)); Value (VInt (4))]);
                                        ClassCreation (Name ("Triangle"), [])])))]);
                             VariableDecl
                             (None, TClass ("VisitorArea"),
                              [(Name ("visitorArea"),
                                Some (ClassCreation (Name ("VisitorArea"), [])))]);
                             VariableDecl
                             (None, TClass ("VisitorPerimeter"),
                              [(Name ("visitorPerimeter"),
                                Some (ClassCreation
                                      (Name ("VisitorPerimeter"), [])))]);
                             For
                             (Some (VariableDecl
                                    (None, TInt,
                                     [(Name ("i"), Some (Value (VInt (0))))])),
                              Some (Less
                                    (Identifier ("i"),
                                     AccessByPoint
                                     (Identifier ("figures"),
                                      Identifier ("length")))),
                              [PostInc (Identifier ("i"))],
                              StatementBlock ([Expression (AccessByPoint
                                                           (Identifier ("Console"),
                                                            CallMethod
                                                            (Identifier ("WriteLine"),
                                                             [AccessByPoint
                                                              (ArrayAccess
                                                               (Identifier ("figures"),
                                                                Identifier ("i")),
                                                               CallMethod
                                                               (Identifier ("Accept"),
                                                                [Identifier ("visitorArea")]))])))]));
                             For
                             (Some (VariableDecl
                                    (None, TInt,
                                     [(Name ("j"), Some (Value (VInt (0))))])),
                              Some (Less
                                    (Identifier ("j"),
                                     AccessByPoint
                                     (Identifier ("figures"),
                                      Identifier ("length")))),
                              [PostInc (Identifier ("j"))],
                              StatementBlock ([Expression (AccessByPoint
                                                           (Identifier ("Console"),
                                                            CallMethod
                                                            (Identifier ("WriteLine"),
                                                             [AccessByPoint
                                                              (ArrayAccess
                                                               (Identifier ("figures"),
                                                                Identifier ("j")),
                                                               CallMethod
                                                               (Identifier ("Accept"),
                                                                [Identifier ("visitorPerimeter")]))])))]))]))))])
  Class
  ([Public; Abstract], Name ("Figure"), None,
   [([Abstract],
     Method
     (TInt, Name ("Accept"), [(TClass ("Visitor"), Name ("visitor"))], None))])
  Class
  ([Public], Name ("Circle"), Some (Name ("Figure")),
   [([Public], Field (TInt, [(Name ("radius"), None)]));
    ([Public],
     Constructor
     (Name ("Circle"), [(TInt, Name ("radius"))], None,
      StatementBlock ([Expression (Assign
                                   (AccessByPoint (This, Identifier ("radius")),
                                    Identifier ("radius")))])));
    ([Public],
     Constructor
     (Name ("Circle"), [], None,
      StatementBlock ([Expression (Assign
                                   (AccessByPoint (This, Identifier ("radius")),
                                    Value (VInt (1))))])));
    ([Public; Override],
     Method
     (TInt, Name ("Accept"), [(TClass ("Visitor"), Name ("visitor"))],
      Some (StatementBlock ([Return (Some (AccessByPoint
                                           (Identifier ("visitor"),
                                            CallMethod
                                            (Identifier ("visit"), [This]))))]))))])
  Class
  ([Public], Name ("Triangle"), Some (Name ("Figure")),
   [([Public],
     Field (TInt, [(Name ("a"), None); (Name ("b"), None); (Name ("c"), None)]));
    ([Public],
     Constructor
     (Name ("Triangle"),
      [(TInt, Name ("a")); (TInt, Name ("b")); (TInt, Name ("c"))], None,
      StatementBlock ([Expression (Assign
                                   (AccessByPoint (This, Identifier ("a")),
                                    Identifier ("a")));
                       Expression (Assign
                                   (AccessByPoint (This, Identifier ("b")),
                                    Identifier ("b")));
                       Expression (Assign
                                   (AccessByPoint (This, Identifier ("c")),
                                    Identifier ("c")))])));
    ([Public],
     Constructor
     (Name ("Triangle"), [], None,
      StatementBlock ([Expression (Assign
                                   (AccessByPoint (This, Identifier ("a")),
                                    Value (VInt (1))));
                       Expression (Assign
                                   (AccessByPoint (This, Identifier ("b")),
                                    Value (VInt (1))));
                       Expression (Assign
                                   (AccessByPoint (This, Identifier ("c")),
                                    Value (VInt (1))))])));
    ([Public; Override],
     Method
     (TInt, Name ("Accept"), [(TClass ("Visitor"), Name ("visitor"))],
      Some (StatementBlock ([Return (Some (AccessByPoint
                                           (Identifier ("visitor"),
                                            CallMethod
                                            (Identifier ("visit"), [This]))))]))))])
  Class
  ([Public], Name ("Rectangle"), Some (Name ("Figure")),
   [([Public], Field (TInt, [(Name ("a"), None); (Name ("b"), None)]));
    ([Public],
     Constructor
     (Name ("Rectangle"), [(TInt, Name ("a")); (TInt, Name ("b"))], None,
      StatementBlock ([Expression (Assign
                                   (AccessByPoint (This, Identifier ("a")),
                                    Identifier ("a")));
                       Expression (Assign
                                   (AccessByPoint (This, Identifier ("b")),
                                    Identifier ("b")))])));
    ([Public],
     Constructor
     (Name ("Rectangle"), [], None,
      StatementBlock ([Expression (Assign
                                   (AccessByPoint (This, Identifier ("a")),
                                    Value (VInt (1))));
                       Expression (Assign
                                   (AccessByPoint (This, Identifier ("b")),
                                    Value (VInt (1))))])));
    ([Public; Override],
     Method
     (TInt, Name ("Accept"), [(TClass ("Visitor"), Name ("visitor"))],
      Some (StatementBlock ([Return (Some (AccessByPoint
                                           (Identifier ("visitor"),
                                            CallMethod
                                            (Identifier ("visit"), [This]))))]))))])
  Class
  ([Public; Abstract], Name ("Visitor"), None,
   [([Abstract],
     Method
     (TInt, Name ("Visit"), [(TClass ("Circle"), Name ("circle"))], None));
    ([Abstract],
     Method
     (TInt, Name ("Visit"), [(TClass ("Rectangle"), Name ("rectangle"))], None));
    ([Abstract],
     Method
     (TInt, Name ("Visit"), [(TClass ("Triangle"), Name ("triangle"))], None))])
  Class
  ([Public], Name ("VisitorArea"), Some (Name ("Visitor")),
   [([Public; Override],
     Method
     (TInt, Name ("Visit"), [(TClass ("Circle"), Name ("circle"))],
      Some (StatementBlock ([Return (Some (Mult
                                           (Mult
                                            (Value (VInt (3)),
                                             AccessByPoint
                                             (Identifier ("circle"),
                                              Identifier ("radius"))),
                                            AccessByPoint
                                            (Identifier ("circle"),
                                             Identifier ("radius")))))]))));
    ([Public; Override],
     Method
     (TInt, Name ("Visit"), [(TClass ("Rectangle"), Name ("rectangle"))],
      Some (StatementBlock ([Return (Some (Mult
                                           (AccessByPoint
                                            (Identifier ("rectangle"),
                                             Identifier ("a")),
                                            AccessByPoint
                                            (Identifier ("rectangle"),
                                             Identifier ("b")))))]))));
    ([Public; Override],
     Method
     (TInt, Name ("Visit"), [(TClass ("Triangle"), Name ("triangle"))],
      Some (StatementBlock ([VariableDecl
                             (None, TInt,
                              [(Name ("perimeter"),
                                Some (Div
                                      (Add
                                       (Add
                                        (AccessByPoint
                                         (Identifier ("triangle"),
                                          Identifier ("a")),
                                         AccessByPoint
                                         (Identifier ("triangle"),
                                          Identifier ("b"))),
                                        AccessByPoint
                                        (Identifier ("triangle"),
                                         Identifier ("c"))),
                                       Value (VInt (2)))))]);
                             Return (Some (Mult
                                           (Mult
                                            (Mult
                                             (Identifier ("perimeter"),
                                              Sub
                                              (Identifier ("perimeter"),
                                               AccessByPoint
                                               (Identifier ("triangle"),
                                                Identifier ("a")))),
                                             Sub
                                             (Identifier ("perimeter"),
                                              AccessByPoint
                                              (Identifier ("triangle"),
                                               Identifier ("b")))),
                                            Sub
                                            (Identifier ("perimeter"),
                                             AccessByPoint
                                             (Identifier ("triangle"),
                                              Identifier ("c"))))))]))))])
  Class
  ([Public], Name ("VisitorPerimeter"), Some (Name ("Visitor")),
   [([Public; Override],
     Method
     (TInt, Name ("Visit"), [(TClass ("Circle"), Name ("circle"))],
      Some (StatementBlock ([Return (Some (Mult
                                           (Mult
                                            (Value (VInt (2)),
                                             Value (VInt (3))),
                                            AccessByPoint
                                            (Identifier ("circle"),
                                             Identifier ("radius")))))]))));
    ([Public; Override],
     Method
     (TInt, Name ("Visit"), [(TClass ("Rectangle"), Name ("rectangle"))],
      Some (StatementBlock ([Return (Some (Mult
                                           (Value (VInt (2)),
                                            Add
                                            (AccessByPoint
                                             (Identifier ("rectangle"),
                                              Identifier ("a")),
                                             AccessByPoint
                                             (Identifier ("rectangle"),
                                              Identifier ("b"))))))]))));
    ([Public; Override],
     Method
     (TInt, Name ("Visit"), [(TClass ("Triangle"), Name ("triangle"))],
      Some (StatementBlock ([Return (Some (Add
                                           (Add
                                            (AccessByPoint
                                             (Identifier ("triangle"),
                                              Identifier ("a")),
                                             AccessByPoint
                                             (Identifier ("triangle"),
                                              Identifier ("b"))),
                                            AccessByPoint
                                            (Identifier ("triangle"),
                                             Identifier ("c")))))]))))])
