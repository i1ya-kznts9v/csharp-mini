  $ (cd ../../../../default && demos/demoParser.exe) 
  Class
  ([Public], "Program", None,
   [([Static],
     Method
     (Void, "Main", [], StatementBlock ([Expression (CallMethod ("E3", []))])));
    ([Public; Static],
     Method
     (Void, "A3", [],
      StatementBlock ([Try
                       (StatementBlock ([Throw (ClassCreate
                                                ("ShittyExn",
                                                 [ConstExpr (VBool (true))]))]),
                        [], Some (StatementBlock ([])))])));
    ([Public; Static],
     Method
     (Void, "B3", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("A3", []))]),
                        [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                          Some (ConstExpr (VBool (false))),
                          StatementBlock ([Print (ConstExpr (VString ("B")))]))],
                        None)])));
    ([Public; Static],
     Method
     (Void, "C3", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("B3", []))]),
                        [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                          Some (ConstExpr (VBool (false))),
                          StatementBlock ([Print (ConstExpr (VString ("C")))]))],
                        None)])));
    ([Public; Static],
     Method
     (Void, "D3", [],
      StatementBlock ([Try
                       (StatementBlock ([Expression (CallMethod ("C3", []))]),
                        [(Some ((CsClass ("ShittyExn"), Some (IdentVar ("e")))),
                          Some (ConstExpr (VBool (false))),
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
  Class
  ([], "ShittyExn", Some ("Exception"),
   [([Public], VariableField (Bool, [("f", None)]));
    ([Public],
     Constructor
     ("ShittyExn", [(Bool, "val")],
      StatementBlock ([Expression (Assign (IdentVar ("f"), IdentVar ("val")))])));
    ([Public],
     Method
     (Bool, "Filter", [], StatementBlock ([Return (Some (IdentVar ("f")))])))])
