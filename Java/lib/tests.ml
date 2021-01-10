open Parser
open Parser.Stmt
open Parser.Expr
open Interpreter

open Interpreter.ClassLoader (Result)

(* -------------------  EXPRESSIONS ------------------- *)

let%test _ =
  apply expression "a = 2" = Some (Assign (Identifier "a", Const (VInt 2)))

let%test _ =
  apply expression "a[i] = 2"
  = Some (Assign (ArrayAccess (Identifier "a", Identifier "i"), Const (VInt 2)))

let%test _ =
  apply expression "a = b = 3"
  = Some (Assign (Identifier "a", Assign (Identifier "b", Const (VInt 3))))

let%test _ =
  apply expression "1 + 2" = Some (Add (Const (VInt 1), Const (VInt 2)))

let%test _ =
  apply expression "1 + 2 * 3 / 4 % 5 - 6"
  = Some
      (Sub
         ( Add
             ( Const (VInt 1),
               Mod
                 ( Div (Mult (Const (VInt 2), Const (VInt 3)), Const (VInt 4)),
                   Const (VInt 5) ) ),
           Const (VInt 6) ))

let%test _ =
  apply expression "(x + y <= 10) && (a % 2 == 0) || !(c / 2 > 3)"
  = Some
      (Or
         ( And
             ( LessOrEqual
                 (Add (Identifier "x", Identifier "y"), Const (VInt 10)),
               Equal (Mod (Identifier "a", Const (VInt 2)), Const (VInt 0)) ),
           Not (More (Div (Identifier "c", Const (VInt 2)), Const (VInt 3))) ))

let%test _ =
  apply expression "2 + 3 * (5 - 3)"
  = Some
      (Add
         ( Const (VInt 2),
           Mult (Const (VInt 3), Sub (Const (VInt 5), Const (VInt 3))) ))

let%test _ =
  apply expression "a[i]" = Some (ArrayAccess (Identifier "a", Identifier "i"))

let%test _ =
  apply expression "someObj.method(arg1, arg2, arg3)"
  = Some
      (FieldAccess
         ( Identifier "someObj",
           CallMethod
             ( Identifier "method",
               [ Identifier "arg1"; Identifier "arg2"; Identifier "arg3" ] ) ))

let%test _ =
  apply expression "arr[i].get()"
  = Some
      (FieldAccess
         ( ArrayAccess (Identifier "arr", Identifier "i"),
           CallMethod (Identifier "get", []) ))

let%test _ =
  apply expression "a[i].b[j]"
  = Some
      (ArrayAccess
         ( FieldAccess
             (ArrayAccess (Identifier "a", Identifier "i"), Identifier "b"),
           Identifier "j" ))

let%test _ =
  apply expression "this.getArray()[i]"
  = Some
      (ArrayAccess
         ( FieldAccess (This, CallMethod (Identifier "getArray", [])),
           Identifier "i" ))

let%test _ =
  apply expression "this.getCar().wheels[2].rad"
  = Some
      (FieldAccess
         ( ArrayAccess
             ( FieldAccess
                 ( FieldAccess (This, CallMethod (Identifier "getCar", [])),
                   Identifier "wheels" ),
               Const (VInt 2) ),
           Identifier "rad" ))

let%test _ =
  apply expression "a.b[i].c[j]"
  = Some
      (ArrayAccess
         ( FieldAccess
             ( ArrayAccess
                 (FieldAccess (Identifier "a", Identifier "b"), Identifier "i"),
               Identifier "c" ),
           Identifier "j" ))

let%test _ =
  apply expression "a.b.c"
  = Some
      (FieldAccess (FieldAccess (Identifier "a", Identifier "b"), Identifier "c"))

let%test _ =
  apply expression "call(1 + 2, 40)"
  = Some
      (CallMethod
         ( Identifier "call",
           [ Add (Const (VInt 1), Const (VInt 2)); Const (VInt 40) ] ))

let%test _ = apply expression "new int[]" = None

let%test _ =
  apply expression "new int[] {1, 2, 7*8, var}"
  = Some
      (ArrayCreateElements
         ( Int,
           [
             Const (VInt 1);
             Const (VInt 2);
             Mult (Const (VInt 7), Const (VInt 8));
             Identifier "var";
           ] ))

let%test _ =
  apply expression "new int[4]" = Some (ArrayCreateSized (Int, Const (VInt 4)))

let%test _ =
  apply expression "new arr[i]"
  = Some (ArrayCreateSized (ClassName "arr", Identifier "i"))

let%test _ =
  apply expression "new Car(2,\"Ford\")"
  = Some (ClassCreate (Name "Car", [ Const (VInt 2); Const (VString "Ford") ]))

let%test _ =
  apply expression "get(new Sth(), new char[10])"
  = Some
      (CallMethod
         ( Identifier "get",
           [
             ClassCreate (Name "Sth", []);
             ArrayCreateSized (Char, Const (VInt 10));
           ] ))

let%test _ =
  apply expression "(new Man(3,\"John\")).scream())"
  = Some
      (FieldAccess
         ( ClassCreate (Name "Man", [ Const (VInt 3); Const (VString "John") ]),
           CallMethod (Identifier "scream", []) ))

let%test _ =
  apply expression "obj.f++ + --x"
  = Some
      (Add
         ( PostInc (FieldAccess (Identifier "obj", Identifier "f")),
           PrefDec (Identifier "x") ))

(* -------------------  STATEMENTS ---------------------*)

let%test _ =
  apply statement "final int a = 0, b, c, d = 5;"
  = Some
      (VarDec
         ( Some Final,
           Int,
           [
             (Name "a", Some (Const (VInt 0)));
             (Name "b", None);
             (Name "c", None);
             (Name "d", Some (Const (VInt 5)));
           ] ))

let%test _ =
  apply statement "int[] a = new int[6];"
  = Some
      (VarDec
         ( None,
           Array Int,
           [ (Name "a", Some (ArrayCreateSized (Int, Const (VInt 6)))) ] ))

let%test _ =
  apply statement "int a = 0, b = 1, c = 2;"
  = Some
      (VarDec
         ( None,
           Int,
           [
             (Name "a", Some (Const (VInt 0)));
             (Name "b", Some (Const (VInt 1)));
             (Name "c", Some (Const (VInt 2)));
           ] ))

let%test _ =
  apply statement "if (x < 10) x++;"
  = Some
      (If
         ( Less (Identifier "x", Const (VInt 10)),
           Expression (PostInc (Identifier "x")),
           None ))

let%test _ =
  apply statement
    "if (a < b) {\n return b - a; \n } else { \n return a - b; \n }"
  = Some
      (If
         ( Less (Identifier "a", Identifier "b"),
           StmtBlock [ Return (Some (Sub (Identifier "b", Identifier "a"))) ],
           Some
             (StmtBlock [ Return (Some (Sub (Identifier "a", Identifier "b"))) ])
         ))

let%test _ =
  apply statement "array = new int[3];"
  = Some
      (Expression
         (Assign (Identifier "array", ArrayCreateSized (Int, Const (VInt 3)))))

let%test _ =
  apply statement
    "if (a % 2 == 0 && b < 2) {\n\
    \ a++;\n\
    \ b--;\n\
    \ return a * b; \n\
    \ } else if (!(b / 2 != 5)) { \n\
    \ --b; \n\
    \  return (a + b)*3; \n\
    \ } else continue;"
  = Some
      (If
         ( And
             ( Equal (Mod (Identifier "a", Const (VInt 2)), Const (VInt 0)),
               Less (Identifier "b", Const (VInt 2)) ),
           StmtBlock
             [
               Expression (PostInc (Identifier "a"));
               Expression (PostDec (Identifier "b"));
               Return (Some (Mult (Identifier "a", Identifier "b")));
             ],
           Some
             (If
                ( Not
                    (NotEqual
                       (Div (Identifier "b", Const (VInt 2)), Const (VInt 5))),
                  StmtBlock
                    [
                      Expression (PrefDec (Identifier "b"));
                      Return
                        (Some
                           (Mult
                              ( Add (Identifier "a", Identifier "b"),
                                Const (VInt 3) )));
                    ],
                  Some Continue )) ))

let%test _ =
  apply statement "while (d * d <= n) { if (n % d == 0) { return true; } d++; }"
  = Some
      (While
         ( LessOrEqual (Mult (Identifier "d", Identifier "d"), Identifier "n"),
           StmtBlock
             [
               If
                 ( Equal (Mod (Identifier "n", Identifier "d"), Const (VInt 0)),
                   StmtBlock [ Return (Some (Const (VBool true))) ],
                   None );
               Expression (PostInc (Identifier "d"));
             ] ))

let%test _ =
  apply statement
    "for (int i = 0, j = n - 1; i < j; i++, j--) { \
     System.out.println(\"test\"); }"
  = Some
      (For
         ( Some
             (VarDec
                ( None,
                  Int,
                  [
                    (Name "i", Some (Const (VInt 0)));
                    (Name "j", Some (Sub (Identifier "n", Const (VInt 1))));
                  ] )),
           Some (Less (Identifier "i", Identifier "j")),
           [ PostInc (Identifier "i"); PostDec (Identifier "j") ],
           StmtBlock
             [
               Expression
                 (FieldAccess
                    ( FieldAccess (Identifier "System", Identifier "out"),
                      CallMethod
                        (Identifier "println", [ Const (VString "test") ]) ));
             ] ))

let%test _ = apply statement "for(public int i = 0;;) {i++;}" = None

let%test _ =
  apply statement
    "Figure[] list = new Figure[] {new Circle(5), new Rectangle(2,4), new \
     Triangle()};"
  = Some
      (VarDec
         ( None,
           Array (ClassName "Figure"),
           [
             ( Name "list",
               Some
                 (ArrayCreateElements
                    ( ClassName "Figure",
                      [
                        ClassCreate (Name "Circle", [ Const (VInt 5) ]);
                        ClassCreate
                          (Name "Rectangle", [ Const (VInt 2); Const (VInt 4) ]);
                        ClassCreate (Name "Triangle", []);
                      ] )) );
           ] ))

(*---------------- IN CLASSES ---------------*)

let%test _ =
  apply class_elem "public int wheel;"
  = Some ([ Public ], VarField (Int, [ (Name "wheel", None) ]))

let%test _ =
  apply class_elem
    "public int arraySum (int[] a) { int sum = 0; for (int i = 0; i < \
     a.length(); i++) {sum = sum + a[i];} return sum; }"
  = Some
      ( [ Public ],
        Method
          ( Int,
            Name "arraySum",
            [ (Array Int, Name "a") ],
            Some
              (StmtBlock
                 [
                   VarDec (None, Int, [ (Name "sum", Some (Const (VInt 0))) ]);
                   For
                     ( Some
                         (VarDec
                            (None, Int, [ (Name "i", Some (Const (VInt 0))) ])),
                       Some
                         (Less
                            ( Identifier "i",
                              FieldAccess
                                ( Identifier "a",
                                  CallMethod (Identifier "length", []) ) )),
                       [ PostInc (Identifier "i") ],
                       StmtBlock
                         [
                           Expression
                             (Assign
                                ( Identifier "sum",
                                  Add
                                    ( Identifier "sum",
                                      ArrayAccess
                                        (Identifier "a", Identifier "i") ) ));
                         ] );
                   Return (Some (Identifier "sum"));
                 ]) ) )

let%test _ =
  apply class_elem
    "public Car(int speed, int[] wheels) {this.speed = speed; this.wheels = \
     wheels;}"
  = Some
      ( [ Public ],
        Constructor
          ( Name "Car",
            [ (Int, Name "speed"); (Array Int, Name "wheels") ],
            StmtBlock
              [
                Expression
                  (Assign
                     (FieldAccess (This, Identifier "speed"), Identifier "speed"));
                Expression
                  (Assign
                     ( FieldAccess (This, Identifier "wheels"),
                       Identifier "wheels" ));
              ] ) )

(* -------------------  CLASS_LOADER ------------------- *)

let%test _ =
  check_modifiers_f ([ Abstract; Static ], Method (Void, Name "m", [], None))
  = Error "Wrong method modifiers"

let%test _ =
  check_modifiers_f ([ Public; Static ], Method (Void, Name "main", [], None))
  = Ok ()

let%test _ =
  check_modifiers_f ([ Abstract ], VarField (Int, [ (Name "a", None) ]))
  = Error "Wrong field modifiers"

let%test _ =
  check_modifiers_f ([ Final ], VarField (Int, [ (Name "a", None) ])) = Ok ()

let%test _ =
  check_modifiers_f ([ Final ], Constructor (Name "Car", [], StmtBlock []))
  = Error "Wrong constructor modifiers"

let%test _ =
  check_modifiers_c (Class ([ Abstract; Final ], Name "Man", None, []))
  = Error "Wrong class modifiers"
