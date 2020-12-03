open Parser
open Parser.Expression
open Parser.Statement
open Parser.Class

module Expression = struct
  let%test _ =
    apply expression "x = 0" = Some (Assign (Identifier "x", Value (VInt 0)))

  let%test _ =
    apply expression "arr[i]"
    = Some (ArrayAccess (Identifier "arr", Identifier "i"))

  let%test _ =
    apply expression "a[i] = 10"
    = Some
        (Assign (ArrayAccess (Identifier "a", Identifier "i"), Value (VInt 10)))

  let%test _ =
    apply expression "x = y = 100"
    = Some (Assign (Identifier "x", Assign (Identifier "y", Value (VInt 100))))

  let%test _ =
    apply expression "19 - 1" = Some (Sub (Value (VInt 19), Value (VInt 1)))

  let%test _ =
    apply expression "6 - 5 * 4 / 3 % 2 + 17"
    = Some
        (Add
           ( Sub
               ( Value (VInt 6)
               , Mod
                   ( Div (Mult (Value (VInt 5), Value (VInt 4)), Value (VInt 3))
                   , Value (VInt 2) ) )
           , Value (VInt 17) ))

  let%test _ =
    apply expression "!(c / 2 < 3) || (x + y >= 19) && (a % 2 != 0)"
    = Some
        (Or
           ( Not (Less (Div (Identifier "c", Value (VInt 2)), Value (VInt 3)))
           , And
               ( MoreOrEqual
                   (Add (Identifier "x", Identifier "y"), Value (VInt 19))
               , NotEqual (Mod (Identifier "a", Value (VInt 2)), Value (VInt 0))
               ) ))

  let%test _ =
    apply expression "(2 + 3) * 5 - 3"
    = Some
        (Sub
           ( Mult (Add (Value (VInt 2), Value (VInt 3)), Value (VInt 5))
           , Value (VInt 3) ))

  let%test _ =
    apply expression "Class.method(arg1, 1 + 9, 1901)"
    = Some
        (AccessByPoint
           ( Identifier "Class"
           , CallMethod
               ( Identifier "method"
               , [ Identifier "arg1"; Add (Value (VInt 1), Value (VInt 9))
                 ; Value (VInt 1901) ] ) ))

  let%test _ =
    apply expression "arr[j].push(\"Parser\")"
    = Some
        (AccessByPoint
           ( ArrayAccess (Identifier "arr", Identifier "j")
           , CallMethod (Identifier "push", [Value (VString "Parser")]) ))

  let%test _ =
    apply expression "a[i % m].b[j + k]"
    = Some
        (ArrayAccess
           ( AccessByPoint
               ( ArrayAccess
                   (Identifier "a", Mod (Identifier "i", Identifier "m"))
               , Identifier "b" )
           , Add (Identifier "j", Identifier "k") ))

  let%test _ =
    apply expression "base.getArrayPart(3)[i]"
    = Some
        (ArrayAccess
           ( AccessByPoint
               (Base, CallMethod (Identifier "getArrayPart", [Value (VInt 3)]))
           , Identifier "i" ))

  let%test _ =
    apply expression "this.getSkate(\"Prof\").wheels[3].size"
    = Some
        (AccessByPoint
           ( ArrayAccess
               ( AccessByPoint
                   ( AccessByPoint
                       ( This
                       , CallMethod
                           (Identifier "getSkate", [Value (VString "Prof")]) )
                   , Identifier "wheels" )
               , Value (VInt 3) )
           , Identifier "size" ))

  let%test _ =
    apply expression "a[i].b.c[j]"
    = Some
        (ArrayAccess
           ( AccessByPoint
               ( AccessByPoint
                   (ArrayAccess (Identifier "a", Identifier "i"), Identifier "b")
               , Identifier "c" )
           , Identifier "j" ))

  let%test _ =
    apply expression "x.y.z"
    = Some
        (AccessByPoint
           (AccessByPoint (Identifier "x", Identifier "y"), Identifier "z"))

  let%test _ =
    apply expression "method(99 < 2, 4 * 10)"
    = Some
        (CallMethod
           ( Identifier "method"
           , [ Less (Value (VInt 99), Value (VInt 2))
             ; Mult (Value (VInt 4), Value (VInt 10)) ] ))

  let%test _ = apply expression "new object[]" = None

  let%test _ =
    apply expression "new int[] {0 % 1, 2 / 3, 4 * 5, i}"
    = Some
        (ArrayCreationWithElements
           ( TInt
           , [ Mod (Value (VInt 0), Value (VInt 1))
             ; Div (Value (VInt 2), Value (VInt 3))
             ; Mult (Value (VInt 4), Value (VInt 5)); Identifier "i" ] ))

  let%test _ =
    apply expression "new int[17]"
    = Some (ArrayCreationWithSize (TInt, Value (VInt 17)))

  let%test _ =
    apply expression "new Class[i]"
    = Some (ArrayCreationWithSize (TClass "Class", Identifier "i"))

  let%test _ =
    apply expression "new Car(150,\"Toyota Corolla\")"
    = Some
        (ClassCreation
           (Name "Car", [Value (VInt 150); Value (VString "Toyota Corolla")]))

  let%test _ =
    apply expression "lock(new Mutex(), new String[5])"
    = Some
        (CallMethod
           ( Identifier "lock"
           , [ ClassCreation (Name "Mutex", [])
             ; ArrayCreationWithSize (TClass "String", Value (VInt 5)) ] ))

  let%test _ =
    apply expression "(new Killer(\"John\", \"Wick\")).shoot())"
    = Some
        (AccessByPoint
           ( ClassCreation
               (Name "Killer", [Value (VString "John"); Value (VString "Wick")])
           , CallMethod (Identifier "shoot", []) ))

  let%test _ =
    apply expression "--(obj.f / (x * y)++)"
    = Some
        (PrefDec
           (Div
              ( AccessByPoint (Identifier "obj", Identifier "f")
              , PostInc (Mult (Identifier "x", Identifier "y")) )))
end

module Statement = struct
  let%test _ =
    apply statement "int a = 0, b = 1, c, d = 17;"
    = Some
        (VariableDecl
           ( TInt
           , [ (Name "a", Some (Value (VInt 0)))
             ; (Name "b", Some (Value (VInt 1))); (Name "c", None)
             ; (Name "d", Some (Value (VInt 17))) ] ))

  let%test _ =
    apply statement "int[] array = new int[19 + 1];"
    = Some
        (VariableDecl
           ( TArray TInt
           , [ ( Name "array"
               , Some
                   (ArrayCreationWithSize
                      (TInt, Add (Value (VInt 19), Value (VInt 1)))) ) ] ))

  let%test _ = apply statement "int[20] array = new int[19 + 1];" = None

  let%test _ =
    apply statement "string a = \"a\", b = \"1\", c = \"a1\";"
    = Some
        (VariableDecl
           ( TString
           , [ (Name "a", Some (Value (VString "a")))
             ; (Name "b", Some (Value (VString "1")))
             ; (Name "c", Some (Value (VString "a1"))) ] ))

  let%test _ =
    apply statement "if (x <= 10 * a) ++x;"
    = Some
        (If
           ( LessOrEqual (Identifier "x", Mult (Value (VInt 10), Identifier "a"))
           , Expression (PrefInc (Identifier "x"))
           , None ))

  let%test _ =
    apply statement
      "if (a < b)\n{\n return b - a; \n }\n else \n{ \n return a - b; \n }"
    = Some
        (If
           ( Less (Identifier "a", Identifier "b")
           , StatementBlock
               [Return (Some (Sub (Identifier "b", Identifier "a")))]
           , Some
               (StatementBlock
                  [Return (Some (Sub (Identifier "a", Identifier "b")))]) ))

  let%test _ =
    apply statement "array = new int[77];"
    = Some
        (Expression
           (Assign
              (Identifier "array", ArrayCreationWithSize (TInt, Value (VInt 77)))))

  let%test _ =
    apply statement
      {|
      if (a % 2 == 0 && b < 2)
      {
        a++;
        --b;
        return a * b;
      } 
      else if (!(b / 2 != 5)) 
      {
        b = b + 40;
        return (a + b)*3;
      } 
      else continue;
      |}
    = Some
        (If
           ( And
               ( Equal (Mod (Identifier "a", Value (VInt 2)), Value (VInt 0))
               , Less (Identifier "b", Value (VInt 2)) )
           , StatementBlock
               [ Expression (PostInc (Identifier "a"))
               ; Expression (PrefDec (Identifier "b"))
               ; Return (Some (Mult (Identifier "a", Identifier "b"))) ]
           , Some
               (If
                  ( Not
                      (NotEqual
                         (Div (Identifier "b", Value (VInt 2)), Value (VInt 5)))
                  , StatementBlock
                      [ Expression
                          (Assign
                             ( Identifier "b"
                             , Add (Identifier "b", Value (VInt 40)) ))
                      ; Return
                          (Some
                             (Mult
                                ( Add (Identifier "a", Identifier "b")
                                , Value (VInt 3) ))) ]
                  , Some Continue )) ))

  let%test _ =
    apply statement
      {|
      while (d * d <= n)
      { 
        if (n % d == 0)
        { 
          break;          
        } 
        d++; 
      }
      |}
    = Some
        (While
           ( LessOrEqual (Mult (Identifier "d", Identifier "d"), Identifier "n")
           , StatementBlock
               [ If
                   ( Equal (Mod (Identifier "n", Identifier "d"), Value (VInt 0))
                   , StatementBlock [Break]
                   , None ); Expression (PostInc (Identifier "d")) ] ))

  let%test _ =
    apply statement
      {|
      for (int i = 0, j = n - 1; i >= j; i++, j--)
      {
        Console.WriteLine("test");
      }
      |}
    = Some
        (For
           ( Some
               (VariableDecl
                  ( TInt
                  , [ (Name "i", Some (Value (VInt 0)))
                    ; (Name "j", Some (Sub (Identifier "n", Value (VInt 1)))) ]
                  ))
           , Some (MoreOrEqual (Identifier "i", Identifier "j"))
           , [PostInc (Identifier "i"); PostDec (Identifier "j")]
           , StatementBlock
               [ Expression
                   (AccessByPoint
                      ( Identifier "Console"
                      , CallMethod
                          (Identifier "WriteLine", [Value (VString "test")]) ))
               ] ))

  let%test _ =
    apply statement
      {|
      if (Mistake())
        throw new Exception("Bad reference");
      |}
    = Some
        (If
           ( CallMethod (Identifier "Mistake", [])
           , Throw
               (ClassCreation
                  (Name "Exception", [Value (VString "Bad reference")]))
           , None ))

  let%test _ = apply statement "for(public int i = 0;;) {i++;}" = None
end

module Class = struct
  let%test _ =
    apply class_element "public static int[] kids;"
    = Some ([Public; Static], Field (TArray TInt, [(Name "kids", None)]))

  let%test _ = apply class_element "public static int[5] kids;" = None

  let%test _ =
    apply class_element
      {|
      public virtual int ArraySum (int[] a)
      { 
        int sum = 0; 
        for (int i = 0; i < a.Length(); i++) 
        {            
          sum = sum + a[i];
        } 
        return sum; 
      }
      |}
    = Some
        ( [Public; Virtual]
        , Method
            ( TInt
            , Name "ArraySum"
            , [(TArray TInt, Name "a")]
            , Some
                (StatementBlock
                   [ VariableDecl (TInt, [(Name "sum", Some (Value (VInt 0)))])
                   ; For
                       ( Some
                           (VariableDecl
                              (TInt, [(Name "i", Some (Value (VInt 0)))]))
                       , Some
                           (Less
                              ( Identifier "i"
                              , AccessByPoint
                                  ( Identifier "a"
                                  , CallMethod (Identifier "Length", []) ) ))
                       , [PostInc (Identifier "i")]
                       , StatementBlock
                           [ Expression
                               (Assign
                                  ( Identifier "sum"
                                  , Add
                                      ( Identifier "sum"
                                      , ArrayAccess
                                          (Identifier "a", Identifier "i") ) ))
                           ] ); Return (Some (Identifier "sum")) ]) ) )

  let%test _ =
    apply class_element
      {|
      public Car(int speed, int[] wheels)
      {
        this.speed = speed; 
        this.wheels = wheels;
      }
      |}
    = Some
        ( [Public]
        , Constructor
            ( Name "Car"
            , [(TInt, Name "speed"); (TArray TInt, Name "wheels")]
            , None
            , StatementBlock
                [ Expression
                    (Assign
                       ( AccessByPoint (This, Identifier "speed")
                       , Identifier "speed" ))
                ; Expression
                    (Assign
                       ( AccessByPoint (This, Identifier "wheels")
                       , Identifier "wheels" )) ] ) )

  let%test _ =
    apply class_element
      {|
      public Car(int speed, int[] wheels) : base(wheels)
      {
        this.speed = speed; 
        this.wheels = wheels;
      }
      |}
    = Some
        ( [Public]
        , Constructor
            ( Name "Car"
            , [(TInt, Name "speed"); (TArray TInt, Name "wheels")]
            , Some (CallMethod (Base, [Identifier "wheels"]))
            , StatementBlock
                [ Expression
                    (Assign
                       ( AccessByPoint (This, Identifier "speed")
                       , Identifier "speed" ))
                ; Expression
                    (Assign
                       ( AccessByPoint (This, Identifier "wheels")
                       , Identifier "wheels" )) ] ) )

  let%test _ =
    apply class_element
      {|
      public Car(int speed, int[] wheels) : this()
      {
        this.speed = speed; 
        this.wheels = wheels;
      }
      |}
    = Some
        ( [Public]
        , Constructor
            ( Name "Car"
            , [(TInt, Name "speed"); (TArray TInt, Name "wheels")]
            , Some (CallMethod (This, []))
            , StatementBlock
                [ Expression
                    (Assign
                       ( AccessByPoint (This, Identifier "speed")
                       , Identifier "speed" ))
                ; Expression
                    (Assign
                       ( AccessByPoint (This, Identifier "wheels")
                       , Identifier "wheels" )) ] ) )

  let%test _ =
    apply constructor_decl
      {|
      public Car(int speed, int[] wheels) : Vehicle(wheels)
      {
        this.speed = speed; 
        this.wheels = wheels;
      }
      |}
    = None

  let%test _ =
    apply class_decl
      {|
      public class JetBrains : Company
      {
        int employees = 1000;
        string status = "Close";

        public override void InviteToJob()
        {
          employees++;
        }

        public override void DismissFromJob()
        {
          employees--;
        }

        public override void Open()
        {
          status = "Open";
        }

        public override void Close()
        {
          status = "Close";
        }
      }
      |}
    = Some
        (Class
           ( [Public]
           , Name "JetBrains"
           , Some (Name "Company")
           , [ ([], Field (TInt, [(Name "employees", Some (Value (VInt 1000)))]))
             ; ( []
               , Field
                   (TString, [(Name "status", Some (Value (VString "Close")))])
               )
             ; ( [Public; Override]
               , Method
                   ( TVoid
                   , Name "InviteToJob"
                   , []
                   , Some
                       (StatementBlock
                          [Expression (PostInc (Identifier "employees"))]) ) )
             ; ( [Public; Override]
               , Method
                   ( TVoid
                   , Name "DismissFromJob"
                   , []
                   , Some
                       (StatementBlock
                          [Expression (PostDec (Identifier "employees"))]) ) )
             ; ( [Public; Override]
               , Method
                   ( TVoid
                   , Name "Open"
                   , []
                   , Some
                       (StatementBlock
                          [ Expression
                              (Assign
                                 (Identifier "status", Value (VString "Open")))
                          ]) ) )
             ; ( [Public; Override]
               , Method
                   ( TVoid
                   , Name "Close"
                   , []
                   , Some
                       (StatementBlock
                          [ Expression
                              (Assign
                                 (Identifier "status", Value (VString "Close")))
                          ]) ) ) ] ))
end
