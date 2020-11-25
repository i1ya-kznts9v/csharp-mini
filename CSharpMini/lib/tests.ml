open Parser
open Parser.Expression
open Parser.Statement
open Parser.Class
open Opal

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
           ( Identifier "Car"
           , [Value (VInt 150); Value (VString "Toyota Corolla")] ))

  let%test _ =
    apply expression "lock(new Mutex(), new String[5])"
    = Some
        (CallMethod
           ( Identifier "lock"
           , [ ClassCreation (Identifier "Mutex", [])
             ; ArrayCreationWithSize (TClass "String", Value (VInt 5)) ] ))

  let%test _ =
    apply expression "(new Killer(\"John\", \"Wick\")).shoot())"
    = Some
        (AccessByPoint
           ( ClassCreation
               ( Identifier "Killer"
               , [Value (VString "John"); Value (VString "Wick")] )
           , CallMethod (Identifier "shoot", []) ))

  let%test _ =
    apply expression "--(obj.f / (x * y)++)"
    = Some
        (PrefDec
           (Div
              ( AccessByPoint (Identifier "obj", Identifier "f")
              , PostInc (Mult (Identifier "x", Identifier "y")) )))
end

module Statement = struct end
module Class = struct end
