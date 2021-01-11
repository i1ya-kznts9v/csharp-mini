open Parser
open Parser.Expr
open Parser.Stat
open Ast

let%test _ =
  apply_parser get_modifier_list "public static const"
  = Some [Public; Static; Const]

let%test _ = apply_parser convert_to_int "4" = Some 4
let%test _ = apply_parser null "  null" = Some Null
let%test _ = apply_parser ident_obj "   car" = Some "car"
let%test _ = apply_parser ident_obj "  Cat1" = Some "Cat1"
let%test _ = apply_parser get_variable "   Cat" = Some (IdentVar "Cat")

let%test _ =
  apply_parser parse_string "\"Parse\"" = Some (ConstExpr (VString "Parse"))

let%test _ = apply_parser atomic "      123123" = Some (ConstExpr (VInt 123123))
let%test _ = apply_parser ident_obj "  123Ret" = None

let%test _ =
  apply_parser atomic "\"Parse\"" = Some (ConstExpr (VString "Parse"))

let%test _ = apply_parser atomic "   true" = Some (ConstExpr (VBool true))
let%test _ = apply_parser atomic "   false" = Some (ConstExpr (VBool false))
let%test _ = apply_parser atomic "   null" = Some Null
let%test _ = apply_parser define_type "  int" = Some Int
let%test _ = apply_parser define_type "  void" = Some Void
let%test _ = apply_parser define_type "  string" = Some String
let%test _ = apply_parser define_type "  Excep" = Some (CsClass "Excep")

(* ------------------------  EXPRESSIONS -------------------------- *)
let%test _ =
  apply_parser expr "a = b = 1"
  = Some (Assign (IdentVar "a", Assign (IdentVar "b", ConstExpr (VInt 1))))

let%test _ =
  apply_parser expr "4 + 5"
  = Some (Add (ConstExpr (VInt 4), ConstExpr (VInt 5)))

let%test _ =
  apply_parser expr "2/5 + 3 * (5 % 3)"
  = Some
      (Add
         ( Div (ConstExpr (VInt 2), ConstExpr (VInt 5))
         , Mul (ConstExpr (VInt 3), Mod (ConstExpr (VInt 5), ConstExpr (VInt 3)))
         ))

let%test _ =
  apply_parser expr "x = true"
  = Some (Assign (IdentVar "x", ConstExpr (VBool true)))

let%test _ =
  apply_parser expr "a.b.c"
  = Some (Access (Access (IdentVar "a", IdentVar "b"), IdentVar "c"))

let%test _ =
  apply_parser expr "obj.Sum(5, arg2, arg3 * 3)"
  = Some
      (Access
         ( IdentVar "obj"
         , CallMethod
             ( "Sum"
             , [ ConstExpr (VInt 5); IdentVar "arg2"
               ; Mul (IdentVar "arg3", ConstExpr (VInt 3)) ] ) ))

let%test _ =
  apply_parser expr "Sum(obj.a, 3)"
  = Some
      (CallMethod
         ("Sum", [Access (IdentVar "obj", IdentVar "a"); ConstExpr (VInt 3)]))

let%test _ =
  apply_parser expr "new Shop(5,\"MVideo\")"
  = Some
      (ClassCreate ("Shop", [ConstExpr (VInt 5); ConstExpr (VString "MVideo")]))

let%test _ =
  apply_parser expr "Fork(new Child())"
  = Some (CallMethod ("Fork", [ClassCreate ("Child", [])]))

(* -----------------------  STATEMENTS ------------------------*)

let%test _ =
  apply_parser parse_statement "int a = 0, b = 1, c = 2;"
  = Some
      (VarDeclare
         ( None
         , Int
         , [ ("a", Some (ConstExpr (VInt 0))); ("b", Some (ConstExpr (VInt 1)))
           ; ("c", Some (ConstExpr (VInt 2))) ] ))

let%test _ =
  apply_parser parse_statement "const int a = 0, b = 1, c = 2;"
  = Some
      (VarDeclare
         ( Some Const
         , Int
         , [ ("a", Some (ConstExpr (VInt 0))); ("b", Some (ConstExpr (VInt 1)))
           ; ("c", Some (ConstExpr (VInt 2))) ] ))

let%test _ = apply_parser break "break;" = Some Break
let%test _ = apply_parser continue "continue;" = Some Continue

let%test _ =
  apply_parser return_stat "return 3;"
  = Some (Return (Some (ConstExpr (VInt 3))))

let%test _ =
  apply_parser parse_statement {|  if(num1 > num2)
{
}   |}
  = Some (If (More (IdentVar "num1", IdentVar "num2"), StatementBlock [], None))

let%test _ =
  apply_parser parse_statement
    {|  if(num1 > num2)
{        Console.WriteLine("help");
}   |}
  = Some
      (If
         ( More (IdentVar "num1", IdentVar "num2")
         , StatementBlock [Print (ConstExpr (VString "help"))]
         , None ))

let%test _ =
  apply_parser parse_statement
    {|  if(num1 > num2)
{
    Console.WriteLine(a.Sum());
}
else if (num1 < num2)
{
    Console.WriteLine(a.b);
}
else
{
    Console.WriteLine("2");
} |}
  = Some
      (If
         ( More (IdentVar "num1", IdentVar "num2")
         , StatementBlock [Print (Access (IdentVar "a", CallMethod ("Sum", [])))]
         , Some
             (If
                ( Less (IdentVar "num1", IdentVar "num2")
                , StatementBlock [Print (Access (IdentVar "a", IdentVar "b"))]
                , Some (StatementBlock [Print (ConstExpr (VString "2"))]) )) ))

let%test _ =
  apply_parser parse_statement
    {| while (i > 0)
{
    Console.WriteLine(i);
    i--;
} |}
  = Some
      (While
         ( More (IdentVar "i", ConstExpr (VInt 0))
         , StatementBlock
             [Print (IdentVar "i"); Expression (PostDec (IdentVar "i"))] ))

let%test _ =
  apply_parser parse_statement
    {| for (int i = 0; i < 9; i++)
{
    Console.WriteLine(1);
}|}
  = Some
      (For
         ( Some (VarDeclare (None, Int, [("i", Some (ConstExpr (VInt 0)))]))
         , Some (Less (IdentVar "i", ConstExpr (VInt 9)))
         , [PostInc (IdentVar "i")]
         , StatementBlock [Print (ConstExpr (VInt 1))] ))

let%test _ =
  apply_parser parse_statement
    {| { if(a<b) {}
for (; ;)
{
    Console.WriteLine("1");
} }
|}
  = Some
      (StatementBlock
         [ If (Less (IdentVar "a", IdentVar "b"), StatementBlock [], None)
         ; For (None, None, [], StatementBlock [Print (ConstExpr (VString "1"))])
         ])

let%test _ =
  apply_parser parse_statement
    {|  for (; i<9;)
{
    Console.WriteLine(i*i);
} |}
  = Some
      (For
         ( None
         , Some (Less (IdentVar "i", ConstExpr (VInt 9)))
         , []
         , StatementBlock [Print (Mul (IdentVar "i", IdentVar "i"))] ))

let%test _ =
  apply_parser parse_statement
    {|  for (int i = 0; i < 9; i++)
{
    if (i == 5)
        break;
    Console.WriteLine(i);
}|}
  = Some
      (For
         ( Some (VarDeclare (None, Int, [("i", Some (ConstExpr (VInt 0)))]))
         , Some (Less (IdentVar "i", ConstExpr (VInt 9)))
         , [PostInc (IdentVar "i")]
         , StatementBlock
             [ If (Equal (IdentVar "i", ConstExpr (VInt 5)), Break, None)
             ; Print (IdentVar "i") ] ))

let%test _ =
  apply_parser parse_statement
    {| try
        {
            int x = 5;
            int y = x / 0;
            Console.WriteLine(y);
        }
        catch
        {
            Console.WriteLine("excep");
        }
        finally
        {
            Console.WriteLine("finally");
        }|}
  = Some
      (Try
         ( StatementBlock
             [ VarDeclare (None, Int, [("x", Some (ConstExpr (VInt 5)))])
             ; VarDeclare
                 ( None
                 , Int
                 , [("y", Some (Div (IdentVar "x", ConstExpr (VInt 0))))] )
             ; Print (IdentVar "y") ]
         , [(None, None, StatementBlock [Print (ConstExpr (VString "excep"))])]
         , Some (StatementBlock [Print (ConstExpr (VString "finally"))]) ))

let%test _ =
  apply_parser parse_statement
    {|  try
{
    int x = 5;
    int y = x / 0;
    Console.WriteLine(x);
}
catch
{
    Console.WriteLine("3");
}
       |}
  = Some
      (Try
         ( StatementBlock
             [ VarDeclare (None, Int, [("x", Some (ConstExpr (VInt 5)))])
             ; VarDeclare
                 ( None
                 , Int
                 , [("y", Some (Div (IdentVar "x", ConstExpr (VInt 0))))] )
             ; Print (IdentVar "x") ]
         , [(None, None, StatementBlock [Print (ConstExpr (VString "3"))])]
         , None ))

let%test _ =
  apply_parser parse_statement
    {|  try
{
    int x = 5;
    int y = x / 0;
    
}
finally
{
 
} 
 |}
  = Some
      (Try
         ( StatementBlock
             [ VarDeclare (None, Int, [("x", Some (ConstExpr (VInt 5)))])
             ; VarDeclare
                 ( None
                 , Int
                 , [("y", Some (Div (IdentVar "x", ConstExpr (VInt 0))))] ) ]
         , []
         , Some (StatementBlock []) ))

let%test _ =
  apply_parser parse_statement {|  try {} catch when (a==2) {}|}
  = Some
      (Try
         ( StatementBlock []
         , [ ( None
             , Some (Equal (IdentVar "a", ConstExpr (VInt 2)))
             , StatementBlock [] ) ]
         , None ))

let%test _ =
  apply_parser parse_statement {| try {} catch {} |}
  = Some (Try (StatementBlock [], [(None, None, StatementBlock [])], None))

let%test _ =
  apply_parser parse_statement {| try {} catch (Exception) when (a==2) {}|}
  = Some
      (Try
         ( StatementBlock []
         , [ ( Some (CsClass "Exception", None)
             , Some (Equal (IdentVar "a", ConstExpr (VInt 2)))
             , StatementBlock [] ) ]
         , None ))

let%test _ =
  apply_parser parse_statement {| try {} catch (Exception) {}|}
  = Some
      (Try
         ( StatementBlock []
         , [(Some (CsClass "Exception", None), None, StatementBlock [])]
         , None ))

let%test _ =
  apply_parser parse_statement {|  try {} catch (Exception e) when (a==2) {}|}
  = Some
      (Try
         ( StatementBlock []
         , [ ( Some (CsClass "Exception", Some (IdentVar "e"))
             , Some (Equal (IdentVar "a", ConstExpr (VInt 2)))
             , StatementBlock [] ) ]
         , None ))

let%test _ =
  apply_parser parse_statement {|   try {} catch (Exception e) {} |}
  = Some
      (Try
         ( StatementBlock []
         , [ ( Some (CsClass "Exception", Some (IdentVar "e"))
             , None
             , StatementBlock [] ) ]
         , None ))

let%test _ =
  apply_parser class_elements {|  public int sum;|}
  = Some ([Public], VariableField (Int, [("sum", None)]))

let%test _ =
  apply_parser class_elements
    {| static void SayHello()
{
    int hour = 23;
    if(hour > 22)
    {
        return;
    }
    else
    {
        Console.WriteLine("Hello");
    }
}
|}
  = Some
      ( [Static]
      , Method
          ( Void
          , "SayHello"
          , []
          , StatementBlock
              [ VarDeclare (None, Int, [("hour", Some (ConstExpr (VInt 23)))])
              ; If
                  ( More (IdentVar "hour", ConstExpr (VInt 22))
                  , StatementBlock [Return None]
                  , Some (StatementBlock [Print (ConstExpr (VString "Hello"))])
                  ) ] ) )

let%test _ =
  apply_parser class_elements
    {| public Person(string n) { name = n; age = 18; }|}
  = Some
      ( [Public]
      , Constructor
          ( "Person"
          , [(String, "n")]
          , StatementBlock
              [ Expression (Assign (IdentVar "name", IdentVar "n"))
              ; Expression (Assign (IdentVar "age", ConstExpr (VInt 18))) ] ) )
