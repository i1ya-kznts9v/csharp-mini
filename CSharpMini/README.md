### An implementaion of CSharpMini language in Ocaml

This is a homework for functional programming course

License: LGPL

Author: Kuznetsov Ilya Alexandrovich, kuznetsov.ilya.alexandrovich@gmail.com

How to use REPL?

- 1 Launching factorial:

```
    > int Fact(int acc, int n) { if (n <= 1) return acc; else return Fact(acc * n, n - 1); }#
    > int Factorial(int n) { return Fact(1, n); }#
    > Factorial(5);#
    > Result: VInt (120)
```

- 2 Lauching built-in commands:

```
    > int Id(int n) { return n; }#                    
    Method added
    > int Square(int n) { return n*n; }#
    Method added
    > show_methods#
    Available methods:
    "int Id(int n) { return n; }"
    "int Square(int n) { return n*n; }"
```

- 3 How to make sure C# is bad (ArrayTypeMismatchException):

```
    > Object[] x = new Object[3];#    
    Statement interpreted
    > x[0] = new Circle(5);#
    ArrayTypeMismatchException
```

Features done:

- 1 AST
- 2 Parser
- 3 Interpretation
- 4 Full and detailed testing of all previous points
- 5 REPL
- 6 Standart REPL library
<<<<<<< HEAD
- 7 Pretty printer
=======
>>>>>>> 97e0f3a26b979a150ca602322e6fbf6e86889754

In detail:

- 1 Class loading
- 2 Standard constructs: branches, loops
- 3 Standard types: numbers, strings and operations with them
- 4 Standard array type and operations with it
<<<<<<< HEAD
- 5 Recursion
- 6 Subtype polymorphism
- 7 OOP: classes, public methods, public fields
- 8 OOP: Standard Base Object Class
- 9 OOP: inheritance
- 10 PP: Standart C# codestyle formatting
=======
- 5 OOP: classes, public methods, public fields
- 6 OOP: Standard Base Object Class
- 7 OOP: inheritance
- 8 Recursion
- 9 Subtype polymorphism
>>>>>>> 97e0f3a26b979a150ca602322e6fbf6e86889754

Notes:

- 1 Keywords cannot be class names
- 2 The Main method does not take anything into itself, it is the entry point of the program. The Main method is the only one
- 3 Post- and pref- incrementing equivalents
- 4 StackOverflow is not detected
- 5 Multidimensional arrays are not supported
- 6 Casting is not supported
- 7 When comparing objects, the Еquals () method is called if it is overridden. Otherwise, the links are directly compared.
- 8 Visitor pattern is fully supported

Features in progress:

<<<<<<< HEAD
- 1 Transformations
- 2 Searching for new bugs and fixing them
=======
- 1 Pretty printer
- 2 Transformations
- 3 Searching for new bugs and fixing them
>>>>>>> 97e0f3a26b979a150ca602322e6fbf6e86889754

Hardcoded classes:

- 1 Object

```
    public class Object 
    {
      public int Equals(Object obj) 
      {
        if (this == obj) return 1;
        else return 0;
      }
            
      public string ToString() 
      {
        return "Object";
      }
    }
```