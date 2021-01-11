open Csharp_lib.Parser
open Csharp_lib.Interpreter.ClassLoader (Csharp_lib.Interpreter.Result)
open Csharp_lib.Interpreter.Interpreter (Csharp_lib.Interpreter.Result)

let test_interp test_val cl_t =
  match load_classes test_val cl_t with
  | Error m -> print_endline m ; Hashtbl.clear cl_t
  | Ok load_table -> (
    match interprete_program load_table with
    | Error m -> print_endline m ; Hashtbl.clear load_table
    | Ok res_context ->
        print_endline (show_context res_context ^ "\n") ;
        Hashtbl.clear load_table )

let () =
  print_string "-_-_-_-_-_-_-_-_-_-_- Assign test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            public static void Main() {
                int a = 1;
                int b = 2;
                int c = 3;
            }  
        }
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string "-_-_-_-_-_-_-_-_-_-_- Arithmetic test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            public static void Main() {
                int a = 1;
        int b = 2;
        int c = 3;
        int val1 = 1 + 2 + 3 + 4 + 5;
        int val2 = a + b;
        int val3 = a + 100;
        int val4 = 10 / 2;
        int val5 = 10 % 2;
        int val6 = (a + b) * 100; 
        a = a + 1;
        int val7 = (val1 * val2 + 4) / 2 + 100; 
        string s1 = "a";
        string s2 = "b";
        string s3 = s1 + s2;
        string s4 = s1 + a;
        string s5 = a + s2;

            }  
        }
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Bool expression test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            public static void Main() {
                int andVal = 0, orVal = 0, notVal = 0;
        int mVal = 0, meVal = 0, lVal = 0, leVal = 0;
        int eVal = 0, neVal = 0;
        
        int a = 10, b = 50, c = 100;
        if (b > a) {
            mVal = 1;
        }
        if (a < c) {
            lVal = 1;
        } 
        if (b <= c) {
            leVal = 1;
        }
        if (c >= b) {
            meVal = 1;
        }
        int d = 10;
        if (d == a) {
            eVal = 1;
        }
        if (a != b) {
            neVal = 1;
        }
        if (a < b && b < c) {
            andVal = 1;
        }
        if (a < b || a != 10) {
            orVal = 1;
        }
        if (!(a >= c)) {
            notVal = 1;
        }
        string s1 = "a", s2 = "b";
        int sEq = 0, sNEQ; 
        if (s1 == "a") {
            sEq = 1;
        }
        if (s2 != "a") {
            sNEQ = 1;
        }
        Person p1 = new Person(20, "Bob"), p2 = new Person(30, "Alice"), p3 = p1;
        int objEq = 0, objNEq = 0; 
        if (p1 != p2) {
            objNEq = 1;
        }
        if (p1 == p3) {
            objEq = 1;
        }



        }  
     }

    class Person 
    {
    int age;
    string name;

    public Person(int agew, string namew) {
        age = agew;
        name = namew;
    }

    } 
      |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Static method call test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

        static void Main()
        {
            SayHello();
            SayGoodbye();

        }
 
        static void SayHello()
        {
            Console.WriteLine("Hello");
        }
        static void SayGoodbye()
        {
            Console.WriteLine("GoodBye");
        }
       
    }

    
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Class method call test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

        static void Main()
        {
            Person tom = new Person();
            tom.GetInfo();     
    
            tom.name = "Tom";
            tom.age = 34;
            tom.GetInfo(); 

        }

       
    
  }
class Person
{
    public string name; 
    public int age;     
 
    public void GetInfo()
    {
        Console.WriteLine(name + age);
    }
}
    
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Update object state -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

        static void Main()
        {
            Person person = new Person();
            Person p1, p2, p3;
            p1 = person;
            p2 = p1;
            p3 = p2;
            person.age = 55;
            int res = p2.GetAge(); 
        }    
  }
class Person
{
    public string name; 
    public int age;     
 
    public int GetAge()
    {
        return age;
    }
}
    
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Visibility level test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

        static void Main()
        {
            int a = 2;
        int b = 3;
        int c = 4;
        if (c == 4) {
           int d = 7;
           a = a + 1;
           int e = 10;
           int g = 42;
        }
        b = 5;
        c = 6;
        a = 15;
        int i = 0;
        while (i < 3) {
      	   int m = 2;
      	   int n = 3;
      	   int z = 4;
      	   int f = 5;
      	   i = i + 1;
      	 }
      	a = 100;
      	b = 200;
        c = 300;
        for (int k = 0, p = 0; k < 6; k++) {
            int m = 2;
      	    int n = 3;
      	    int z = 4;
      	    int f = 5;
            p = p + 1;
        }
        a = 1000;
        b = 2000;
        c = 3000;

        }    
  }

    
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string "-_-_-_-_-_-_-_-_-_-_- Cycles test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

        static void Main()
        {
           for(int i = 0; i < 3; i++)
           {
             for(int j = 0; j < 2; j++)
             {
               Console.WriteLine(i*j);
             }
           }

        }    
  }

    
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string "-_-_-_-_-_-_-_-_-_-_- Break test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

        static void Main()
        {
           for (int i = 0; i < 9; i++)
          {
              if (i == 5)
                  break;
              Console.WriteLine(i);
          }

        }    
  }

    
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string "-_-_-_-_-_-_-_-_-_-_- Continue test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

        static void Main()
        {
           for (int i = 0; i < 9; i++)
          {
              if (i == 5)
                  continue;
              Console.WriteLine(i);
          }

        }    
  }

    
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string "-_-_-_-_-_-_-_-_-_-_- While test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

        static void Main()
        {
           int i = 6;
            while (i > 0)
            {
                Console.WriteLine(i);
                i--;
            }

        }    
  }

    
        |})

let () = test_interp parse_input (Hashtbl.create 128)
let () = print_string "-_-_-_-_-_-_-_-_-_-_- If test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

        static void Main()
        {
           int num1 = 8;
          int num2 = 6;
          if(num1 > num2)
          {
              Console.WriteLine("num1");
          }

        }    
  }

    
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string "-_-_-_-_-_-_-_-_-_-_- If else if test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

        static void Main()
        {
           int num1 = 6;
            int num2 = 6;
            if(num1 > num2)
            {
                Console.WriteLine(1);
            }
            else if (num1 < num2)
            {
                Console.WriteLine(2);
            }
            else
            {
                Console.WriteLine(3);
            }

        }    
  }

    
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Recursion test FACTORIAL -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

        static void Main()
        {
        int res = Factorial(5);
        }    
        static int Factorial(int x)
        {
            if (x == 0)
            {
                return 1;
            }
            else
            {
                return x * Factorial(x - 1);
            }
        }

  }

    
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Recursion test Fibonachi -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

        static void Main()
        {
        int res = Fibonachi(18);
        }    
        static int Fibonachi(int n)
        {
            if (n == 0 || n == 1)
            {
                return n;
            }
            else
            {
                return Fibonachi(n - 1) + Fibonachi(n - 2);
            }
        }
  }

    
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Const fields test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

        static void Main()
        {
          Math m = new Math();
          m.PI = 4;

        }    
        
  }

  class Math {
    const int PI = 3;
}


    
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Const variables test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

        static void Main()
        {
          const int a = 3;
          a = 5;

        }    
        
  }
   
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Overloading test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

        static void Main()
        {
          ShittyExn ex = new ShittyExn();
          string res = ex.ToString();
          Console.WriteLine(ex.ToString());

        }    
        
  }

  public class ShittyExn : Exception
  {
    public override string ToString()
    {
      return "Hello";
    }
  }
   
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string "-_-_-_-_-_-_-_-_-_-_- Print test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

        static void Main()
        {
          ShittyExn a = new ShittyExn();
          int b = 3;
          Console.WriteLine(b > 2);
          Console.WriteLine(3);
          Console.WriteLine("Hello World");
          Console.WriteLine(a);
          
        }    
        
  }

  public class ShittyExn : Exception
  {
    public override string ToString()
    {
      return "Hello";
    }
  }
   
        |})

let () = test_interp parse_input (Hashtbl.create 128)
