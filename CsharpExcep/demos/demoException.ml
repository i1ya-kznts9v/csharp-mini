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
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Exception test in Main CASE 1 -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            public static void Main() {
                try
                  {
                    throw new Exception();
                  }
                  catch
                  {
                    Console.WriteLine("Handled");
                  }
                  finally
                  {
                    Console.WriteLine("In finally");
                  }
            }  
        }
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Exception test in Main CASE 2 -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            public static void Main() {
                int a =1;
                try
                  {
                    throw new Exception();
                  }
                  catch when (a == 1)
                  {
                    Console.WriteLine("Handled");
                  }
                  
            }  
        }
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Exception test in Main CASE 3 -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            public static void Main() {
                int a =1;
                try
                  {
                    throw new Exception();
                  }
                  catch (Exception)
                  {
                    Console.WriteLine("Handled");
                  }
                  
            }  
        }
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Exception test in Main CASE 4 -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            public static void Main() {
                int a =1;
                try
                  {
                    throw new Exception();
                  }
                  catch (Exception) when (a == 1)
                  {
                    Console.WriteLine("Handled");
                  }
                  finally
                  {
                    Console.WriteLine("In finally");
                  }
                  
            }  
        }
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Exception test in Main CASE 5 -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            public static void Main() {
                int a =1;
                try
                  {
                    throw new Exception();
                  }
                  catch (Exception e) 
                  {
                    Console.WriteLine("Handled");
                  }
                  finally
                  {
                    Console.WriteLine("In finally");
                  }
                  
            }  
        }
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Exception test in Main CASE 6 -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            public static void Main() {
                int a =1;
                try
                  {
                    throw new Exception();
                  }
                  catch (Exception e) when (a == 1)
                  {
                    Console.WriteLine("Handled");
                  }
                  finally
                  {
                    Console.WriteLine("In finally");
                  }
                  
            }  
        }
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Exception test without new in throw \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            public static void Main() {
                int a =1;
                try
                  {
                    Exception ex = new Exception();
                    ex.message = "FUNCTIONAL PROGRAMMING THE BEEEST!";
                    throw ex;
                  }
                  catch (Exception e) when (a == 1)
                  {
                    Console.WriteLine(e.message);
                  }
                  finally
                  {
                    Console.WriteLine("In finally");
                  }
                  
            }  
        }
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string "-_-_-_-_-_-_-_-_-_-_- Try in try test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            public static void Main() {
                int a =1;
                try
                  {
                    try
                      {
                        throw new Exception();
                      }
                      catch
                      {
                        Console.WriteLine("DER");
                      }
                  }
                  catch (Exception e) when (a == 1)
                  {
                    Console.WriteLine("Inter");
                  }
                  finally
                  {
                    Console.WriteLine("In finally");
                  }
                  
            }  
        }
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Exception in external method -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            public static void Main() {
                int a =1;
                try
                  {
                    Thqrow();
                  }
                  catch (Exception e) 
                  {
                    Console.WriteLine(a);
                  }
                  finally
                  {
                    Console.WriteLine("In finally");
                  }
                  
            }  

            public static void Thqrow()
            {
                  throw new Exception();
            }
        }
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Unhandled exception test without filter \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            public static void Main() {
                int a =1;
                try
                  {
                    Thqrow();
                  }
                 catch(Shitty)
                 {

                 }
                  finally
                  {
                    Console.WriteLine("In finally");
                  }
                  
            }  

            public static void Thqrow()
            {
                  throw new Exception();
            }
        }
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Unhandled exception test with filter \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            public static void Main() {
                int a =1;
                try
                  {
                    Thqrow();
                  }
                  catch(Exception) when (a == 3)
                  {

                  }
                  finally
                  {
                    Console.WriteLine("In finally");
                  }
                  
            }  

            public static void Thqrow()
            {
                  throw new Exception();
            }
        }
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Return with finally test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            public static void Main() {
                
                  int rw = VVAA();
                  int a = 5;
                  Console.WriteLine(rw);
                  Console.WriteLine(3);
                  Console.WriteLine(rw);
            }  
	
          static int VVAA()
          {
            int a = 1;
            try
            {
              a = 2;
              return a;
            }
            finally
            {
              a = 3;
            }
            
          }
        }
        |})

let () = test_interp parse_input (Hashtbl.create 128)
let () = print_string "-_-_-_-_-_-_-_-_-_-_- Test 1 -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            static void Main()
        {
            Console.WriteLine(foo());
        }

        static int foo() {
            int x = 5;
            try {
                return x;
            } finally {
                x = 200;
            }
        }
        }
        |})

let () = test_interp parse_input (Hashtbl.create 128)
let () = print_string "-_-_-_-_-_-_-_-_-_-_- Test 2 -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

            static void Main()
        {
            E3();
        }


        public static void A3()
        {
            try
            {
                throw new ShittyExn(true);
            }
            finally
            {
                
            }
        }
        public static void B3()
        {
            try
            {
                A3();
            }
            catch (ShittyExn e) when (false)
            {
                Console.WriteLine("B");
            }
        }
        public static void C3()
        {
            try
            {
                B3();
            }
            catch (ShittyExn e) when (false)
            {
                Console.WriteLine("C");
            }
        }
        public static void D3()
        {
            try
            {
                C3();
            }
            catch (ShittyExn e) when (false)
            {
                Console.WriteLine("D");
            }
        }
        public static void E3()
        {
            try
            {
                D3();
            }
            catch (ShittyExn e) when (e.Filter())
            {
                Console.WriteLine("E");
            }
        }
    }

    class ShittyExn : Exception
        {
            public bool f;
            public ShittyExn(bool val)
            {
               f = val;
            }

            public bool Filter()
            {
                return f;
            }
        }
        |})

let () = test_interp parse_input (Hashtbl.create 128)
let () = print_string "-_-_-_-_-_-_-_-_-_-_- Test 3  -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program {

        static void Main()
        {
             TrickyTest21();
        }

        public static bool Filter(string msg)
       	{
	        Console.WriteLine(msg);
            return true;
      	}

       public static void A21()
        {
            try
            {
                throw new Exception();
            }
            finally
            {
                Console.WriteLine("A21.finally");
                throw new Exception();
            }
            Console.WriteLine("Resuming A2");
        }

       public static void B21()
        {
            try
            {
                A21();
            }
            catch (Exception) when (Filter("filter B21"))
            {
                Console.WriteLine("B21 Exception");
            }
            finally
            {
                Console.WriteLine("B21.finally");
            }

            Console.WriteLine("Resuming B21");
        }

        public static void TrickyTest21()
        {
            try
            {
                B21();
            }
            catch
            {
              Console.WriteLine("Handled");
            }
			
            finally
            {
                Console.WriteLine("TrickyTest Finally");
            }

            Console.WriteLine("Resuming TrickyTest");
        }
     }
        |})

let () = test_interp parse_input (Hashtbl.create 128)
