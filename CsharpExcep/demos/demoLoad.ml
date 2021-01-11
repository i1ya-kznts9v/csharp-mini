open Csharp_lib.Parser
open Csharp_lib.Interpreter
open Csharp_lib.Interpreter.ClassLoader (Csharp_lib.Interpreter.Result)
open Csharp_lib.Hashtbl_der

let show_hashtbl ht pp_k pp_el = pp pp_k pp_el Format.std_formatter ht
let show_class_table ht = show_hashtbl ht pp_table_key pp_table_class

let test_load prog prog_table =
  match load_classes prog prog_table with
  | Error m -> print_endline m ; Hashtbl.clear prog_table
  | Ok cl_t -> show_class_table cl_t ; Hashtbl.clear cl_t

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Inheritance testing from Exception class \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|  
    class Program
    {
        static void Main()
        {
            E3();
        }

        public static void A3(int a, string b)
        {
            int x = 0;
            try
            {
                throw new cat();
                
            }
            finally
            {
                Console.WriteLine(x);
            }
            
        }

        public static void B3()
        {
            try
            {
                A3();
            }
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            
            public ShittyExn()
            {
                f = f;
            }

            public bool Filter()
            {
                return f();
            }
        }
       
     |})

let () = test_load parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Incorrect modifiers testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|  
    class Program
    {
        static void Main()
        {
            E3();
        }

        public const static void A3(int a, string b)
        {
            int x = 0;
            try
            {
                throw new cat();
                
            }
            finally
            {
                Console.WriteLine(x);
            }
            
        }

        public static void B3()
        {
            try
            {
                A3();
            }
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            
            public ShittyExn()
            {
                f = f;
            }

            public bool Filter()
            {
                return f();
            }
        }
       
     |})

let () = test_load parse_input (Hashtbl.create 128)

let parse_input =
  Option.get
    (apply_parser parser
       {|  
    static class Program
    {
        static void Main()
        {
            E3();
        }

        public static void A3(int a, string b)
        {
            int x = 0;
            try
            {
                throw new cat();
                
            }
            finally
            {
                Console.WriteLine(x);
            }
            
        }

        public static void B3()
        {
            try
            {
                A3();
            }
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            
            public ShittyExn()
            {
                f = f;
            }

            public bool Filter()
            {
                return f();
            }
        }
       
     |})

let () = test_load parse_input (Hashtbl.create 128)

let parse_input =
  Option.get
    (apply_parser parser
       {|  
    class Program
    {
        static void Main()
        {
            E3();
        }

        public static void A3(int a, string b)
        {
            int x = 0;
            try
            {
                throw new cat();
                
            }
            finally
            {
                Console.WriteLine(x);
            }
            
        }

        public static void B3()
        {
            try
            {
                A3();
            }
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            
            public static ShittyExn()
            {
                f = f;
            }

            public bool Filter()
            {
                return f();
            }
        }
       
     |})

let () = test_load parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Similar fields error testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|  
    class Program
    {
        static void Main()
        {
            E3();
        }

        public static void A3(int a, string b)
        {
            int x = 0;
            try
            {
                throw new cat();
                
            }
            finally
            {
                Console.WriteLine(x);
            }
            
        }

        public static void B3()
        {
            try
            {
                A3();
            }
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            int age;
            int age;
            public ShittyExn()
            {
                f = f;
            }

            public bool Filter()
            {
                return f();
            }
        }
       
     |})

let () = test_load parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Similar methods error testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|  
    class Program
    {
        static void Main()
        {
            E3();
        }

        public static void A3(int a, string b)
        {
            int x = 0;
            try
            {
                throw new cat();
                
            }
            finally
            {
                Console.WriteLine(x);
            }
            
        }

        public static void A3(int a, string b)
        {
            
            
        }

        public static void B3()
        {
            try
            {
                A3();
            }
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
       
     |})

let () = test_load parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Constructor name error -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|  
    class Program
    {
        static void Main()
        {
            E3();
        }

        public static void A3(int a, string b)
        {
            int x = 0;
            try
            {
                throw new cat();
                
            }
            finally
            {
                Console.WriteLine(x);
            }
            
        }

        public static void B3()
        {
            try
            {
                A3();
            }
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            int age;
            
            public Something()
            {
                f = f;
            }

            public bool Filter()
            {
                return f();
            }
        }
       
     |})

let () = test_load parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Similar constructor error testing \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|  
    class Program
    {
        static void Main()
        {
            E3();
        }

        public static void A3(int a, string b)
        {
            int x = 0;
            try
            {
                throw new cat();
                
            }
            finally
            {
                Console.WriteLine(x);
            }
            
        }

        public static void B3()
        {
            try
            {
                A3();
            }
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            int age;
            
            public ShittyExn()
            {
                f = f;
            }

             public ShittyExn()
            {
                
            }

            public bool Filter()
            {
                return f();
            }
        }
       
     |})

let () = test_load parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Similar Classes error -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|  
    class Program
    {
        static void Main()
        {
            E3();
        }

        public static void A3(int a, string b)
        {
            int x = 0;
            try
            {
                throw new cat();
                
            }
            finally
            {
                Console.WriteLine(x);
            }
            
        }

        public static void B3()
        {
            try
            {
                A3();
            }
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            int age;
            
            public ShittyExn()
            {
                f = f;
            }

            

            public bool Filter()
            {
                return f();
            }
        }

        class ShittyExn : Exception
        {
            int age;
            
            public ShittyExn()
            {
                f = f;
            }

            

            public bool Filter()
            {
                return f();
            }
        }
       
     |})

let () = test_load parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- The class can only be inherited from the Exception \
     class error -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|  
    class Program
    {
        static void Main()
        {
            E3();
        }

        public static void A3(int a, string b)
        {
            int x = 0;
            try
            {
                throw new cat();
                
            }
            finally
            {
                Console.WriteLine(x);
            }
            
        }

        public static void B3()
        {
            try
            {
                A3();
            }
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
                class ShittyExn : Cat
        {
            int age;
            
            public ShittyExn()
            {
                f = f;
            }

            

            public bool Filter()
            {
                return f();
            }
        }

       
       
     |})

let () = test_load parse_input (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Override errors testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|  
    class Program
    {
        static void Main()
        {
            E3();
        }

        public static void A3(int a, string b)
        {
            int x = 0;
            try
            {
                throw new cat();
                
            }
            finally
            {
                Console.WriteLine(x);
            }
            
        }

        public static void B3()
        {
            try
            {
                A3();
            }
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            int age;
            
            public ShittyExn()
            {
                f = f;
            }

            public override string ToString()
            {

            }

            public bool Filter()
            {
                return f();
            }
        }

       
       
     |})

let () = test_load parse_input (Hashtbl.create 128)

let parse_input =
  Option.get
    (apply_parser parser
       {|  
    class Program
    {
        static void Main()
        {
            E3();
        }

        public static void A3(int a, string b)
        {
            int x = 0;
            try
            {
                throw new cat();
                
            }
            finally
            {
                Console.WriteLine(x);
            }
            
        }

        public static void B3()
        {
            try
            {
                A3();
            }
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            catch (ShittyExn e) when (e.Filter())
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
            int age;

            public ShittyExn()
            {
                f = f;
            }

            public override int Sum()
            {
              
            }

            public bool Filter()
            {
                return f();
            }
        }

       
       
     |})

let () = test_load parse_input (Hashtbl.create 128)
