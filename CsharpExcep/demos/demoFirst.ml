open Csharp_lib.Ast
open Csharp_lib.Parser

let print_list =
  Format.pp_print_list Format.pp_print_string Format.std_formatter

let parse_input =
  Option.get
    (apply_parser parser
       {|  class Program
    {
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
                throw new DivideByZeroException();
            }
            finally
            {
                Console.WriteLine("A21.finally()");
                throw new NullReferenceException();
            }
            Console.WriteLine("Resuming A2");
        }

        public static void B21()
        {
            try
            {
                A21();
            }
            catch (DivideByZeroException) when (Filter("filter B21"))
            {
                Console.WriteLine("B21 DivideByZeroException");
            }
            finally
            {
                Console.WriteLine("B21.finally()");
            }

            Console.WriteLine("Resuming B21");
        }

        public static void TrickyTest21()
        {
            try
            {
                B21();
            }
            finally
            {
                Console.WriteLine("TrickyTest Finally");
            }

            Console.WriteLine("Resuming TrickyTest");
        }
    } |})

let run_test = print_list (List.map show_cs_class parse_input)
