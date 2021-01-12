open Csharp_lib.Parser
open Csharp_lib.Pretty_printer

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Pretty Printer test -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
        public class Program 
    {

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
                Console.WriteLine("Hello");
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
            catch (Exception)
            {

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

let () = print_pretty parse_input
