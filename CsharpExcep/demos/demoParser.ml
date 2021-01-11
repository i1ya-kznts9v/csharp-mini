open Csharp_lib.Ast
open Csharp_lib.Parser

let print_list =
  Format.pp_print_list Format.pp_print_string Format.std_formatter

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

let run_test = print_list (List.map show_cs_class parse_input)
