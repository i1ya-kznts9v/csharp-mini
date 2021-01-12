(*open Opal*)
(*open Bash_lib.Ast2*)
open Bash_lib.Parser

let pre_print = print_string "\n//////BCmd+BCommand TEST////////////\n"

let (str_bash : string) =
  "funcFactor ()\n\
   {\n\
   facto=1 ;\n\
   count=$1 ;\n\
   while (( $count > 0 ))\n\
   do\n\
   facto=$(($facto*$count)) ; count=$(($count-1))\n\
   done ; echo $facto\n\
   }\n\n\
   funcFactor 4"

let start_print = print_string (str_bash ^ "\n <-> \n")

let test_result =
  print_string (option_bCmdConv_string (apply pars_bComdConv str_bash))

(*-------------------*)

;;
print_string "\n//////WORD TEST////////////\n"

let (str_word : string) = "ssss{${sds[1]},sss,}dsdsd"

;;
print_string (str_word ^ " = ")
;;
print_string (option_word_string (apply pars_word str_word))

(*------------------*)

;;
print_string "\n//////PIPECONV TEST////////////\n"

let (str_pipe : string) = "foreach i ( 1 2 3 4 ) local s=$i ; echo $i end"

;;
print_string (str_pipe ^ "\n <-> \n")
;;
print_string (option_pipeConveyr_string (apply pars_pipeConv str_pipe))
