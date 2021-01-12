open Opal
open Ast2

(* эта функция применяет парсер к строке *)
let apply p s = parse p (LazyStream.of_string s)

(* тут производится парсинг целых чисел и операция + - * / *)
let digits = spaces >> many1 digit => implode
let integer = digits => int_of_string
let temp_integer c = Int (int_of_string c)

(*промежуточный перевод*)

let some_integer = digits => temp_integer

(*возвращает int  в нужном формате и остается парсером*)

let floater =
  digits
  >>= fun fir ->
  token "." >> digits >>= fun sec -> return (float_of_string (fir ^ "." ^ sec))

let some_floater =
  digits
  >>= fun fir ->
  token "." >> digits
  >>= fun sec -> return (Float (float_of_string (fir ^ "." ^ sec)))

let reserved =
  [ "true"; "false"; "if"; "else"; "fi"; "for"; "while"; "do"; "done"; "forech"
  ; "end"; "local"; "return" ]

(*returns string which is not in reserved*)
let bash_ident =
  many alpha_num => implode
  >>= function s when List.mem s reserved -> mzero | s -> return s

let ident =
  spaces
  >> (letter <~> many alpha_num)
  => implode
  >>= function s when List.mem s reserved -> mzero | s -> return s

(*had to be like ident but точки тоже включает в слово*)
let ident_p =
  spaces
  >> (letter <~> many (alpha_num <|> one_of ['.']))
  => implode
  >>= function s when List.mem s reserved -> mzero | s -> return s

(*had to be like ident, but with points.*)

let some_ident =
  many (alpha_num <|> one_of ['.'])
  => implode
  >>= function s when List.mem s reserved -> mzero | s -> return (String s)

let parens = between (token "(") (token ")")
let add_op = token "+" >> return (fun x y -> Plus (x, y))
let sub_op = token "-" >> return (fun x y -> Minus (x, y))
let multi_op = token "*" >> return (fun x y -> Multi (x, y))
let div_op = token "/" >> return (fun x y -> Divide (x, y))
let and_op = token "&&" >> return (fun x y -> AndAr (x, y))
let or_op = token "||" >> return (fun x y -> OrAr (x, y))
let great_op = token ">" >> return (fun x y -> GreatAr (x, y))
let less_op = token "<" >> return (fun x y -> LessAr (x, y))
let ecGreat_op = token ">=" >> return (fun x y -> EcGreatAr (x, y))
let ecLess_op = token "=<" >> return (fun x y -> EcLessAr (x, y))
let ecualy_op = token "==" >> return (fun x y -> EcualyAr (x, y))

(*--------------------------------------------------------------------------------------------------------------------*)
(*Парсим const vari arifm*)

let rec pars_vari input =
  (pars_bracesEx <|> pars_array <|> pars_Simplevari) input

and pars_bracesEx input =
  ( token "$"
  >> between (token "{") (token "}") pars_variNot
  >>= fun var -> return (Braces var) )
    input

and pars_Simplevari input =
  (token "$" >> bash_ident >>= fun str -> return (SimpleVari str)) input

and pars_array input =
  ( bash_ident
  >>= fun str ->
  between (token "[") (token "]") arifm
  >>= fun index -> return (ArrayVari (str, index)) )
    input

and pars_variNot input = (pars_array <|> pars_SimplevariNot) input

and pars_SimplevariNot input =
  (bash_ident >>= fun str -> return (SimpleVari str)) input

(*----------*)
and vari_to_container input =
  (pars_vari >>= fun var -> return (Variable var)) input

and pars_container input =
  (vari_to_container <|> some_floater <|> some_integer <|> some_ident) input

and pars_container_arifm input =
  (pars_container >>= fun ar -> return (Container ar)) input

and arifm input =
  chainl1 arifmetic
    ( and_op <|> or_op <|> ecGreat_op <|> ecLess_op <|> great_op <|> less_op
    <|> ecualy_op )
    input

and arifmetic input = chainl1 term (add_op <|> sub_op) input
and term input = chainl1 factor (multi_op <|> div_op) input
and factor input = (parens arifm <|> pars_container_arifm) input

let local_vari_eqal input =
  ( token "local" >> spaces >> bash_ident
  >>= fun str -> return (LocalVari (SimpleVari str)) )
    input

let vari_eqal input =
  (*используем в парсере expr, для приравнивания*)
  ( local_vari_eqal
  <|> (bash_ident >>= fun str -> return (SimpleVari str))
  <|> pars_array )
    input

let rec vari_to_string var =
  match var with
  | SimpleVari x -> "SimpleVari(" ^ x ^ ")"
  | ArrayVari (name, index) ->
      "ArryVari(" ^ name ^ ", " ^ arifm_to_string index ^ ")"
  | ArrayDecl name -> "ArrayDeclar(" ^ name ^ ")"
  | Braces v -> "Barces(" ^ vari_to_string v ^ ")"
  | LocalVari v -> "Local(" ^ vari_to_string v ^ ")"

and container_to_string ct =
  match ct with
  | Int c -> "INT:" ^ string_of_int c
  | String d -> "STR:" ^ d
  | Float d -> "FLOAT:" ^ string_of_float d
  | Variable v -> "Vari( " ^ vari_to_string v ^ ")"

and arifm_to_string ex =
  match ex with
  | Plus (l, r) -> "Plus(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"
  | Minus (l, r) ->
      "Minus(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"
  | Multi (l, r) ->
      "Multi(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"
  | Divide (l, r) ->
      "Divide(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"
  | Container ct -> "Container(" ^ container_to_string ct ^ ")"
  | AndAr (l, r) ->
      "And(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")" (*a && b *)
  | OrAr (l, r) ->
      "Or(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")" (*a || b *)
  | GreatAr (l, r) ->
      "Great(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")" (*a > b *)
  | LessAr (l, r) ->
      "Less(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")" (*a < b *)
  | EcGreatAr (l, r) ->
      "EcGreat(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"
      (*a >= b *)
  | EcLessAr (l, r) ->
      "EcLess(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"
      (*a =< b *)
  | EcualyAr (l, r) ->
      "EcualY(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"

(*a == b *)

let containerOption_to_string d =
  match d with None -> "None" | Some x -> container_to_string x

(*
print_string "//////CONST+VARI TEST////////////\n"

let (str : string) = "${aa[1+${bb[2]}]}"


print_string (str ^ " = ")

print_string (containerOption_to_string (apply pars_container str))

let arifmOption_to_string d =
  match d with None -> "None" | Some x -> arifm_to_string x


print_string "\n//////ARIFM TEST////////////\n"

let (str : string) = "(3+5)==4.1 +${Path[1]}"


print_string (str ^ " = ")

print_string (arifmOption_to_string (apply arifm str))
*)
(*-----------------------Парсим WORD--------------------------------------------------------*)
let rec pars_word input =
  (wbrExpCont <|> wStringCont <|> wString <|> wbrExp) input

and wString input = (pars_container >>= fun con -> return (WString con)) input

and wbrExp input =
  (token "{" >> inBraceExp >>= fun brac -> token "}" >> return (WbrExp brac))
    input

and wStringCont input =
  ( pars_container
  >>= fun con ->
  pars_word_NoContainer >>= fun wor -> return (WStringCont (con, wor)) )
    input

and wbrExpCont input =
  ( token "{" >> inBraceExp
  >>= fun barc ->
  token "}" >> pars_word_NoBr >>= fun wor -> return (WbrExpCont (barc, wor)) )
    input

and pars_word_NoContainer input = (wbrExpCont <|> wbrExp) input
and pars_word_NoBr input = (wStringCont <|> wString) input

and inBraceExp input =
  ( many1 (pars_word >>= fun elem -> token "," >> return elem)
  >>= (fun lst -> pars_word >>= fun last -> return (InBracExpan (lst @ [last])))
  <|> (pars_word >>= fun only -> return (InBracExpan [only])) )
    input

let rec word_string wor =
  match wor with
  | WString c -> "WString(" ^ container_to_string c ^ ")"
  | WbrExp brac -> "WbrExp(" ^ brexp_to_string brac ^ ")"
  | WStringCont (con, brac) ->
      "WStringCont(" ^ container_to_string con ^ "| " ^ word_string brac ^ ")"
  | WbrExpCont (brac, con) ->
      "WbrExpCont(" ^ brexp_to_string brac ^ "|" ^ word_string con ^ ")"

and brexp_to_string bra =
  let rec wordlst_to_string = function
    | [] -> ""
    | e :: l -> word_string e ^ ", " ^ wordlst_to_string l in
  match bra with InBracExpan ls -> wordlst_to_string ls

let option_word_string arg =
  match arg with None -> "None" | Some x -> word_string x

(*
let ff = print_string "\n//////WORD TEST////////////\n"
let (str : string) = "ssss{${sds[1]},sss,}dsdsd"


print_string (str ^ " = ")

let ddddd = print_string (option_word_string (apply pars_word str))
*)
(*-----------------------------Парсим stroperator----------------------------------------------------------------------------*)

let rec pars_stroperator input =
  ( replaceEnd <|> replaceBeg <|> replaceAll <|> replaceFirst <|> cutEndMore
  <|> cutBegMore <|> cutEndLess <|> cutBegLess <|> picFromPosLeng <|> picFromPos
  <|> leng )
    input

and replaceEnd input =
  ( token "$" >> token "{" >> pars_container
  >>= fun str ->
  token "/" >> token "%" >> pars_container
  >>= fun patrn ->
  token "/" >> pars_container
  >>= fun rep -> token "}" >> return (ReplaceEnd (str, patrn, rep)) )
    input

and replaceBeg input =
  ( token "$" >> token "{" >> pars_container
  >>= fun str ->
  token "/" >> token "#" >> pars_container
  >>= fun patrn ->
  token "/" >> pars_container
  >>= fun rep -> token "}" >> return (ReplaceBeg (str, patrn, rep)) )
    input

and replaceAll input =
  ( token "$" >> token "{" >> pars_container
  >>= fun str ->
  token "/" >> token "/" >> pars_container
  >>= fun patrn ->
  token "/" >> pars_container
  >>= fun rep -> token "}" >> return (ReplaceAll (str, patrn, rep)) )
    input

and replaceFirst input =
  ( token "$" >> token "{" >> pars_container
  >>= fun str ->
  token "/" >> pars_container
  >>= fun patrn ->
  token "/" >> pars_container
  >>= fun rep -> token "}" >> return (ReplaceFirst (str, patrn, rep)) )
    input

and cutEndMore input =
  ( token "$" >> token "{" >> pars_container
  >>= fun str ->
  token "%" >> token "%" >> pars_container
  >>= fun substr -> token "}" >> return (CutEndMore (str, substr)) )
    input

and cutBegMore input =
  ( token "$" >> token "{" >> pars_container
  >>= fun str ->
  token "#" >> token "#" >> pars_container
  >>= fun substr -> token "}" >> return (CutBegMore (str, substr)) )
    input

and cutEndLess input =
  ( token "$" >> token "{" >> pars_container
  >>= fun str ->
  token "%" >> pars_container
  >>= fun substr -> token "}" >> return (CutEndLess (str, substr)) )
    input

and cutBegLess input =
  ( token "$" >> token "{" >> pars_container
  >>= fun str ->
  token "#" >> pars_container
  >>= fun substr -> token "}" >> return (CutBegLess (str, substr)) )
    input

and picFromPosLeng input =
  ( token "$" >> token "{" >> pars_container
  >>= fun str ->
  token ":" >> pars_container
  >>= fun pos ->
  token ":" >> pars_container
  >>= fun leng -> token "}" >> return (PicFromPosLeng (str, pos, leng)) )
    input

and picFromPos input =
  ( token "$" >> token "{" >> pars_container
  >>= fun str ->
  token ":" >> pars_container
  >>= fun pos -> token "}" >> return (PicFromPos (str, pos)) )
    input

and leng input =
  ( token "$" >> token "{" >> token "#" >> ident
  >>= fun str -> token "}" >> return (Leng (String str)) )
    input

(*---------------------------------Парсим expr-------------------------------------------------------------------------*)
let rec pars_arg input = (sub_arifm <|> sub_fn <|> string_op <|> word_p) input

and sub_arifm input =
  (token "$((" >> arifm >>= fun ar -> token "))" >> return (Subarifm ar)) input

and sub_fn input =
  (token "$(" >> pars_expr >>= fun fn -> token ")" >> return (SubFn fn)) input

and string_op input =
  (pars_stroperator >>= fun strop -> return (StringOp strop)) input

and word_p input = (pars_word >>= fun wd -> return (Word wd)) input

and pars_expr input =
  (*<<<<<<<<<<<<<<<<*)
  (eqal_e <|> call_e) input

and eqal_e input = (array_decl_eq <|> single_eqal) input

and single_eqal input =
  ( vari_eqal
  >>= fun variable ->
  token "=" >> pars_arg >>= fun ag -> return (Eqal (variable, [ag])) )
    input

and array_decl input =
  ( token "local" >> spaces >> bash_ident
  >>= (fun arr -> return (LocalVari (ArrayDecl arr)))
  <|> (bash_ident >>= fun str -> return (ArrayDecl str)) )
    input

and array_decl_eq input =
  ( array_decl
  >>= fun massiv ->
  token "=(" >> pars_list_arg_array
  >>= fun args -> token ")" >> return (Eqal (massiv, args)) )
    input

and pars_list_arg_array input =
  ( pars_arg
  >>= fun beg ->
  many1 (space >> spaces >> pars_arg) >>= fun lst -> return (beg :: lst) )
    input

and call_e input =
  ( bash_ident
  >>= fun nam ->
  many (space >> spaces >> pars_arg)
  (*pars_list_arg*)
  >>= fun arglst ->
  with_redirF nam arglst <|> with_redirL nam arglst
  <|> return (CallFunction (nam, arglst, None)) )
    input

and with_redirF nam arg input =
  ( token ">"
  >> (* > *)
  ident_p
  >>= fun red ->
  match arg with
  | [] -> return (CallFunction (nam, [], Some (Redirect (red, "", ""))))
  | _ :: _ -> return (CallFunction (nam, arg, Some (Redirect (red, "", "")))) )
    input

and with_redirL nam arg input =
  ( token "<"
  >> (* < *)
  ident_p
  >>= fun red ->
  match arg with
  | [] -> return (CallFunction (nam, [], Some (Redirect ("", red, ""))))
  | _ :: _ -> return (CallFunction (nam, arg, Some (Redirect ("", red, "")))) )
    input

let rec arg_to_string ar =
  match ar with
  | Subarifm x -> "Subarifm(" ^ arifm_to_string x ^ ")" (*$ 2 + 2 *)
  | SubFn ex -> "SubFn(" ^ expr_to_string ex ^ ")" (*$(...)*)
  | StringOp _ -> "StrngOP" ^ "did not done" ^ ")" (*${#string}*)
  | Word wd -> "Word(" ^ word_string wd ^ ")"

(*{a,b,c}, или string*)
and expr_to_string ex =
  match ex with
  | Eqal (var, ar) ->
      "Eqal(" ^ vari_to_string var ^ "," ^ arglst_to_string ar ^ ")" (* a=b*)
  | CallFunction (str, arglst, Some (Redirect (fir, sec, thir))) ->
      "CallFunction(" ^ str ^ ", " ^ "Listarg(" ^ arglst_to_string arglst
      ^ ")Redirect(" ^ fir ^ ", " ^ sec ^ ", " ^ thir ^ "))"
  | CallFunction (str, arglst, None) ->
      "CallFunction(" ^ str ^ ", " ^ "Listarg(" ^ arglst_to_string arglst
      ^ ")Redirect( NONE ))"

(*name, parametrs*)
and arglst_to_string = function
  | [] -> ""
  | a :: lis -> arg_to_string a ^ ", " ^ arglst_to_string lis

let option_expr_string = function None -> "NONE" | Some x -> expr_to_string x

(*
let ff = print_string "\n//////EXPR+ARG TEST////////////\n"

(*cat .w123.txt > txt.txt*)
let (str : string) = "ss=(111 33 ww $(cat .w123.txt))"


print_string (str ^ " <-> ")

let ddddd = print_string (option_expr_string (apply pars_expr str))
*)
(*-------------------------------------Парсим PIPE--------------------------------------------------------------------*)

let rec pars_pipeConv input =
  (parse_pipeline <|> single_pipe_conveyr >>= fun par -> return par) input

and single_pipe input =
  ( while_pars <|> ifelse_pars <|> foreach_pars <|> expr_pipe
  >>= fun par -> return par )
    input

and single_pipe_conveyr input =
  ( while_pars <|> ifelse_pars <|> foreach_pars <|> expr_pipe
  >>= fun par -> return (SigPipe par) )
    input

and parse_pipeline input = (p_dpoint <|> p_pipe >>= fun par -> return par) input

and p_dpoint input =
  ( single_pipe
  >>= fun fir ->
  spaces >> token ";" >> spaces >> pars_pipeConv
  >>= fun sec -> return (Pipline (Dpoint, fir, sec)) )
    input

and p_pipe input =
  ( single_pipe
  >>= fun fir ->
  spaces >> token "|" >> spaces >> pars_pipeConv
  >>= fun sec -> return (Pipline (Redi, fir, sec)) )
    input

and expr_pipe input = (pars_expr >>= fun ar -> return (Expression ar)) input

and while_pars input =
  ( token "while"
  >> (* while *)
  spaces >> token "((" >> spaces >> arifm
  >>= fun pred ->
  spaces >> token "))" >> spaces >> token "do" >> spaces
  >> (* do *)
  pars_pipeConv
  >>= fun act -> spaces >> token "done" >> return (While (pred, act)) )
    input

and ifelse_pars input =
  ( token "if"
  >> (* if *)
  spaces >> token "((" >> spaces >> arifm
  >>= fun pred ->
  spaces >> token "))" >> spaces >> token "then" >> spaces
  >> (* then *)
  pars_pipeConv
  >>= fun thn ->
  spaces >> token "else"
  >> (* else *)
  spaces >> pars_pipeConv
  >>= fun els -> spaces >> token "fi" >> return (IfElse (pred, thn, Some els))
  )
    input

and foreach_pars input =
  ( token "foreach"
  >> (* foreach *)
  spaces >> vari_eqal
  >>= fun variable ->
  (*variable*)
  spaces >> token "(" >> pars_list_arg_array (* in array*)
  >>= fun args ->
  token ")" >> spaces >> pars_pipeConv
  >>= fun action ->
  spaces >> token "end"
  >> (* end *)
  return (Foreach (variable, args, action)) )
    input

let rec pipe_to_string pip =
  (*let rec list_string = function
    [] -> ""
    | e::l -> e ^ " " ^ list_string l
    in*)
  match pip with
  | Expression ar -> "Expression(" ^ expr_to_string ar ^ ")"
  | While (pred, p) ->
      "While(" ^ arifm_to_string pred ^ "->act " ^ pipeConveyor_to_string p
      ^ ")"
  | IfElse (ar, l, Some r) ->
      "IFElse(" ^ arifm_to_string ar ^ ", " ^ pipeConveyor_to_string l ^ ", "
      ^ pipeConveyor_to_string r ^ ")"
  | IfElse (ar, l, None) ->
      "IFElse(" ^ arifm_to_string ar ^ ", " ^ pipeConveyor_to_string l
      ^ ", NONE )"
  | Foreach (var, arlst, act) ->
      "Foreach(" ^ vari_to_string var ^ ", List(" ^ arglst_to_string arlst
      ^ "), " ^ pipeConveyor_to_string act ^ ")"

and pipeConveyor_to_string conv =
  match conv with
  | SigPipe p -> "SigPipe(" ^ pipe_to_string p ^ ")"
  | Pipline (Dpoint, l, r) ->
      "Pipeline(Dpoint, " ^ pipe_to_string l ^ ", " ^ pipeConveyor_to_string r
      ^ ")"
  | Pipline (Redi, l, r) ->
      "Pipeline(Redi, " ^ pipe_to_string l ^ ", " ^ pipeConveyor_to_string r
      ^ ")"
  | _ -> "did not done yet"

let option_pipeConveyr_string pip =
  match pip with None -> "None!" | Some p -> pipeConveyor_to_string p

(*
let ff = print_string "\n//////PIPECONV TEST////////////\n"

let (str : string) =
  "while (($1 > 0)) do facto=$(($facto*$1)) ; 1=$(($1-1)) done"


print_string (str ^ "\n <-> \n")

let pp_str = "test () {pwd file.txt > my.txt; ss= 10+2}"
let ddddd = print_string (option_pipeConveyr_string (apply pars_pipeConv str))
*)

(*-------------------------------------Парсим DECLFUNTION----------------------------------------------------------*)
let pars_declFunction input =
  ( bash_ident
  >>= fun nam ->
  spaces >> token "(" >> token ")" >> spaces >> token "{" >> spaces
  >> pars_pipeConv
  >>= fun act -> spaces >> token "}" >> return (Method (nam, act)) )
    input

let declFunction_to_string dec =
  match dec with
  | Method (name, act) ->
      "DeclFunct(" ^ name ^ ">" ^ pipeConveyor_to_string act ^ ")"

let option_declFunction_string = function
  | None -> "NONE"
  | Some x -> declFunction_to_string x

(*
let ff = print_string "\n//////DeclFUNCTION TEST////////////\n"

(*funcFactor () { facto=1 ; while [$1 > 0] do facto=$facto*$1 ; 1=$1-1 done }*)
let (str : string) = "myfunction (){ echo www.txt ; cat w123.txt | echo yra }"


print_string (str ^ "\n <-> \n")

let ddddd =
  print_string (option_declFunction_string (apply pars_declFunction str))
*)
(*------------------------------Парсим BCOMMAND и BCMDCONV--------------------------------------------------------*)

let rec pars_bComdConv input = (b_conv <|> bCmd_sig_cmd) input
and b_sig_cmd input = (b_func <|> b_pipeConv) input
and bCmd_sig_cmd input = (b_sig_cmd >>= fun bcmd -> return (BSigCmd bcmd)) input

and b_func input =
  (spaces >> pars_declFunction >>= fun fn -> return (DeclFunct fn)) input

and b_pipeConv input =
  (spaces >> pars_pipeConv >>= fun conv -> return (PipeConv conv)) input

and b_conv input =
  ( b_sig_cmd
  >>= fun bcmd ->
  space >> pars_bComdConv >>= fun conv -> return (BConv (bcmd, conv)) )
    input

let bCommand_to_string = function
  | PipeConv x -> pipeConveyor_to_string x
  | DeclFunct x -> declFunction_to_string x

let rec bCmdConv_to_string bcmd =
  match bcmd with
  | BSigCmd x -> "BsigCmd(" ^ bCommand_to_string x ^ ")"
  | BConv (x, y) ->
      "BConv(" ^ bCommand_to_string x ^ "<>" ^ bCmdConv_to_string y ^ ")"

let option_bCmdConv_string = function
  | None -> "NONE"
  | Some x -> bCmdConv_to_string x

(*Итоговая функция парсера*)
let pars_mini_bash input = pars_bComdConv input

(*
let ff = print_string "\n//////BCmd+BCommand TEST////////////\n"

let (str : string) =
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


print_string (str ^ "\n <-> \n")

let ddddd = print_string (option_bCmdConv_string (apply pars_bComdConv str))
*)

(*--------------------------------------------------------------------------------------------------*)
let first = CallFunction ("echo", [Word (WString (String "eeee"))], None)
let second = SigPipe (Expression first)
let third = Method ("myfn", second)

let pred =
  BSigCmd
    (PipeConv
       (SigPipe
          (Expression
             (CallFunction ("echo", [Word (WString (String "rrr"))], None)))))

let result = BConv (DeclFunct third, pred)

let%test _ = apply pars_bComdConv "myfn (){echo eeee} echo rrr" = Some result
