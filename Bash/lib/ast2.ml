(*............................*)
(*............................*)
(*............................*)
(*............................*)
(*............................*)
(*............................*)
(*............................*)
(*........AST....................*)
type vari =
  (*Variable; SimpleVari => $s*)
  | SimpleVari of string
  (*обращение к массиву; ArrayVari => array[1+1] или array[$s]*)
  | ArrayVari of string * arifm
  (*объявление массива и присваивание ему чего-то; ArrayDecl => array - он появляется только в 1 случае: array=(hello world)*)
  | ArrayDecl of string
  (*${ss}, но не ${$s}*)
  | Braces of vari
  (*для объявления локальных переменных и массивов*)
  | LocalVari of vari

and container =
  | Int of int
  | Float of float
  | String of string
  | Variable of vari

(*редирект ввода и вывода*)
and redirect = Redirect of string * string * string

and stroperator =
  (*${#string}*)
  | Leng of container
  (*$ {string: position} *)
  | PicFromPos of container * container
  (*${string:position:length}*)
  | PicFromPosLeng of container * container * container
  (*${string#substring}*)
  | CutBegLess of container * container
  (*${string%substring}*)
  | CutEndLess of container * container
  (*${string##substring}*)
  | CutBegMore of container * container
  (*${string%%substring}*)
  | CutEndMore of container * container
  (*${string/pattern/replacement}*)
  | ReplaceFirst of container * container * container
  (*${string//pattern/replacement}*)
  | ReplaceAll of container * container * container
  (*${string/#pattern/replacement}*)
  | ReplaceBeg of container * container * container
  (*${string/%pattern/replacement}*)
  | ReplaceEnd of container * container * container

and arifm =
  (* means a + b *)
  | Plus of arifm * arifm
  (* means a - b *)
  | Minus of arifm * arifm
  (* means a * b *)
  | Multi of arifm * arifm
  (* means a / b *)
  | Divide of arifm * arifm
  (* "x", "y", "n", etc. *)
  | Container of container
  (*a && b *)
  | AndAr of arifm * arifm
  (*a || b *)
  | OrAr of arifm * arifm
  (*a > b *)
  | GreatAr of arifm * arifm
  (*a < b *)
  | LessAr of arifm * arifm
  (*a >= b *)
  | EcGreatAr of arifm * arifm
  (*a =< b *)
  | EcLessAr of arifm * arifm
  (*a == b *)
  | EcualyAr of arifm * arifm

(*{a,b,c}*)
and brExpan = InBracExpan of word list

(*string{a,b,c}string2{d,f,g} ИЛИ просто строка или значение переменной*)
and word =
  | WString of container
  | WbrExp of brExpan
  | WStringCont of container * word
  | WbrExpCont of brExpan * word

(*Аргумент функции*)
and arg =
  (*$(...)*)
  | SubFn of expr
  (*${#string}*)
  | StringOp of stroperator
  (*{a,b,c}, или string*)
  | Word of word
  (*$((арифметическое выражение))*)
  | Subarifm of arifm

(*вызов функции или присваивание*)
and expr =
  (*a=b, список для массива array = (hello world), в ином случае список из 1 элемента*)
  (*Equal (...,[]) - присваивание ничего в переменную или в массив*)
  | Eqal of vari * arg list
  (*имя функции, список аргументов, редирект ввода вывода(редиректа нет - он содержит пустые строки)*)
  | CallFunction of string * arg list * redirect option

(*разделители меджду пайпами (пайпа - 1 команда, например echo hello или variable=42 или весь цикл while)*)
and pipeOp = And | Or | Dpoint | Redi

(*пайпа*)
and pipe =
  | Expression of expr
  (*while (()) do done*)
  | While of arifm * pipeConveyor
  (*if (()) then else*)
  | IfElse of arifm * pipeConveyor * pipeConveyor option
  (*variable, list of values, todoList*)
  | Foreach of vari * arg list * pipeConveyor

(*множество пайп разделенных pipeOP, в случае 1 пайпы - без разделителья*)
and pipeConveyor = Pipline of pipeOp * pipe * pipeConveyor | SigPipe of pipe

(*так объявляется функция*)
type declFunct = Method of string * pipeConveyor

(*либо множество пайп либо объявление функции*)
type bCommand = PipeConv of pipeConveyor | DeclFunct of declFunct

(*по аналогии с множеством пайп - множество команд. Это итоговый тип, который возвращается парсером*)
type bCmdConv =
  | BConv of bCommand * bCmdConv (*конвеер функций и кода*)
  | BSigCmd of bCommand

(*...........AST.................*)
(*............................*)
(*............................*)
(*............................*)
(*............................*)
