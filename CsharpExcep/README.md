### An implementaion of Csharp+Exception mini-language

This is a homework for functional programming course.

License: LGPL

Author:  Miroshnikov Vladislav, vladislaw.miroshnikov@gmail.com

Замечания:
- 1 Метод Main ничего не принимает, то есть никаких string[] args, потому что массивов и по заданию нет
- 2 Ключевые слова не могут являться именами классов
- 3 i++ и ++i эквиваленты i = i + 1
- 4 StackOverflow не детектится
- 5 foreach не надо, вложенные классы друг в друга не надо
- 6 Строки как объекты делать не нужно, StartsWith не нужен

Features done:

- 1 AST + переименовал типы
- 2 Parser
- 3 Убрал возможность парсинга массивов, так как их нет по заданию
- 4 Тесты на парсер + Cram test
- 5 Загрузка классов
- 6 Стандартные конструкции: ветвления, циклы
- 7 Стандартные типы: числа, строки и операции с ними
- 8 Рекурсия
- 9 Класс Exception
- 10 ООП: классы, публичные методы, публичные поля
- 11 ООП: наследование только от класса Exception
- 12 Работа с исключениями, конструкции try/catch/finally/filter
- 13 Функция печати на консоль
- 13 Полный набор тестов, и на конструкции мини-языка, и на исключения 
- 14 ДОП: полиморфизм подтипов (включения) при присваивании объектов
- 15 REPL + stdlib (стандартная библиотека классов) - в стандартной библиотеке помимо классов на вычисление факториала и фибонначи есть класс пользовательского исключения DivideByZeroException, а также два класса HandleException и UnhandleException, эмулирующие работу с исключениями.
- 16 Pretty-Printer

Работа с REPL: Факториал
```
REPL # int fac1(int acc, int n) { if (n<=1) return acc; else return fac1(acc*n, n-1);}@

REPL # int fac(int n) {return fac1(1,n);}@

REPL # fac(6)@

REPL # Result: VInt (720)
```

Работа с REPL: HandleException класс
```
REPL # HandleException exr = new HandleException();@

REPL # exr.Handle();@
Exception handled
IT's ZERO!
Statement interpreted
```

Работа с REPL: UnhandleException класс
```
REPL # UnhandleException ex = new UnhandleException();@

REPL # ex.Unhandle();@
Unhandled exception
```

Работа с REPL: Выброшенное исключение через throw
```
REPL # throw new DivideByZeroException();@

Unhandled exception
```

Работа с REPL: Несколько выражений сразу - нужно оборачивать в { }
```
REPL # {Console.WriteLine(1); Console.WriteLine(1);}@

```

Класс Exception : 
```
 public class Exception 
 {
    public string message;

   public string ToString()
   {
     return message;
   }
   
  }
```
Features in progress:

- 1 Трансформации

##### Замечания по стилю кодирования

Пока оно не проверяется в CI, но может неожиданно начать проверяться.

- если merge request не проходит CI -- проверять не буду
- имена типов и функций -- snake_case
- имена типов модулей и модулей -- CamelCase 
- ворнинги должны быть пофикшены
- используйте quoted string literals в тестах, чтобы не экранировать руками 
```
─( 11:21:01 )─< command 1 >────────────────────────────
utop # {|
  int main () {
    return 0;
  }
  |};;
- : string = "\n  int main () {\n    return 0;\n  }\n  "
```
- 1