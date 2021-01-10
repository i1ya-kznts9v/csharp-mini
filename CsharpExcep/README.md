### An implementaion of Csharp+Exception mini-language

This is a homework for functional programming course.

License: LGPL

Author: Miroshnikov Vladislav, vladislaw.miroshnikov@gmail.com

Features done:

- 1 AST + переименовал типы
- 2 Parser
- 3 Убрал возможность парсинга массивов, так как их нет по заданию
- 4 Тесты на парсер + Cram test

Features in progress:

- 1 
- 2

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