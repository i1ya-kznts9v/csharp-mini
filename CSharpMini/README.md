### An implementaion of CSharp mini-language

This is a homework for functional programming course.

License: LGPL

Author: Kuznetsov Ilya Alexandrovich, kuznetsov.ilya.alexandrovich@gmail.com

Features done:

- 1 AST
- 2 Parser + tests + cram test

Features in progress:

- 1 Interpreter + tests + cram test
- 2 Other tasts

### Замечания по стилю кодирования

- Если merge request не проходит CI -- проверять не буду
- Имена типов и функций -- snake_case
- Имена типов модулей и модулей -- CamelCase 
- Ворнинги должны быть пофикшены
- Используйте quoted string literals в тестах, чтобы не экранировать руками 
```
─( 11:21:01 )─< command 1 >────────────────────────────
utop # {|
  int main () {
    return 0;
  }
  |};;
- : string = "\n  int main () {\n    return 0;\n  }\n  "
```