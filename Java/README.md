### An implementaion of Java-OOP mini-language

This is a homework for functional programming course.

License: LGPL

Author: Bozhnyuk Alexander, bozhnyuks@mail.ru

Замечания. 
- 1) Ключевые слова не могут являться именами классов
- 2) Метод main ничего в себя не принимает, является просто точкой входа программы. Метод main - единственный.
- 3) i++ и ++i эквиваленты i = i + 1. Ничего более
- 4) StackOverflow не детектится
- 5) Многомерные массивы не поддерживаются
- 6) Приведение типов через () не поддерживается. 
- 7) Метод equals при сравнении объектов вызывается в случае, если он переопределен. В противном случае происходит непосредственное сравнение ссылок.
- 8) Работа с реплом: запуск факториала 
    ```
    > int fac1(int acc, int n) {if (n <= 1) return acc; else return fac1(acc * n, n - 1); }@
    > int fac(int n) {return fac(1, n);}@
    > fac(5)@
    > Result: VInt (120)
    ```
- 9) Работа с реплом: ArrayTypeMismatchException: 
    ```
    > Object[] x = new Object[3];@    
    Statement evaluated
    > x[0] = new Circle(5);@
    ArrayStoreException
    > Object[] y = new Figure[3];@
    Statement evaluated
    > y[0] = new QuickSorter();@
    Wrong assign type!

    ```
- 10) Работа с реплом: show_available_methods@
    ```
    > int id(int n) {return n;}@                    
    Method added
    > int square(int n) {return n*n;}@
    Method added
    > show_available_methods@
    Current available methods:
    "int square(int n) {return n*n;}"
    "int id(int n) {return n;}"
    ```

Features done:

- 1 Загрузка классов
- 2 Стандартные конструкции: ветвления, циклы
- 3 Стандартные типы: числа, строки и операции с ними
- 4 Стандартный тип массива и операции с ним.
- 5 ООП: классы, публичные методы, публичные поля.  
- 6 ООП: стандартный базовый класс Object
- 7 ООП: наследование
- 8 Рекурсия
- 9 Тесты, в том числе и тест паттерна Visitor, требуемый тест на массивы
- 10 REPL и набор классов для стандартной библиотеки.
- 11 Типы данных - boolean и char как полноценные
- 12 Cтроки - теперь полноценные объекты класса String, загружаемого заранее.

Класс Object: 
```
public class Object {
    public boolean equals(Object obj) {
        return this == obj;
    }
    
    public String toString() {
    	return "Object";
    }
}
```

Класс String:
```
final class String {
    public final char[] value;

    public String() {
        this.value = new char[0];
    }

    public String(String original) {
        this.value = original.value;
    }

    public String(char[] value) {
        this.value = new char[value.length];
        for (int i = 0; i < value.length; i++) {
            this.value[i] = value[i];
        }
    }

    public int length() {
        return value.length;
    }

    public String concat(String str) {
        int otherLen = str.length();
        if (str.length() == 0) {
            return this;
        }
        int len = value.length;
        char[] newValue = new char[len + otherLen];
        for (int i = 0; i < len; i++) {
            newValue[i] = value[i];
        }
        for (int j = len; j < len + otherLen; j++) {
            newValue[j] = str.value[j - len];
        }
        return new String(newValue);
    }

    public boolean startsWith(String prefix, int toffset) {
        char[] ta = value;
        char[] pa = prefix.value;
        int pc = prefix.length();
        if ((toffset < 0) || (toffset > value.length - pc)) {
            return false;
        }
        for (int i = toffset; i < toffset + pc; i++) {
            if (ta[i] != pa[i - toffset]) {
                return false;
            }
        }
        return true;
    }

    public boolean startsWith(String prefix) {
        return startsWith(prefix, 0);
    }
}
```

Features in progress:

- 1 Правки
- 2 Pretty-printer
- 3 Трансформации



