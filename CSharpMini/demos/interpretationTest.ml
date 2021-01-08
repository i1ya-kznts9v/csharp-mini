open Csharpmini_lib.Parser
open Csharpmini_lib.Interpreter.ClassLoader (Csharpmini_lib.Interpreter.Result)

open Csharpmini_lib.Interpreter.Interpretation
       (Csharpmini_lib.Interpreter.Result)

let interpret_test program class_table =
  match load program class_table with
  | Error message -> print_endline message ; Hashtbl.clear class_table
  | Ok loaded -> (
    match execute loaded with
    | Error message -> print_endline message ; Hashtbl.clear loaded
    | Ok result ->
        print_endline (show_context result ^ "\n") ;
        Hashtbl.clear loaded )

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- \"God, let it work!\" testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    int a = 1;
    int b = 2;
    int c = 3;
  }  
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Arithmetic testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    int a = 1;
    int b = 2;
    int c = 3;
    int val1 = 1 + 2 + 3 + 4 + 5;
    int val2 = a + b;
    int val3 = a + 100;
    int val4 = 10 / 2;
    int val5 = 10 % 2;
    int val6 = (a + b) * 100; 
    a = a + 1;
    int val7 = (val1 * val2 + 4) / 2 + 100; 
    string s1 = "a";
    string s2 = "b";
    string s3 = s1 + s2;
    string s4 = s1 + a;
    string s5 = a + s2;
  }  
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Bool expressions testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    int andVal = 0, orVal = 0, notVal = 0;
    int mVal = 0, meVal = 0, lVal = 0, leVal = 0;
    int eVal = 0, neVal = 0;
    
    int a = 10, b = 50, c = 100;
    if (b > a)
    {
      mVal = 1;
    }
    if (a < c)
    {
      lVal = 1;
    } 
    if (b <= c)
    {
      leVal = 1;
    }
    if (c >= b)
    {
      meVal = 1;
    }
    int d = 10;
    if (d == a)
    {
      eVal = 1;
    }
    if (a != b)
    {
      neVal = 1;
    }
    if (a < b && b < c)
    {
      andVal = 1;
    }
    if (a < b || a != 10)
    {
      orVal = 1;
    }
    if (!(a >= c))
    {
      notVal = 1;
    }
    string s1 = "a", s2 = "b";
    int sEq = 0, sNEQ; 
    if (s1 == "a")
    {
      sEq = 1;
    }
    if (s2 != "a")
    {
      sNEQ = 1;
    }
    Person p1 = new Person(20, "Bob"), p2 = new Person(30, "Alice"), p3 = p1;
    int objEq = 0, objNEq = 0; 
    if (p1 != p2)
    {
      objNEq = 1;
    }
    if (p1 == p3)
    {
      objEq = 1;
    }
  }  
}

public class Person
{
  int age;
  string name;

  public Person(int age, string name)
  {
    this.age = age;
    this.name = name;
  }
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Method call testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Person person = new Person(25, "Bob");
    int res = person.Sum(25, 100);
    int a1 = person.GetAge();
    person.SetAge(30);
    int a2 = person.GetAge(); 
  }
}

public class Person
{
  int age;
  string name;

  public Person()
  {
    
  }

  public Person(int age, string name)
  {
    this.age = age;
    this.name = name;
  }

  public int Sum(int a, int b)
  {
    return a + b;
  }

  public int GetAge()
  {
    return this.age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Update object state testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Person person = new Person(25, "Bob");
    Person p1, p2, p3;
    p1 = person;
    p2 = p1;
    p3 = p2;
    person.SetAge(55);
    int res = p2.GetAge(); 
  }
}

public class Person
{
  int age;
  string name;

  public Person()
  {

  }

  public Person(int age, string name)
  {
    this.age = age;
    this.name = name;
  }

  public int Sum(int a, int b)
  {
    return a + b;
  }

  public int GetAge()
  {
    return this.age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Inheritance testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Person person = new Person(25, "Bob");
    Person childFirst = new Child(3, "Alice");
    Child childSecond = new Child(person);
    childSecond.SetAge(20);
    childFirst.SetAge(4);
    person.SetAge(27);
  }
}

public class Person
{
  int age;
  string name;

  public Person()
  {

  }

  public Person(int age, string name)
  {
    this.age = age;
    this.name = name;
  }

  public int Sum(int a, int b)
  {
    return a + b;
  }

  public int GetAge()
  {
    return this.age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}

public class Child : Person
{
  public Person parent;

  public Child(int age, string name) : base(age, name)
  {
    parent = new Person(40, "Flexer");
  }

  public Child(Person parent)
  {
    this.parent = parent;
  }

  public Child()
  {

  }

  public int GetName()
  {
    return name;
  }

  public Person GetParent()
  {
    return parent;
  }
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string "-_-_-_-_-_-_-_-_-_-_- Scope testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    int a = 2;
    int b = 3;
    int c = 4;
    if (c == 4) 
    {
      int d = 7;
      a = a + 1;
      int e = 10;
      int g = 42;
    }
    b = 5;
    c = 6;
    a = 15;
    int i = 0;
    while (i < 3)
    {
      int m = 2;
      int n = 3;
      int z = 4;
      int f = 5;
      i = i + 1;
    }
    a = 100;
    b = 200;
    c = 300;
    for (int k = 0, p = 0; k < 6; k++)
    {
      int m = 2;
      int n = 3;
      int z = 4;
      int f = 5;
      p = p + 1;
    }
    a = 1000;
    b = 2000;
    c = 3000;
  }
}   
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Many loops and array bubble sorting testing \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    int[] arr = new int[] {10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
    int n = 11;
    for (int i = 0; i < n - 1; i++)
    {
      for (int j = 0; j < n - i - 1; j++)
      {
        if (arr[j] > arr[j + 1])
        {
          int temp = arr[j];
          arr[j] = arr[j + 1];
          arr[j + 1] = temp;
        }
      }
    }
  }
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Break and continue testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    int[] arr = new int[10];
    for (int i = 0; i < 10; ++i)
    {
      if (i % 2 == 0)
      {
        continue;
      }
      arr[i] = 1;
    }
    int[] b = new int[20];
    for (int i = 0; i < 20; ++i)
    {
      if (i == 15)
      {
        break;
      }
      b[i] = i;
    } 
  }
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Array sorting as a function, where array state \
     change in another context testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    int[] arr = new int[] {15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
    BubbleSorter bubbleSorter = new BubbleSorter();
    bubbleSorter.Sort(arr, 16);
  }
}

public class BubbleSorter
{
  public void Sort(int[] arr, int n)
  {
    for (int i = 0; i < n - 1; i++)
    {
      for (int j = 0; j < n - i - 1; j++)
      {
        if (arr[j] > arr[j + 1])
        {
          int temp = arr[j];
          arr[j] = arr[j + 1];
          arr[j + 1] = temp;
        }
      }
    }
  }
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Object state changing in another context testing \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Person person = new Person(25, "Bob");
    Child child = new Child(person);
    child.SetParentAge(30);
  }
}

public class Person
{
    int age;
    string name;

    public Person()
    {

    }

    public Person(int age, string name)
    {
      this.age = age;
      this.name = name;
    }
}

public class Child : Person
{
  public Person parent;

  public Child(int age, string name) : base(age, name)
  {
    parent = new Person(40, "Spike");
  }

  public Child(Person parent)
  {
    this.parent = parent;
  }

  public void SetParentAge(int age)
  {
    Person p1 = parent; 
    p1.age = age;
  }
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Pattern Visitor testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Figure[] list = new Figure[] { new Circle(5), new Rectangle(2,4), new Triangle() };

    AreaVisitor areaVisitor = new AreaVisitor();
    PerimeterVisitor perimeterVisitor = new PerimeterVisitor();

    int[] resPerimeter = new int[3];
    int[] resArea = new int[3];

    for(int i = 0; i < 3; i++)
    {
      resPerimeter[i] = list[i].Accept(areaVisitor);
    }

    for(int j = 0; j < 3; j++) 
    {
      resArea[j] = list[j].Accept(perimeterVisitor);
    }
  }
}

public abstract class Figure
{
  public abstract int Accept(Visitor v);
}

public abstract class Visitor
{
  public abstract int Visit(Circle circle);
  public abstract int Visit(Rectangle rectangle);
  public abstract int Visit(Triangle triangle);
}

public class AreaVisitor : Visitor
{
  public override int Visit(Circle circle)
  {
    return 3 * circle.radius * circle.radius;
  }

  public override int Visit(Rectangle rectangle)
  {
    return rectangle.a * rectangle.b;
  }

  public override int Visit(Triangle triangle)
  {
    int p = (triangle.a + triangle.b + triangle.c) / 2;
    return p * (p - triangle.a) * (p - triangle.b) * (p - triangle.c);
  }
}

public class PerimeterVisitor : Visitor
{
  public override int Visit(Circle circle)
  {
    return 2 * 3 * circle.radius;
  }

  public override int Visit(Rectangle rectangle)
  {
    return (rectangle.a + rectangle.b) * 2;
  }

  public override int Visit(Triangle triangle)
  {
    return triangle.a + triangle.b + triangle.c;
  }
}

public class Circle : Figure
{
  public int radius;

  public Circle(int radius)
  {
    this.radius = radius;
  }

  public Circle()
  {
    this.radius = 1;
  }

  public override int Accept(Visitor v)
  {
    return v.Visit(this);
  }
}

public class Triangle : Figure
{
  public int a, b, c;

  public Triangle(int a, int b, int c)
  {
    this.a = a;
    this.b = b;
    this.c = c;
  }

  public Triangle()
  {
    this.a = 1;
    this.b = 1;
    this.c = 1;
  }

  public override int Accept(Visitor v)
  {
    return v.Visit(this);
  }
}

public class Rectangle : Figure
{
  public int a, b;

  public Rectangle()
  {
    this.a = 1;
    this.b = 1;
  }

  public Rectangle(int a, int b)
  {
    this.a = a;
    this.b = b;
  }

  public override int Accept(Visitor v)
  {
    return v.Visit(this);
  }
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Factorial recursion testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Factorial factorial = new Factorial();
    int f = factorial.GetFact(5);
  }
}

public class Factorial
{
  public int GetFact (int n)
  {
    if (n <= 1) return 1;
    else return n * GetFact(n - 1);
  }
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Quick sort recursion testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    int[] arr = new int[] {15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
    QuickSorter quickSorter = new QuickSorter();
    int n = 16;
    int low = 0;
    int high = 15;
    quickSorter.QuickSort(arr, n, low, high);
  }
}

public class QuickSorter
{
  public void QuickSort(int[] array, int n, int low, int high)
  {
    if (n == 0) 
        return;
    
    if (low >= high) 
        return;
    
    int middle = low + (high - low) / 2;
    int pivot = array[middle];
    int i = low, j = high;

    while (i <= j)
    {
      while (array[i] < pivot)
      {
        i++;
      }
      while (array[j] > pivot)
      {
        j--;
      }
      if (i <= j)
      {
        int temp = array[i];
        array[i] = array[j];
        array[j] = temp;
        i++;
        j--;
      }
    }

    if (low < j) 
      QuickSort(array, n, low, j);

    if (high > i) 
      QuickSort(array, n, i, high);
  }
}
|})

let () = interpret_test program (Hashtbl.create 128)

(*Kakadu:
Обязательно тест про массивы: я хочу, чтобы все почувствовали какой отстойный это язык, потому что там есть
вот это: https://docs.microsoft.com/en-us/dotnet/api/system.arraytypemismatchexception?view=net-5.0*)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- ArrayTypeMismatchException testing \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Pet[] pets = new Pet[10];
    pets[0] = new Dog(2, "Spike");
  }
}

public class Pet
{
  public int age;
  public string name;

  public Pet(int age, string name)
  {
    this.age = age;
    this.name = name;
  }
}

public class Cat : Pet
{
  public Cat(int age, string name) : base(age, name)
  {

  }
}

public class Dog : Pet
{
  public Dog(int age, string name) : base(age, name)
  {

  }
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Constructor chaining testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Cat cat = new Cat(2, "Mars", 30);
  }
}

public class Pet
{
  public int age;
  public string name;

  public Pet(int age, string name)
  {
    this.age = age;
    this.name = name;
  }
}

public class Cat : Pet
{
  public int hairLevel;

  public Cat(int age, string name) : base(age, name)
  {

  }

  public Cat(int age, string name, int hairLevel) : this(age, name)
  {
    this.hairLevel = hairLevel;
  }
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Constructor chaining recursion testing \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Cat cat = new Cat(2, "Mars", 30);
  }
}

public class Pet
{
  public int age;
  public string name;

  public Pet(int age, string name)
  {
    this.age = age;
    this.name = name;
  }
}

public class Cat : Pet
{
    public int hairLevel;

    public Cat(int age, string name) : base(age, name)
    {

    }

    public Cat(int age, string name, int hairLevel) : this(age, name, hairLevel)
    {
      
    }
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Const fields testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Math m = new Math();
    m.PI = 4;
  }
}

public class Math
{
  public const int PI = 3;
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Const variables testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program 
{
  public static void Main()
  {
    const int a = 25;
    a++;
  }
}
|})

let () = interpret_test program (Hashtbl.create 128)

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Ad-hoc polymorphism, specifically methods \
     overloading, testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Summator summator = new Summator();
    int a = summator.Sum(5, 3);
    string s = summator.Sum("GG", "WP"); 
  }
}

public class Summator
{
  public int Sum(int a, int b)
  {
    return a + b;
  }

  public string Sum(string a, string b)
  {
    return a + b;
  }
}
|})

let () = interpret_test program (Hashtbl.create 128)
