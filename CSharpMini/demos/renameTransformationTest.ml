open Csharpmini_lib.Transformator
open Format

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- First rename transformating testing \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  {|
public class Program
{
  public static void Main()
  {
    int[] arr = new int[] {15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
    QuickSorter quickSorter = new QuickSorter();
    int n = 16;
    int i = 0;
    int j = 15;
    quickSorter.QuickSort(arr, n, i, j);
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

public class Something
{
  public void Do()
  {
    int[] arr = new int[10];

    for (int i = 0; i < 10; i++)
    {
      if (i % 2 == 0)
      {
        continue;
      }

      arr[i] = 1;
    }
  }
}
|}

let () = rename_transformation program "i" "k" std_formatter

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Second rename transformating testing \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
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
|}

let () = rename_transformation program "name" "ident" std_formatter

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Third rename transformating testing \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
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
|}

let () = rename_transformation program "n" "number" std_formatter
