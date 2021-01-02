open Opal
open Csharpmini_lib.Ast
open Csharpmini_lib.Parser
open Csharpmini_lib.Interpreter
open Csharpmini_lib.Interpreter.Result
open Csharpmini_lib.Interpreter.ClassLoader (Csharpmini_lib.Interpreter.Result)

let show_string_option = function Some str -> str | None -> "None"
let show_key_list list = List.fold_left (fun str el -> el ^ " " ^ str) "" list

let show_hashtbl ht show =
  match Hashtbl.length ht with
  | 0 -> "[[]]"
  | _ ->
      Hashtbl.fold
        (fun key value el -> el ^ key ^ " -> " ^ show value ^ "\n")
        ht "[["
      ^ "]]"

let show_fields_table ht = show_hashtbl ht show_field_t
let show_methods_table ht = show_hashtbl ht show_method_t
let show_constructors_table ht = show_hashtbl ht show_constructor_t

let show_class_t class_t =
  let this_key = "{ this_key : " ^ class_t.this_key ^ "; " in
  let fields_table =
    "fields_table : " ^ show_fields_table class_t.fields_table ^ "; " in
  let methods_table =
    "methods_table : " ^ show_methods_table class_t.methods_table ^ "; " in
  let constructors_table =
    "constructors_table : "
    ^ show_constructors_table class_t.constructors_table
    ^ "; " in
  let children_keys =
    "children_keys : " ^ show_key_list class_t.children_keys ^ "; " in
  let is_abstract =
    "is_abstract : " ^ Bool.to_string class_t.is_abstract ^ "; " in
  let is_sealed = "is_sealed : " ^ Bool.to_string class_t.is_sealed ^ "; " in
  let parent_key =
    "parent_key : " ^ show_string_option class_t.parent_key ^ "}" in
  String.concat ""
    [ this_key; fields_table; methods_table; constructors_table; children_keys
    ; is_abstract; is_sealed; parent_key ]

let show_class_table ht = show_hashtbl ht show_class_t

let load_test program =
  match load program with
  | Error error ->
      print_string (error ^ "\n") ;
      Hashtbl.clear class_table
  | Ok _ ->
      print_string (show_class_table class_table ^ "\n") ;
      Hashtbl.clear class_table

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
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int weight;
  public int age;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}

public class Child : Person
{
  public int cash;
  
  public Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Wrong modifiers testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int weight;
  public int age;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public const void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}

public class Child : Person
{
  public int cash;
  
  public Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int weight;
  public int age;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}

static class Child : Person
{
  public int cash;
  
  public Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int weight;
  public int age;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}

class Child : Person
{
  public int cash;
  
  public abstract Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Similar fields error testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int age;
  public int age;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}

public class Child : Person
{
  public int cash;
  
  public Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Similar methods error esting -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int weight;
  public int age;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetWeight(int age)
  {
    this.age = age;
  }
}

public class Child : Person
{
  public int cash;
  
  public Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Similar constructors error testing \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int weight;
  public int height;
  public int age;
  public int income;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }

  public Person(int height, int income)
  {
    this.height = height;
    this.income = income;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetWeight(int age)
  {
    this.age = age;
  }
}

public class Child : Person
{
  public int cash;
  
  public Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Abstract errors testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Builder builder = new Builder();
    Circle circle = new Circle();

    circle.build(builder);
	}
}

public class Figure 
{
  public abstract void build(Builder builder);
}

public class Circle : Figure
{
  public int radius;

  public Circle() : this(1)
  {

  }

  public Circle(int radius)
  {
    this.radius = radius;
  }

  public override void build(Builder builder)
  {
    return builder.build(radius);
  }
}
|})

let () = load_test program

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Builder builder = new Builder();
    Circle circle = new Circle();

    circle.build(builder);
	}
}

public class Figure 
{
  public virtual void build(Builder builder);
}

public class Circle : Figure
{
  public int radius;

  public Circle() : this(1)
  {

  }

  public Circle(int radius)
  {
    this.radius = radius;
  }

  public override void build(Builder builder)
  {
    return builder.build(radius);
  }
}
|})

let () = load_test program

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Builder builder = new Builder();
    Circle circle = new Circle();

    circle.build(builder);
	}
}

public abstract class Figure 
{
  public int radius = 1;

  public abstract void build(Builder builder)
  {
    return builder.build(radius);
  }
}

public class Circle : Figure
{
  public int radius;

  public Circle() : this(1)
  {

  }

  public Circle(int radius)
  {
    this.radius = radius;
  }

  public override void build(Builder builder)
  {
    return builder.build(radius);
  }
}
|})

let () = load_test program

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Builder builder = new Builder();
    Circle circle = new Circle();

    circle.build(builder);
	}
}

public abstract class Figure 
{
  public abstract void build(Builder builder);
}

public class Circle : Figure
{
  public int radius;

  public Circle() : this(1)
  {

  }

  public Circle(int radius)
  {
    this.radius = radius;
  }
}
|})

let () = load_test program

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Sealed class errors testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Builder builder = new Builder();
    Circle circle = new Circle();

    circle.build(builder);
	}
}

public sealed class Figure
{
  public int radius;
  
  public virtual void build(Builder builder)
  {
    return builder.build(radius);
  }
}

public class Circle : Figure
{
  public int radius;

  public Circle() : this(1)
  {

  }

  public Circle(int radius)
  {
    this.radius = radius;
  }

  public override void build(Builder builder)
  {
    return builder.build(radius);
  }
}
|})

let () = load_test program

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Builder builder = new Builder();
    Circle circle = new Circle();

    circle.build(builder);
	}
}

public sealed class Figure
{
  public int radius;
  
  public void build(Builder builder)
  {
    return builder.build(radius);
  }
}

public class Circle : Figure
{
  public int radius;

  public Circle() : this(1)
  {

  }

  public Circle(int radius)
  {
    this.radius = radius;
  }
}
|})

let () = load_test program

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Override errors testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int weight;
  public int age;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}

public class Child : Person
{
  public int cash;
  
  public Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public override int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int weight;
  public int age;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}

public class Child : Person
{
  public int age;
  public int cash;
  
  public Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }

  public override void SetAge(int age)
  {
    this.age = age + 1;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program
