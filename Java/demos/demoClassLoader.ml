open Java_lib.Parser
open Java_lib.Interpreter

open Java_lib.Interpreter.ClassLoader (Java_lib.Interpreter.Result)

open Java_lib.Hashtbl_p

let show_hashtbl ht pp_k pp_el = pp pp_k pp_el Format.std_formatter ht

let show_class_table ht = show_hashtbl ht pp_key_t pp_class_r

let test_load t_val cl_tbl =
  match load t_val cl_tbl with
  | Error m ->
      print_endline m;
      Hashtbl.clear cl_tbl
  | Ok cl_t ->
      show_class_table cl_t;
      Hashtbl.clear cl_t

let () =
  print_string "-------------------TESTING_INHERITANCE-------------------\n\n"

let test_value =
  Option.get
    (apply parser
       {| 
public class Main
{
	public static void main() {
	    Person p = new Person(80, 45);
	}
}

class Person {
    public int weight;
    public int age;
    
    public Person(int w, int a) {
        this.weight = w;
        this.age = a;
    }
    
    
    public int getWeight() {
        return weight;
    }
    
    public int getAge() {
        return age;
    }
    
    public void setWeight(int w) {
        this.weight = w;
    }
    public void setAge(int a) {
        this.age = a;
    }
    
}

class Child extends Person{
    public int cash;
    
    public Child(int w, int a) {
        super(w,a);
        cash = 0;
    }
    @Override
    public int getAge() {
        return age + 1;
    }
    
    public int getCash() {
        return cash;
    }
    
    public void setCash(int c) {
        this.cash = c;
    }
    
    public Child (int w, int a, int c) {
        super(w, a);
        cash = c;
    }
    
    public void giveEvenNumbers100() {
        for (int i = 0; i < 100; i++) {
            if (i % 2 == 0 && !(i % 2 == 1)) {
                System.out.println(i);
            }
            else {
                continue;
            }
        }
    }
    
}
|})

let () = test_load test_value (Hashtbl.create 100)

let () = print_string "-------------------SIMILAR_FIELDS-------------------\n\n"

let test_value =
  Option.get
    (apply parser
       {| 
public class Main
{
	public static void main() {
		Person p = new Person(80, 45);
	}
}

class Person {
    public int weight;
    public int weight;
    
    public Person(int w, int a) {
        this.weight = w;
        this.age = a;
    }
    
    
    
    public int getWeight() {
        return weight;
    }
    
    public int getAge() {
        return age;
    }
    
    public void setWeight(int w) {
        this.weight = w;
    }
    public void setAge(int a) {
        this.age = a;
    }
    
}

class Child extends Person{
    public int cash;
    
    public Child(int w, int a) {
        super(w,a);
        cash = 0;
    }
    
    public int getCash() {
        return cash;
    }
    
    public void setCash(int c) {
        this.cash = c;
    }
    
    public Child (int w, int a, int c) {
        super(w, a);
        cash = c;
    }
    
    public void giveEvenNumbers100() {
        for (int i = 0; i < 100; i++) {
            if (i % 2 == 0 && !(i % 2 == 1)) {
                System.out.println(i);
            }
            else {
                continue;
            }
        }
    }
    
}
|})

let () = test_load test_value (Hashtbl.create 100)

let () =
  print_string "-------------------SIMILAR_METHODS_ERROR-------------------\n\n"

let test_value =
  Option.get
    (apply parser
       {| 
public class Main
{
	public static void main() {
		Person p = new Person(80, 45);
	}
}

class Person {
    public int age;
    public int weight;
    
    public Person(int w, int a) {
        this.weight = w;
        this.age = a;
    }
    
    public void setWeight(int w) {
        this.weight = w;
    }
    
    public void setWeight(int a) {
  
    }
}

class Child extends Person{
    public int cash;
    
    public Child(int w, int a) {
        super(w,a);
        cash = 0;
    }
    
    public int getCash() {
        return cash;
    }
    
    public void setCash(int c) {
        this.cash = c;
    }
    
    public Child (int w, int a, int c) {
        super(w, a);
        cash = c;
    }
    
}
|})

let () = test_load test_value (Hashtbl.create 100)

let () =
  print_string
    "-------------------SIMILAR_CONSTRUCTOR_ERROR-------------------\n\n"

let test_value =
  Option.get
    (apply parser
       {| 
public class Main
{
	public static void main() {
		Person p = new Person(80, 45);
	}
}

class Person {
    public int age;
    public int weight;
    
    public Person(int w, int a) {
        this.weight = w;
        this.age = a;
    }

    public Person(int x, int y) {
        int z = z + y; 
    }
    
    
}

class Child extends Person{
    public int cash;
    
    public Child(int w, int a) {
        super(w,a);
        cash = 0;
    }
    
    public int getCash() {
        return cash;
    }
    
    public void setCash(int c) {
        this.cash = c;
    }
    
    public Child (int w, int a, int c) {
        super(w, a);
        cash = c;
    }
    
}
|})

let () = test_load test_value (Hashtbl.create 100)

let () =
  print_string "-------------------ABSTRACTNESS_ERRORS-------------------\n\n"

let test_value =
  Option.get
    (apply parser
       {| 
public class Main
{
	public static void main() {
	    
	}
}

class Figure {
    abstract int accept(Visitor v);
}

class Circle extends Figure {
    public int radius;

    public Circle(int radius) {
        this.radius = radius;
    }

    public Circle() {
        this.radius = 1;
    }

    @Override
    int accept(Visitor v) {
        return v.visit(this);
    }

|})

let () = test_load test_value (Hashtbl.create 100)

let test_value =
  Option.get
    (apply parser
       {| 
public class Main
{
	public static void main() {
	}
}

abstract class Figure {
    int accept(Visitor v);
}

class Circle extends Figure {
    public int radius;

    public Circle(int radius) {
        this.radius = radius;
    }

    public Circle() {
        this.radius = 1;
    }

    @Override
    int accept(Visitor v) {
        return v.visit(this);
    }
}
|})

let () = test_load test_value (Hashtbl.create 100)

let test_value =
  Option.get
    (apply parser
       {| 
public class Main
{
	public static void main() {
	}
}

abstract class Figure {
    abstract int accept(Visitor v) {
        int x = 1 + 2;
    }
}

class Circle extends Figure {
    public int radius;

    public Circle(int radius) {
        this.radius = radius;
    }

    public Circle() {
        this.radius = 1;
    }

    @Override
    int accept(Visitor v) {
        return v.visit(this);
    }
}
|})

let () = test_load test_value (Hashtbl.create 100)

let test_value =
  Option.get
    (apply parser
       {| 
public class Main
{
	public static void main() {
	}
}

abstract class Figure {
    abstract int accept(Visitor v);
}

class Circle extends Figure {
    public int radius;

    public Circle(int radius) {
        this.radius = radius;
    }

    public Circle() {
        this.radius = 1;
    }

}
|})

let () = test_load test_value (Hashtbl.create 100)

let () =
  print_string
    "-------------------FINAL_MODIFIERS_ERRORS-------------------\n\n"

let test_value =
  Option.get
    (apply parser
       {| 
public class Main
{
	public static void main() {
		Person p = new Person(80, 45); 
	}
}

final class Person {
    public int age;
    public int weight;
    
    public Person(int w, int a) {
        super(w, a);
        this.weight = w;
        this.age = a;
    }
    
}

class Child extends Person{
    public int cash;
    
    public Child(int w, int a) {
        cash = 0;
    }
    
    public int getCash() {
        return cash;
    }
    
    public void setCash(int c) {
        this.cash = c;
    }
    
    public Child (int w, int a, int c) {
        super(w, a);
        cash = c;
    }
    
}
|})

let () = test_load test_value (Hashtbl.create 100)

let test_value =
  Option.get
    (apply parser
       {| 
public class Main
{
	public static void main() {
		Person p = new Person(80, 45);
	}
}

class Person {
    public int age;
    public int weight;
    
    public Person(int w, int a) {
        this.weight = w;
        this.age = a;
    }
    
    public int getWeight() {
        return weight;
    }
    
    final public int getAge() {
        return age;
    }
    
}

class Child extends Person{
    public int cash;
    
    public Child(int w, int a) {
        super(w, a);
        cash = 0;
    }
    
    public int getCash() {
        return cash;
    }
    
    public void setCash(int c) {
        this.cash = c;
    }
    
    public Child (int w, int a, int c) {
        super(w, a);
        cash = c;
    }
    
}
|})

let () = test_load test_value (Hashtbl.create 100)

let () =
  print_string "-------------------@OVERRIDE_ERRORS-------------------\n\n"

let test_value =
  Option.get
    (apply parser
       {| 
public class Main
{
	public static void main() {
		Person p = new Person(80, 45);
	}
}

class Person {
    public int age;
    public int weight;
    
    public Person(int w, int a) {
        this.weight = w;
        this.age = a;
    }
    
    public int getWeight() {
        return weight;
    }
    
    public int getAge() {
        return age;
    }
    
}

class Child extends Person{
    public int cash;
    
    public Child(int w, int a) {
        super(w, a);
        cash = 0;
    }
    
    public int getCash() {
        return cash;
    }

    @Override
    public void setCash(int c) {
        this.cash = c;
    }
    
    public Child (int w, int a, int c) {
        super(w, a);
        cash = c;
    }
    
}
|})

let () = test_load test_value (Hashtbl.create 100)
