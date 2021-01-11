open Csharpmini_lib.Parser
open Csharpmini_lib.Pretty_printer

let parse_result =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Figure[] figures = new Figure[] { new Circle(5), new Rectangle(2, 4), new Triangle() };

    VisitorArea visitorArea = new VisitorArea();
    VisitorPerimeter visitorPerimeter = new VisitorPerimeter();

    for (int i = 0; i < figures.length; i++)
    {
      Console.WriteLine(figures[i].Accept(visitorArea));
    }

    for(int j = 0; j < figures.length; j++)
    {
      Console.WriteLine(figures[j].Accept(visitorPerimeter));
    }
  }
}

public abstract class Figure
{
  abstract int Accept(Visitor visitor);
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

  public override int Accept(Visitor visitor)
  {
    return visitor.visit(this);
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

  public override int Accept(Visitor visitor)
  {
    return visitor.visit(this);
  }
}

public class Rectangle : Figure
{
  public int a, b;

  public Rectangle(int a, int b)
  {
    this.a = a;
    this.b = b;
  }

  public Rectangle()
  {
    this.a = 1;
    this.b = 1;
  }

  public override int Accept(Visitor visitor)
  {
    return visitor.visit(this);
  }
}

public abstract class Visitor
{
  abstract int Visit(Circle circle);
  abstract int Visit(Rectangle rectangle);
  abstract int Visit(Triangle triangle);
}

public class VisitorArea : Visitor
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
    int perimeter = (triangle.a + triangle.b + triangle.c) / 2;
    return perimeter * (perimeter - triangle.a) * (perimeter - triangle.b) * (perimeter - triangle.c);
  }
}

public class VisitorPerimeter : Visitor
{
  public override int Visit(Circle circle)
  {
    return 2 * 3 * circle.radius;
  }

  public override int Visit(Rectangle rectangle)
  {
    return 2 * (rectangle.a + rectangle.b);
  }

  public override int Visit(Triangle triangle)
  {
    return triangle.a + triangle.b + triangle.c;
  }
}
|})

let () = pretty_print parse_result

let parse_result =
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

let () = pretty_print parse_result
