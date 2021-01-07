open Csharpmini_lib.Ast
open Csharpmini_lib.Parser

let print_list =
  Format.pp_print_list Format.pp_print_string Format.std_formatter

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

let test = print_list (List.map show_classes parse_result)
