open Java_lib.Parser
open Java_lib.Pretty_printer

let test_value =
  Option.get
    (apply parser
       {| 
public class Main {

    public static void main() {
        Figure[] list = new Figure[] {new Circle(5), new Rectangle(2,4), new Triangle()};
        AreaVisitor areaVisitor = new AreaVisitor();
        PerimeterVisitor perimeterVisitor = new PerimeterVisitor();

        for (int i = 0; i < list.length; i++) {
            System.out.println(list[i].accept(areaVisitor));
        }
        for(int j = 0; j < list.length; j++) {
            System.out.println(list[j].accept(perimeterVisitor));
        }
    }
}

abstract class Figure {
    abstract int accept(Visitor v);
}

abstract class Visitor {
    abstract int visit(Circle circle);
    abstract int visit(Rectangle rectangle);
    abstract int visit(Triangle triangle);
}

class AreaVisitor extends Visitor {

    @Override
    int visit(Circle circle) {
        return 3 * circle.radius * circle.radius;
    }

    @Override
    int visit(Rectangle rectangle) {
        return rectangle.a * rectangle.b;
    }

    @Override
    int visit(Triangle triangle) {
        int p = (triangle.a + triangle.b + triangle.c) / 2;
        return p * (p - triangle.a) * (p - triangle.b) * (p - triangle.c);
    }
}

class PerimeterVisitor extends Visitor {

    @Override
    int visit(Circle circle) {
        return 2 * 3 * circle.radius;
    }

    @Override
    int visit(Rectangle rectangle) {
        return (rectangle.a + rectangle.b) * 2;
    }

    @Override
    int visit(Triangle triangle) {
        return triangle.a + triangle.b + triangle.c;
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

class Triangle extends Figure {
    public int a, b, c;

    public Triangle(int a, int b, int c) {
        this.a = a;
        this.b = b;
        this.c = c;
    }
    public Triangle() {
        this.a = 1;
        this.b = 1;
        this.c = 1;
    }

    @Override
    int accept(Visitor v) {
        return v.visit(this);
    }
}

class Rectangle extends Figure {
    public int a, b;

    public Rectangle() {
        this.a = 1;
        this.b = 1;
    }

    public Rectangle(int a, int b) {
        this.a = a;
        this.b = b;
    }

    @Override
    int accept(Visitor v) {
        return v.visit(this);
    }
}
|})

let () = print_class_dec_list test_value

let test_value =
  Option.get
    (apply parser
       {|
        
public class Main {
    public static void main() {
        int[] arr = new int[] {15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
        QuickSorter quickSorter = new QuickSorter();
        int n = 16;
        int low = 0;
        int high = 15;
        quickSorter.quickSort(arr, n, low, high);
    }
}

class QuickSorter {
    public void quickSort(int[] array, int n, int low, int high) {
        if (n == 0) 
            return;
        
        if (low >= high) 
            return;
        
        int middle = low + (high - low) / 2;
        int pivot = array[middle];
        int i = low, j = high;
        while (i <= j) {
            while (array[i] < pivot) {
                i++;
            }
            while (array[j] > pivot) {
                j--;
            }
            if (i <= j) {
                int temp = array[i];
                array[i] = array[j];
                array[j] = temp;
                i++;
                j--;
            }
        }
        if (low < j) 
            quickSort(array, n, low, j);

        if (high > i) 
            quickSort(array, n, i, high);
        
    }
}

        |})

let () = print_class_dec_list test_value
