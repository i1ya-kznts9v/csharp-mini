open Parser

let stdlib_storage =
  {|
  public class BubbleSorter 
  {
    public void Sort(int[] array, int n) 
    {
      int i = 0;

      while (i < n - 1) 
      {
        int j = 0;

        while (j < n - i - 1) 
        {
          if (array[j] > array[j + 1]) 
          {
            int temp = array[j];
            array[j] = array[j + 1];
            array[j + 1] = temp;
          }

          j++;
        }

        i++;
      }
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

  public abstract class Figure 
  {
    public const int PI = 3;

    public abstract int Area();
    public abstract int Perimeter(); 
  }

  public class Circle : Figure
  {
    public int radius;
    
    public Circle(int radius) 
    {
      this.radius = radius;
    }
    
    public override int Area() 
    {
      return PI * radius * radius;
    }

    public override int Perimeter() 
    {
      return 2 * PI * radius;
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
    
    public override int Area()
    {
      return a * b;
    }

    public override int Perimeter() 
    {
      return 2 * (a + b);
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
    
    public override int Area()
    {
      int p = (a + b + c) / 2;
      return p * (p - a) * (p - b) * (p - c);
    } 

    public override int Perimeter()
    {
      return a + b + c;
    }
  }

  public class Factorial 
  {
    public int GetFactorial(int n) 
    {
      if (n <= 1) 
      {
        return 1;
      }
      else 
      {
        return n * GetFactorial(n - 1);
      }
    }
  }

  public class Fibonacci 
  {
    public int GetFibonacci(int n) 
    {
      if (n <= 1) 
      {
        return 0;
      }
      else if (n == 2) 
      {
        return 1;
      } 
      else
      {
        return GetFibonacci(n - 1) + GetFibonacci(n - 2);
      } 
    } 
  }

  public class Repl 
  {

  }
|}

let stdlib_classes = apply parser stdlib_storage
