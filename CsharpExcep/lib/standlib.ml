open Parser

let standlib_classes =
  {|
  
  class Factorial 
  {
    public int CalFac(int n) 
    {
        if (n <= 1) return 1;
        else return n * CalFac(n - 1);
    }
  }

  class Repl
  {

  }

  class DivideByZeroException : Exception
  {
    string zero = "IT's ZERO!";
    public bool Filter()
    {
      return true;
    }
  }

  class UnhandleException
  {
    public void Unhandle()
    {
      throw new Exception();
    }
  }

  class HandleException
  {
    public void Handle()
    {
      try
        {
          throw new DivideByZeroException();
        }
        catch(DivideByZeroException e) when (e.Filter())
        {
          Console.WriteLine("Exception handled");
          Console.WriteLine(e.zero);
        }
    }
  }


  class Fibonacci 
  {
    public int CalFib(int n) 
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
					return CalFib(n - 1) + CalFib(n - 2);
				} 
		} 
  }
|}

let parse_std_cl = apply_parser parser standlib_classes
