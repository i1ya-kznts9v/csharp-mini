  $ (cd ../../../../default && demos/renameTransformationTest.exe)
  -_-_-_-_-_-_-_-_-_-_- First variables rename transformating testing -_-_-_-_-_-_-_-_-_-_-
  
  -- int i = 0;
  ++ int k = 0;
  
  -- quickSorter.QuickSort(arr, n, i, j);
  ++ quickSorter.QuickSort(arr, n, k, j);
  
  -- int i = low, j = high;
  ++ int k = low, j = high;
  
  -- while (i <= j)
  ++ while (k <= j)
  
  -- while (array[i] < pivot)
  ++ while (array[k] < pivot)
  
  -- i++;
  ++ k++;
  
  -- if (i <= j)
  ++ if (k <= j)
  
  -- int temp = array[i];
  ++ int temp = array[k];
  
  -- array[i] = array[j];
  ++ array[k] = array[j];
  
  -- i++;
  ++ k++;
  
  -- if (high > i)
  ++ if (high > k)
  
  -- QuickSort(array, n, i, high);
  ++ QuickSort(array, n, k, high);
  
  -- for (int i = 0;; i < 10; i++)
  ++ for (int k = 0;; k < 10; k++)
  
  -- if (i % 2 == 0)
  ++ if (k % 2 == 0)
  
  -- arr[i] = 1;
  ++ arr[k] = 1;
  
  -_-_-_-_-_-_-_-_-_-_- Second variables rename transformating testing -_-_-_-_-_-_-_-_-_-_-
  
  --  string name;
  ++  string ident;
  
  -- public Person(int age, string name)
  ++ public Person(int age, string ident)
  
  -- this.name = name;
  ++ this.ident = ident;
  
  -- public Child(int age, string name) : base(age, name)
  ++ public Child(int age, string ident) : base(age, ident)
  
  -_-_-_-_-_-_-_-_-_-_- Third variables rename transformating testing -_-_-_-_-_-_-_-_-_-_-
  
  -- public int GetFact(int n)
  ++ public int GetFact(int number)
  
  -- if (n <= 1)
  ++ if (number <= 1)
  
  -- return n * GetFact(n - 1);
  ++ return number * GetFact(number - 1);
  
