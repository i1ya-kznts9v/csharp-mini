  $ (cd ../../../../default && demos/demoFirst.exe)
  
  //////BCmd+BCommand TEST////////////
  funcFactor ()
  {
  facto=1 ;
  count=$1 ;
  while (( $count > 0 ))
  do
  facto=$(($facto*$count)) ; count=$(($count-1))
  done ; echo $facto
  }
  
  funcFactor 4
   <-> 
  BConv(DeclFunct(funcFactor>Pipeline(Dpoint, Expression(Eqal(SimpleVari(facto),Word(WString(INT:1)), )), Pipeline(Dpoint, Expression(Eqal(SimpleVari(count),Word(WString(Vari( SimpleVari(1)))), )), Pipeline(Dpoint, While(Great(Container(Vari( SimpleVari(count))) , Container(INT:0))->act Pipeline(Dpoint, Expression(Eqal(SimpleVari(facto),Subarifm(Multi(Container(Vari( SimpleVari(facto))) , Container(Vari( SimpleVari(count))))), )), SigPipe(Expression(Eqal(SimpleVari(count),Subarifm(Minus(Container(Vari( SimpleVari(count))) , Container(INT:1))), ))))), SigPipe(Expression(CallFunction(echo, Listarg(Word(WString(Vari( SimpleVari(facto)))), Word(WString(STR:)), )Redirect( NONE ))))))))<>BsigCmd(SigPipe(Expression(CallFunction(funcFactor, Listarg(Word(WString(INT:4)), )Redirect( NONE ))))))
  //////WORD TEST////////////
  ssss{${sds[1]},sss,}dsdsd = WStringCont(STR:ssss| WbrExpCont(WString(Vari( Barces(ArryVari(sds, Container(INT:1))))), WString(STR:sss), WString(STR:), |WString(STR:dsdsd)))
  //////PIPECONV TEST////////////
  foreach i ( 1 2 3 4 ) local s=$i ; echo $i end
   <-> 
  SigPipe(Foreach(SimpleVari(i), List(Word(WString(INT:1)), Word(WString(INT:2)), Word(WString(INT:3)), Word(WString(INT:4)), Word(WString(STR:)), ), Pipeline(Dpoint, Expression(Eqal(Local(SimpleVari(s)),Word(WString(Vari( SimpleVari(i)))), )), SigPipe(Expression(CallFunction(echo, Listarg(Word(WString(Vari( SimpleVari(i)))), )Redirect( NONE )))))))
