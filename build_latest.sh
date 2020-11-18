#/bin/sh -x

if [ $# -eq 0 ]
  then
    echo "No arguments supplied"
    exit 1
fi

STRAT=
if [ "$1" = "build" ]; then
	STRAT="$1"
elif [ "$1" = "test" ]; then
	STRAT="$1"
else
	echo "bad argument $1"
	exit 1
fi

rm -fr _opam
#export OPAMSWITCH=4.10.0
eval $(opam env)
opam switch
which ocamlc
which ocaml

LASTDIR=`git diff --name-only HEAD HEAD~1 | tail -n 1 | cut -f 1 -d'/'`

if [ "$LASTDIR" = "." ]; then
	git diff --name-only HEAD HEAD~1
	echo "DO NOTHING"
else
	cd $LASTDIR
  echo "Going to run tests in $PWD..."
  PKG_OPAM="$LASTDIR.opam"
  if [ ! -f "$PKG_OPAM" ]; then
      echo "$PKG_OPAM does not exist. Exit"
      exit 1
  fi
  if [ "$(opam lint -s $PKG_OPAM)" != "" ]; then
    echo "File $PKG_OPAM is not updated well. Command 'opam lint -s $PKG_OPAM' should print nothing"
    opam lint $PKG_OPAM
    #exit 1
  else
    echo "Checking $PKG_OPAM passed."
  fi
  eval $(opam env)
  if [ "$STRAT" = "build" ]; then
    #[ -d _opam ] || cp -r ~/.opam/4.10 _opam
    #sudo apt install m4 -y
    #opam update
    opam install --deps-only -t -y .

    dune build @fmt
    if [ $? = 0 ]; then 
      echo "Formatting OK"
    else 
      echo "Formatting is not decent. Either intergrate ocamlformat to VsCode"
      echo "  or apply it manualy before every commit https://dune.readthedocs.io/en/stable/formatting.html"      
      exit 1
    fi
    dune build
    if [ $? = 0 ]; then
      echo "Running $STRAT in $LASTDIR finished\n"
    else
      exit 1
    fi
  else
    opam install --deps-only -t -y .
    dune runtest
    if [ $? = 0 ]; then
      echo "Running $STRAT in $LASTDIR finished\n"
    else
      exit 1
    fi
  fi
fi

