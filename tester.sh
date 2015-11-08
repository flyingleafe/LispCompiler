#!/bin/sh

testnum="*"
if [ "$1" != "" ] ; then
    testnum=$1
fi
if  cabal build compiler ; then

    lispcompiler="./dist/build/compiler/compiler"
    using_cabal=1
    echo "using cabal"
else
    using_cabal=0
fi
if [ $using_cabal -eq 0 ] && nix-build ; then
    lispcompiler="./result/bin/compiler"
    using_nix=1
    echo "using nix"
else
    using_nix=0
fi
if [ $using_nix -eq 0 ] && [ $using_cabal -eq 0 ] ; then
    echo "exiting, build tool unspecified"
    exit
fi

mkdir tmp
sources=$(find test/lisp -iname "test$testnum.lisp" | sort -h)

for inputsource in $sources
do
    echo "processing "$inputsource

    prefix=${inputsource:0:$((${#inputsource}-5))}
    rightoutput=$prefix".out"
    rightinput=$prefix".in"
    externtester=$prefix".cpp"


    if [ -f "$externtester" ] ; then
        echo "Using extern tester: "$externtester

        # Compiling lisp → yasm → elf
        $lispcompiler -MS -o tmp/test.yasm $inputsource
        c0=$?
        yasm -felf64 -gdwarf2 -o tmp/test.o tmp/test.yasm
        c1=$?
        c++ -ggdb tmp/test.o $externtester -o tmp/test
        c2=$?
        chmod 744 tmp/test
        ./tmp/test > ./tmp/output
        c3=$?

        if [ $c1 -eq 0 ] && [ $c2 -eq 0 ] && [ $c3 -eq 0 ] ; then
            echo "[PASSED] for "$inputsource", return code "$c3
            #rm tmp/*
        else
            echo "[FAILED] for "$inputsource
            echo "Error mask (return codes for compiler:yasm:gcc:program): "$c0" "$c1" "$c2" "$c3
            exit
        fi
        echo
    else
        [ -f "$rightoutput" ] && echo "There's output test pattern for this executable"
        [ -f "$rightinput" ] && echo "There's input test pattern for this executable"

        $lispcompiler -S -o tmp/test.yasm $inputsource
        c0=$?
        yasm -felf64 -gdwarf2 -o tmp/test.o tmp/test.yasm
        c1=$?
        gcc tmp/test.o -g -o tmp/test
        c2=$?
        chmod 744 tmp/test
        if [ -f "$rightinput" ] ; then
            ./tmp/test < $rightinput > ./tmp/output
        else
            ./tmp/test > ./tmp/output
        fi

        c3=$?

        if [ $c1 -eq 0 ] && [ $c2 -eq 0 ] ; then
            if [ -f "$rightoutput" ] && ! diff ./tmp/output $rightoutput >/dev/null ; then
                echo "[FAILED] for "$inputsource", outputs differ, check "$rightoutput" and ./tmp/output contents. Diff:\n"
                diff ./tmp/output $rightoutput
                exit
            fi
            echo "[PASSED] for "$inputsource", return code "$c3
            #rm tmp/*
        else
            echo "[FALIED] for "$inputsource
            echo "Error mask (return codes for compiler:yasm:gcc:program): "$c0" "$c1" "$c2" "$c3
            exit
        fi
        echo
    fi
done
#rm -rf tmp/
