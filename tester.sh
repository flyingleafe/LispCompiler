#!/bin/sh

cabal build compiler
mkdir tmp
sources=$(find lisp-sources -iname "test*.lisp" | sort -h)
for inputsource in $sources
do
    echo "processing "$inputsource

    prefix=${inputsource:0:$((${#inputsource}-5))}
    rightoutput=$prefix".out"
    externtester=$prefix".cpp"


    if [ -f "$externtester" ] ; then
        echo "Using extern tester: "$externtester

        # Compiling lisp → yasm → elf
        ./dist/build/compiler/compiler -M -o tmp/test.yasm $inputsource
        c0=$?
        yasm -felf64 -gdwarf2 -o tmp/test.o tmp/test.yasm
        c1=$?
        clang++ tmp/test.o $externtester -o tmp/test
        c2=$?
        chmod 744 tmp/test
        ./tmp/test >> ./tmp/output
        c3=$?

        if [ $c1 -eq 0 ] && [ $c2 -eq 0 ] && [ $c3 -eq 0 ] ; then
            echo "[PASSED] for "$inputsource", return code "$c3
            rm tmp/*
        else
            echo "[FALIED] for "$inputsource
            echo "Error mask (return codes for compiler:yasm:gcc:program): "$c0" "$c1" "$c2" "$c3
            exit
        fi
        echo
    else
        [ ! -f "$rightoutput" ] && echo "No output test pattern for this executable, checking only compiltion"

        ./dist/build/compiler/compiler -o tmp/test.yasm $inputsource
        c0=$?
        yasm -felf64 -gdwarf2 -o tmp/test.o tmp/test.yasm
        c1=$?
        gcc tmp/test.o -o tmp/test
        c2=$?
        chmod 744 tmp/test
        ./tmp/test >> ./tmp/output
        c3=$?

        if [ $c1 -eq 0 ] && [ $c2 -eq 0 ] ; then
            if [ -f "$rightoutput" ] && ! diff ./tmp/output $rightoutput >/dev/null ; then
                echo "[FAILED] for "$inputsource", outputs differ, check "$rightoutput" and ./tmp/output contents"
                exit
            fi
            echo "[PASSED] for "$inputsource", return code "$c3
            rm tmp/*
        else
            echo "[FALIED] for "$inputsource
            echo "Error mask (return codes for compiler:yasm:gcc:program): "$c0" "$c1" "$c2" "$c3
            exit
        fi
        echo
    fi
done
rm -rf tmp/
