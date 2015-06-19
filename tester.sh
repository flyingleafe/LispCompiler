#!/bin/sh

cabal build
sources=$(find lisp-sources -iname "test*.in")
for inputsource in $sources
do
    echo "processing "$inputsource
    rightoutput=${inputsource:0:$((${#inputsource}-2))}"out"
    echo "rightoutput: "$rightoutput
    [ ! -f "$rightoutput" ] && echo "No output test pattern for this executable"
    ./dist/build/compiler/compiler -o tmp/test.yasm $inputsource
    yasm -felf64 -gdwarf2 -Werror -o tmp/test.o tmp/test.yasm
    c1=$?
    gcc tmp/test.o -o tmp/test
    c2=$?
    chmod 744 tmp/test
    ./tmp/test >> ./tmp/output
    c3=$?
    if [ $c1 -eq 0 ] && [ $c2 -eq 0 ] && [ $c3 -eq 0 ]; then
        echo "[PASSED] for "$inputsource
        rm tmp/*
    else
        echo "[FALIED] for "$inputsource
        echo "Mask: "$c1" "$c2" "$c3
        exit
    fi
    echo
done
