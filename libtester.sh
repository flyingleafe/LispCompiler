#!/bin/sh

cabal build compiler
mkdir -p tmp/lib
sources=$(find src/stdlib/ -iname "*.asm" | sort -h)
for inputsource in $sources
do
    echo "processing "$inputsource
    ./dist/build/asm-tester/asm-tester $inputsource tmp/lib/parsed.asm | grep "Fail"
    c0=$?
    yasm -felf64 -gdwarf2 -o tmp/lib/test.o tmp/lib/parsed.asm
    c1=$?

    if [ ! $c0 -eq 0 ] && [ $c1 -eq 0 ] ; then
        echo "[PASSED] for "$inputsource", return code "$c3
        #rm tmp/*
    else
        echo "[FALIED] for "$inputsource
        echo "Error mask (return codes for parser(if 'Fail' found):yasm): "$c0" "$c1" "
        exit
    fi
    echo
done
#rm -rf tmp/
