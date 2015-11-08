#!/bin/sh

if  cabal build asm-tester; then
    asmtester="./dist/build/asm-tester/asm-tester"
    using_cabal=1
    echo "using cabal"
else
    using_cabal=0
fi
if [ $using_cabal -eq 0 ] && nix-build ; then
    asmtester="./result/bin/asm-tester"
    using_nix=1
    echo "using nix"
else
    using_nix=0
fi
if [ $using_nix -eq 0 ] && [ $using_cabal -eq 0 ] ; then
    echo "exiting, build tool unspecified"
    exit
fi

mkdir -p tmp/lib
sources=$(find src/stdlib/ -iname "*.asm" | sort -h)
for inputsource in $sources
do
    echo "processing "$inputsource
    $asmtester $inputsource tmp/lib/parsed.asm > ./tmp/lib/output
    cat ./tmp/lib/output | grep 'Done ""' > /dev/null
    c0=$?
    yasm -felf64 -gdwarf2 -o tmp/lib/test.o tmp/lib/parsed.asm
    c1=$?
    if [ $c0 -eq 0 ] && [ $c1 -eq 0 ] ; then
        echo "[PASSED] for "$inputsource", return code "$c1
        #rm tmp/*
    else
        echo "[FALIED] for "$inputsource
        echo "Error mask (return codes for parser(if done '' found):yasm): "$c0" "$c1" "
        if [ ! $c0 -eq 0 ] ; then
            cat ./tmp/lib/output
        fi
        exit
    fi
    echo
done
#rm -rf tmp/
