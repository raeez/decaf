rm -f l buffer
cd in
rm -f *.out
ls * > ../l
cd ..
while read line 
do
echo "  TestLabel \"$line\" (scannerShow " >> buffer
../show in/$line >> buffer
echo "                                           ~=?" >> buffer
rm -f temp
../../../../dist/build/scanner/scanner in/$line > temp 
../show temp >> buffer
echo "                                            )," >> buffer
echo >> buffer
echo >> buffer
echo >> buffer
done < l
