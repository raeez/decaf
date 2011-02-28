rm -f l buffer
cd in
rm -f *.out
ls *legal* > ../l
cd ..
while read line 
do
echo "  TestLabel \"$line\" (semchecker_ " >> buffer
../show in/$line >> buffer
echo "                                           ~=? False" >> buffer
rm -f temp
#../../../../dist/build/semchecker/semchecker in/$line > temp 
#../show temp >> buffer
echo "                                            )," >> buffer
echo >> buffer
echo >> buffer
echo >> buffer
done < l
cd in
rm -f *.out
cd ..
rm -f temp l 

