rm -f l buffer
cd in
rm -f *.out
ls *legal* > ../l
cd ..
while read line 
do
echo "  TestLabel \"$line\" (parser \"" >> buffer
cat in/$line >> buffer
echo "\"" >> buffer
echo "                                           ~=?" >> buffer
rm -f temp
../../../../dist/build/parser/parser in/$line > temp 
cat temp >> buffer
echo "                                            )," >> buffer
echo >> buffer
echo >> buffer
echo >> buffer
done < l
