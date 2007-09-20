#!/bin/sh
# Analyse with lcov
RESULT_DIRECTORY=lcov_analyse

rm -fr $RESULT_DIRECTORY
mkdir $RESULT_DIRECTORY
lcov -d . -o $RESULT_DIRECTORY/analyse.info -c -b .
genhtml -q -o $RESULT_DIRECTORY/ -legend -highlight $RESULT_DIRECTORY/analyse.info
html2text -width 300 -nobs -ascii -o $RESULT_DIRECTORY/index.txt_tmp $RESULT_DIRECTORY/index.html
#AWS_LINES_INSTRUMENTED=`cat $RESULT_DIRECTORY/index.txt_tmp | grep aws | awk '{print $7}'`
ADAINCLUDE_LINES_INSTRUMENTED=`cat $RESULT_DIRECTORY/index.txt_tmp | grep adainc | awk '{print $7}'`
ADAINCLUDE_LINES_EXECUTED=`cat $RESULT_DIRECTORY/index.txt_tmp | grep adainc | awk '{print $5}'`

INSTRUMENTED_LINES=`cat $RESULT_DIRECTORY/index.txt_tmp | awk '/Instrumented lines/ {print $5}'`

#let "INSTRUMENTED_LINES -= $AWS_LINES_INSTRUMENTED"
let "INSTRUMENTED_LINES -= $ADAINCLUDE_LINES_INSTRUMENTED"

EXECUTED_LINES=`cat $RESULT_DIRECTORY/index.txt_tmp | awk '/Executed lines/ {print $7}'`

let "EXECUTED_LINES -= $ADAINCLUDE_LINES_EXECUTED"

CODE_COVERED=`echo "(($EXECUTED_LINES*100)/$INSTRUMENTED_LINES)" | bc`

html2text -style pretty -nobs -ascii $RESULT_DIRECTORY/index.html |
while read line
do
#     echo $line | grep -v "Instrumented lines" | grep -v "Executed lines" |
#     grep -v gcc | grep -v adainclude | grep -v aws | grep -v "code coverage report"
    echo $line | grep "lines$" | grep -v gcc | grep -v adainclude |
    grep -v morzhol | grep -v aws |
    awk '{printf "%-40s %-20s %s / %s %s\n", $1,  $2, $6, $8, $9}'
done
echo
echo -e Instrumented lines\ : $INSTRUMENTED_LINES
echo -e Executed lines\ \ \ \ \ : $EXECUTED_LINES
echo -e Code coverage \ \ \ \ \ : $CODE_COVERED%

