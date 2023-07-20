#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# Ed Hartnett, 12/28/21

set -e
echo ""
echo "*** Running degrib2 tests"

# Confirm that degrib2 fails without any arguments.
degrib2 && exit 1

# Degrib2 a GRIB2 file.
../src/degrib2/degrib2 data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 &> test_gdaswave.degrib2.txt

#echo "got:"
#cat test_gdaswave.degrib2.txt
#echo "expected:"
#cat data/ref_gdaswave.degrib2.txt

# Check against expected output.
diff -w test_gdaswave.degrib2.txt data/ref_gdaswave.degrib2.txt

echo "*** SUCCESS!"
exit 0
