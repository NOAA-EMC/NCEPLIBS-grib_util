#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# This tests the copygb2 utility. This test is similar to
# run_copygb2_tests.sh, but this test does not use the g2c_compare
# utility.
#
# Ed Hartnett, 3/21/24

set -e
echo ""
echo "*** Running copygb2 test"

# Copy GRIB2 file.
../src/copygb2/copygb2 -x data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave_2.grib2

# Make degrib2 output for the copied file.
../src/degrib2/degrib2 test_gdaswave_2.grib2 > test_gdaswave_2.grib2.degrib2

# Check against expected output.
#diff -w test_gdaswave_2.grib2.degrib2 data/ref_copygb2_test_gdaswave.degrib2.txt

# Invoke interpolation logic.
#../src/copygb2/copygb2 -g "30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000" -i"1 1" -x data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave_2.ip.grib2

# Make degrib2 output for the copied file.
#../src/degrib2/degrib2 test_gdaswave_2.ip.grib2 > test_gdaswave_2.ip.grib2.degrib2

# Check against expected output.
#diff -w test_gdaswave_2.ip.grib2.degrib2 data/ref_copygb2_test_gdaswave_2.ip.grib2.degrib2

echo "*** SUCCESS!"
exit 0
