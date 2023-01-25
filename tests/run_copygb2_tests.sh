#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# Ed Hartnett, 12/27/21

set -e
echo ""
echo "*** Running copygb2 test"

# Copy GRIB2 file.
../src/copygb2/copygb2 -x data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave_2.grib2

# Are the files the same?
g2c_compare -v data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave_2.grib2

# Create an index of the copied file.
#../src/grb2index/grb2index test_gdaswave_2.grib2 test_gdaswave_2.idx

# Check against expected output. First 120 bytes contain differences,
# so ignore them.
#cmp -i 120 test_gdaswave_2.idx data/ref_gdaswave_2.idx

echo "*** SUCCESS!"
exit 0
