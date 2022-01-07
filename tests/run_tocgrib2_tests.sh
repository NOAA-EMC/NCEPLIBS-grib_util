#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# Ed Hartnett, 1/7/22

set -e
echo ""
echo "*** Running tocgrib2 test"

# Copy GRIB2 file.
#../src/copygb/copygb -x ref_gdaswave.t00z.wcoast.0p16.f000.grib1 test_gdaswave_2.grib1

# Create an index of the copied file.
#../src/grbindex/grbindex test_gdaswave_2.grib1 test_gdaswave_2.grib1.idx

# Check against expected output. First 120 bytes contain differences,
# so ignore them.
#cmp -i 120 test_gdaswave_2.grib1.idx ref_gdaswave_2.grib1.idx

echo "*** SUCCESS!"
exit 0
