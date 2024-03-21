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

# Invoke interpolation logic.
../src/copygb2/copygb2 -g "30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000" -i"1 1" -x data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave_2.ip.grib2

# Are the files the same?
g2c_compare -v data/ref_gdaswave.t00z.wcoast.0p16.f000.ip.grib2 test_gdaswave_2.ip.grib2

echo "*** SUCCESS!"
exit 0
