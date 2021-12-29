#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# Ed Hartnett, 12/28/21

set -e
echo ""
echo "*** Running degrib2 test"

# Degrib2 a GRIB2 file.
../src/degrib2/degrib2 gdaswave.t00z.wcoast.0p16.f000.grib2 &> gdaswave.degrib2.txt

# Check against expected output.
cmp gdaswave.degrib2.txt ref_gdaswave.degrib2.txt

echo "*** SUCCESS!"
exit 0
