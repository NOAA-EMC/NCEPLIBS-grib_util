#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# Ed Hartnett, 12/30/21

set -e
echo ""
echo "*** Running grbindex test"

# Create an index of a GRIB2 file.
../src/grbindex/grbindex data/ref_gdaswave.t00z.wcoast.0p16.f000.grib1 test_gdaswave.grbindex.grib1.idx

# Check against expected output. First 120 bytes contain differences,
# so ignore them.
cmp -i 120 test_gdaswave.grbindex.grib1.idx data/ref_gdaswave.grbindex.grib1.idx

echo "*** SUCCESS!"
exit 0
