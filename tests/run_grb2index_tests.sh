#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# Ed Hartnett, 12/27/21

set -e
echo ""
echo "*** Running grb2index tests"

# Confirm that 2 arguments are required, otherwise an error code of 2
# is returned.
../src/grb2index/grb2index && exit 1
[ $? -ne 2 ] && exit 2
../src/grb2index/grb2index one && exit 1
[ $? -ne 2 ] && exit 2

# Confirm that input file can be read, otherwise an error code of 8 is returned.
../src/grb2index/grb2index missing1 missing2 && exit 1
[ $? -ne 8 ] && exit 2

# When there are no grib2 messages in the file, an error code of 1 is returned.
../src/grb2index/grb2index data/ref_gdaswave_2.idx missing2 && exit 1
[ $? -ne 1 ] && exit 2

# Create an index of a GRIB2 file.
../src/grb2index/grb2index 1 data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.grb2index.idx

# Check against expected output. First 140 bytes contain differences,
# so ignore them.
cmp -i 140 test_gdaswave.grb2index.idx data/ref_gdaswave.grb2index.idx

# Create an index of a GRIB2 file.
../src/grb2index/grb2index 2 data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.grb2index.idx2

# Check against expected output. First 140 bytes contain differences,
# so ignore them.
cmp -i 140 test_gdaswave.grb2index.idx2 data/ref_gdaswave.grb2index.idx2

echo "*** SUCCESS!"
exit 0
