#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# This script runs degrib2 on a very large GRIB2 data file
# downloaded from the NOAA EMC FTP site, and checks the output against
# reference output. This test is only run of the Cmake option
# FTP_LARGE_TEST_FILES is ON.
#
# Ed Hartnett, 2/26/24

set -e
echo ""
echo "*** Running degrib2 FTP large file tests"

ftp_files="fv3lam.t00z.prslev.f000.grib2"

for f in $ftp_files
do
    echo "Testing degrib2 with large file $f"
    ls -l data/$f
    ../src/degrib2/degrib2 data/$f > ${f}.degrib2
    diff -w ${f}.degrib2 data/ref_${f}.degrib2
done

echo "*** SUCCESS!"
exit 0
