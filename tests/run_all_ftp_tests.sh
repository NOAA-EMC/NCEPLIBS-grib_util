#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# This script tests with GRIB2 data files downloaded from the NOAA EMC FTP site.
#
# Ed Hartnett, 11/24/22

set -e
echo ""
echo "*** Running all FTP file tests"

ftp_files="blend.t19z.core.f001.co.grib2 \
cmc_geavg.t12z.pgrb2a.0p50.f000 \
WW3_Regional_US_West_Coast_20220718_0000.grib2 \
WW3_Regional_US_East_Coast_20220717_0600.grib2 \
gdas.t12z.pgrb2.1p00.anl.grib2"

for f in $ftp_files
do
    echo "Testing with file $f"
    ls -l data/$f
    ../src/degrib2/degrib2 data/$f > ${f}.degrib2
    diff -w ${f}.degrib2 data/ref_${f}.degrib2
done

echo "*** SUCCESS!"
exit 0
