#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# This script tests with GRIB2 data files downloaded from the NOAA EMC FTP site.
#
# Ed Hartnett, 11/24/22

set -e
echo ""
echo "*** Running all FTP file tests"

ftp_files="blend.t19z.core.f001.co.grib2 cmc_geavg.t12z.pgrb2a.0p50.f000"

for f in $ftp_files
do
    echo "Testing with file $f"
    ls -l data/$f
done

echo "*** SUCCESS!"
exit 0
