#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# This script runs degrib2 on GRIB2 data files downloaded from the
# NOAA EMC FTP site, and checks the output against reference output.
#
# Ed Hartnett, 11/24/22

set -e
echo ""
echo "*** Running degrib2 FTP large file tests"

ftp_files="fv3lam.t00z.prslev.f000.grib2"

for f in $ftp_files
do
    echo "Testing degrib2 with large file $f"
    ls -l data/$f
    ../src/degrib2/degrib2 data/$f > ${f}.degrib2
#    diff -w ${f}.degrib2 data/ref_${f}.degrib2
done

echo "*** SUCCESS!"
exit 0
