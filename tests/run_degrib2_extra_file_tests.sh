#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# This script runs degrib2 on a extra very large GRIB2 data files
# downloaded from the NOAA EMC FTP site, and checks the output against
# reference output. This test is only run of the Cmake option
# FTP_EXTRA_TEST_FILES is ON.
#
# Ed Hartnett, 2/21/24

set -e
echo ""
echo "*** Running degrib2 FTP extra file tests"

ftp_files="rrfs.t12z.prslevfaa.f010.na3km.grib2 rrfs.t18z.prslev.f000.grib2 GFSPRS.GrbF06"

for f in $ftp_files
do
    echo "Testing degrib2 with large file $f"
    ls -l data/$f
    ../src/degrib2/degrib2 data/$f > ${f}.degrib2
    diff -w ${f}.degrib2 data/ref_${f}.degrib2
done

echo "*** SUCCESS!"
exit 0
