#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# This script runs degrib2 on GRIB2 data files downloaded from the
# NOAA EMC FTP site, and checks the output against reference output.
#
# Ed Hartnett, 11/24/22

set -e
echo ""
echo "*** Running degrib2 FTP file tests"

ftp_files="blend.t19z.core.f001.co.grib2 \
aqm.t12z.max_8hr_o3.227.grib2 \
GLOBAL.grib2.2022103000.0000 \
hiresw.t00z.arw_5km.f00.hi.grib2 \
naefs_ge10pt.t12z.pgrb2a.0p50_bcf003 \
rap.t00z.awp130pgrbf00.grib2 \
seaice.t00z.grb.grib2 \
sgx_nwps_CG3_20221117_1200.grib2 \
cmc_geavg.t12z.pgrb2a.0p50.f000 \
WW3_Regional_US_West_Coast_20220718_0000.grib2 \
WW3_Regional_US_East_Coast_20220717_0600.grib2 \
gdas.t12z.pgrb2.1p00.anl.grib2 \
flxf2022111712.01.2022111712.grb2 \
"

for f in $ftp_files
do
    echo "Testing degrib2 with file $f"
#    ls -l data/$f
    ../src/degrib2/degrib2 data/$f > ${f}.degrib2
    diff -w ${f}.degrib2 data/ref_${f}.degrib2
done

echo "*** SUCCESS!"
exit 0
