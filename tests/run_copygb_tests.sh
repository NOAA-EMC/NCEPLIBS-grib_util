#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# Ed Hartnett, 12/30/21

set -e
echo ""
echo "*** Running copygb test"

# Copy GRIB1 file.
../src/copygb/copygb -x data/ref_gdaswave.t00z.wcoast.0p16.f000.grib1 test_gdaswave_2.grib1

# Create an index of the copied file.
../src/grbindex/grbindex test_gdaswave_2.grib1 test_gdaswave_2.grib1.idx

# Check against expected output. First 120 bytes contain differences,
# so ignore them.
cmp -i 120 test_gdaswave_2.grib1.idx data/ref_gdaswave_2.grib1.idx

# Interpolate GFS landmask to NCEP grid 172
../src/copygb/copygb -g172 -x data/ref_gfs.landmask.grib1 172.land.grib1
cmp 172.land.grib1 data/ref_grid_172.landmask.grib1

# Interpolate GFS landmask to NCEP grid 220
../src/copygb/copygb -g220 -x data/ref_gfs.landmask.grib1 220.land.grib1
cmp 220.land.grib1 data/ref_grid_220.landmask.grib1

echo "*** SUCCESS!"
exit 0
