#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# Ed Hartnett, 12/25/21

set -e
echo ""
echo "*** Running cnvgrib test"

# Convert test file to GRIB1.
../src/cnvgrib/cnvgrib -g21 ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.t00z.wcoast.0p16.f000.grib1

# Generate an inventory of the GRIB1 file.
../src/wgrib/wgrib test_gdaswave.t00z.wcoast.0p16.f000.grib1 &> test_gdaswave.t00z.wcoast.0p16.f000.grib1.inventory.txt

# Check against expected output.
cmp test_gdaswave.t00z.wcoast.0p16.f000.grib1.inventory.txt ref_gdaswave_grib1_inventory.txt

# Convert GRIB1 output back to GRIB2.
../src/cnvgrib/cnvgrib -g12 test_gdaswave.t00z.wcoast.0p16.f000.grib1 test_gdaswave.t00z.wcoast.0p16.f000.grib2


echo "*** SUCCESS!"
exit 0
