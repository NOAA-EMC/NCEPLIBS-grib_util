# This is the CMake file for the test directory in the NCEPLIBS-grib_util
# project.
#
# Ed Hartnett

# Run each shell test.
function(gu_test name)
  # Copy the test scripts.
  file(COPY "${CMAKE_SOURCE_DIR}/tests/${name}.sh"
    DESTINATION ${CMAKE_BINARY_DIR}/tests
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
  # Add the shell script as a test.
  add_test(NAME ${name}.sh COMMAND bash ${name}.sh)
endfunction()

# Copy each necessary test data file to the binary build directory.
function(gu_copy_test_data name)
  file(COPY "${CMAKE_SOURCE_DIR}/tests/${name}"
    DESTINATION ${CMAKE_BINARY_DIR}/tests
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
endfunction()
 
# Copy necessary test data files.
gu_copy_test_data(ref_gdaswave.t00z.wcoast.0p16.f000.grib2)
gu_copy_test_data(ref_gdaswave_2.idx)
gu_copy_test_data(ref_gdaswave.degrib2.txt)
gu_copy_test_data(ref_gdaswave_grib1_inventory.txt)
gu_copy_test_data(ref_gdaswave.grb2index.idx)

# Run these shell tests.
gu_test(run_cnvgrib_tests)
gu_test(run_copygb_tests)
gu_test(run_copygb2_tests)
gu_test(run_degrib2_tests)
gu_test(run_grb2index_tests)

