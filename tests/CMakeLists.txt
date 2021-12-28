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

# Copy the test data file to the binary build directory.
file(COPY "${CMAKE_SOURCE_DIR}/tests/gdaswave.t00z.wcoast.0p16.f000.grib2"
  DESTINATION ${CMAKE_BINARY_DIR}/tests
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)

# Copy this reference file to the binary build directory.
file(COPY "${CMAKE_SOURCE_DIR}/tests/ref_gdaswave_2.idx"
  DESTINATION ${CMAKE_BINARY_DIR}/tests
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)

# Copy the expected GRIB1 inventory file to the binary build directory.
file(COPY "${CMAKE_SOURCE_DIR}/tests/gdaswave_grib1_inventory.txt"
  DESTINATION ${CMAKE_BINARY_DIR}/tests
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)

# Run these shell tests.
gu_test(run_cnvgrib_tests)
gu_test(run_copygb2_tests)

