# This is the CMake file for the test directory in the NCEPLIBS-grib_util
# project.
#
# Ed Hartnett

# Copy the test data file to the binary build directory.
file(COPY "${CMAKE_SOURCE_DIR}/tests/gdaswave.t00z.wcoast.0p16.f000.grib2"
  DESTINATION ${CMAKE_BINARY_DIR}/tests
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)

# Copy the expected GRIB1 inventory file to the binary build directory.
file(COPY "${CMAKE_SOURCE_DIR}/tests/gdaswave_grib1_inventory.txt"
  DESTINATION ${CMAKE_BINARY_DIR}/tests
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)

# Copy the test script.
file(COPY "${CMAKE_SOURCE_DIR}/tests/run_tests.sh"
  DESTINATION ${CMAKE_BINARY_DIR}/tests
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

# Tell CMake to run the test script.
add_test(NAME run_tests.sh COMMAND bash run_tests.sh)

