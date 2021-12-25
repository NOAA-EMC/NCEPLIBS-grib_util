# This is the CMake file for the test directory in the NCEPLIBS-grib_util
# project.
#
# Ed Hartnett

FILE(COPY "${CMAKE_SOURCE_DIR}/tests/run_tests.sh"
  DESTINATION ${CMAKE_BINARY_DIR}/tests
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

add_test(NAME run_tests.sh COMMAND bash run_tests.sh)

