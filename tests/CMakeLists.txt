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
 
# Some test files are large and are kept on the NOAA EMC FTP
# site. This function is used to download such test data. It takes two
# arguments, the URL and the file to be downloaded.
function(PULL_DATA THE_URL THE_FILE)
  if(NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/${THE_FILE}")
    file(DOWNLOAD
      ${THE_URL}/${THE_FILE}
      ${CMAKE_CURRENT_BINARY_DIR}/${THE_FILE}
      SHOW_PROGRESS
      STATUS status
      INACTIVITY_TIMEOUT 30
      )
    list(GET status 0 status_num)
    if(NOT status_num EQUAL 0 OR NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/${THE_FILE}")
      message(FATAL_ERROR "Could not download ${THE_FILE}")
    endif()
  endif()
endfunction()

# Copy necessary test data files.
gu_copy_test_data(ref_gdaswave.t00z.wcoast.0p16.f000.grib2)
gu_copy_test_data(ref_gdaswave.t00z.wcoast.0p16.f000.grib1)
gu_copy_test_data(ref_gdaswave_2.idx)
gu_copy_test_data(ref_gdaswave.degrib2.txt)
gu_copy_test_data(ref_gdaswave_grib1_inventory.txt)
gu_copy_test_data(ref_gdaswave.grb2index.idx)
gu_copy_test_data(ref_gdaswave.grbindex.grib1.idx)
gu_copy_test_data(ref_gdaswave_2.grib1.idx)
gu_copy_test_data(ref_gdaswave.t00z.wcoast.0p16.f000.grib2.idx)
gu_copy_test_data(ref_gdaswave.t00z.wcoast.0p16.f000_2.grib2.idx)

# Run these shell tests.
gu_test(run_cnvgrib_tests)
gu_test(run_copygb_tests)
gu_test(run_copygb2_tests)
gu_test(run_degrib2_tests)
gu_test(run_grbindex_tests)
gu_test(run_grb2index_tests)

# Does the user want to get extra test files from the FTP site, and
# run extra tests on them?
if(FTP_TEST_FILES)
  # Get the files from the FTP site.
  set(G2_FTP_URL "https://ftp.emc.ncep.noaa.gov/static_files/public/NCEPLIBS-g2")
  set(WW3_WEST_FILE "WW3_Regional_US_West_Coast_20220718_0000.grib2")
  set(WW3_EAST_FILE "WW3_Regional_US_East_Coast_20220717_0600.grib2")
  # Not using the East Coast file yet.
  # foreach(THE_FILE IN LISTS WW3_WEST_FILE WW3_EAST_FILE) 
  foreach(THE_FILE IN LISTS WW3_WEST_FILE) 
    PULL_DATA(${G2_FTP_URL} ${THE_FILE})
  endforeach()

  # Run these shell tests which use the FTP files.  

endif()
