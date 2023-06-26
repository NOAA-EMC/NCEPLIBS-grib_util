# This is the CMake file for the test directory in the NCEPLIBS-grib_util
# project.
#
# Ed Hartnett

# This fortran test is for the internals of degrib2.
add_executable(test_degrib2_int test_degrib2_int.F90 ${CMAKE_SOURCE_DIR}/src/degrib2/prlevel.F90
  ${CMAKE_SOURCE_DIR}/src/degrib2/prvtime.F90)
target_link_libraries(test_degrib2_int PRIVATE g2::g2_4)
add_test(NAME test_degrib2_int COMMAND test_degrib2_int)

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
  file(COPY "${CMAKE_SOURCE_DIR}/tests/data/${name}"
    DESTINATION ${CMAKE_BINARY_DIR}/tests/data
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
endfunction()
 
# Some test files are large and are kept on the NOAA EMC FTP
# site. This function is used to download such test data. It takes two
# arguments, the URL and the file to be downloaded.
function(PULL_DATA THE_URL THE_FILE)
  # If the TEST_FILE_DIR was specified, look for our test data files
  # there before FTPing them. Developers can keep all test files on
  # their machines, and save the time of downloading them every time.
  if(NOT ${TEST_FILE_DIR} STREQUAL ".")
    if (EXISTS ${TEST_FILE_DIR}/${THE_FILE})
      message(STATUS "Copying file ${TEST_FILE_DIR}/${THE_FILE} to test data directory.")
      FILE(COPY ${TEST_FILE_DIR}/${THE_FILE}
        DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/data)
    endif()
  endif()
  if(NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/data/${THE_FILE}")
    message(STATUS "Downloading file ${CMAKE_CURRENT_BINARY_DIR}/data/${THE_FILE}.")
    file(DOWNLOAD
      ${THE_URL}/${THE_FILE}
      ${CMAKE_CURRENT_BINARY_DIR}/data/${THE_FILE}
      SHOW_PROGRESS
      STATUS status
      INACTIVITY_TIMEOUT 30
    )
    list(GET status 0 status_num)
    if(NOT status_num EQUAL 0 OR NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/data/${THE_FILE}")
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
gu_copy_test_data(ref_gfs.landmask.grib1)
gu_copy_test_data(ref_grid_172.landmask.grib1)
gu_copy_test_data(ref_grid_220.landmask.grib1)
gu_copy_test_data(ref_blend.t19z.core.f001.co.grib2.degrib2)
gu_copy_test_data(ref_cmc_geavg.t12z.pgrb2a.0p50.f000.degrib2)
gu_copy_test_data(ref_WW3_Regional_US_West_Coast_20220718_0000.grib2.degrib2)
gu_copy_test_data(ref_WW3_Regional_US_East_Coast_20220717_0600.grib2.degrib2)
gu_copy_test_data(ref_gdas.t12z.pgrb2.1p00.anl.grib2.degrib2)

# Run these shell tests.
gu_test(run_cnvgrib_tests)
gu_test(run_copygb_tests)
gu_test(run_degrib2_tests)
gu_test(run_grbindex_tests)
gu_test(run_grb2index_tests)

# This test depends on the g2c_compare utility, which may not be
# present.
if (G2C_COMPARE)
  gu_test(run_copygb2_tests)
endif()

# Does the user want to get extra test files from the FTP site, and
# run extra tests on them?
if(FTP_TEST_FILES)
  set(G2_FTP_URL "https://ftp.emc.ncep.noaa.gov/static_files/public/NCEPLIBS-g2")
  set(WW3_WEST_FILE "WW3_Regional_US_West_Coast_20220718_0000.grib2")
  set(WW3_EAST_FILE "WW3_Regional_US_East_Coast_20220717_0600.grib2")
  set(GDAS_FILE "gdas.t12z.pgrb2.1p00.anl.grib2")
  set(BLEND_FILE "blend.t19z.core.f001.co.grib2")
  set(CMC_FILE "cmc_geavg.t12z.pgrb2a.0p50.f000")
  
  # Get the files from the FTP site.
  foreach(THE_FILE IN LISTS WW3_WEST_FILE WW3_EAST_FILE GDAS_FILE BLEND_FILE CMC_FILE) 
    PULL_DATA(${G2_FTP_URL} ${THE_FILE})
  endforeach()

  # Fetch the large test file(s) from FTP if desired.
  set(LARGE_FTP_FILES "fv3lam.t00z.namerica.f00.grib2")
  if(LARGE_FTP_TESTS)
    foreach(THE_FILE IN LISTS LARGE_FTP_FILES) 
      PULL_DATA(${G2_FTP_URL} ${THE_FILE})
    endforeach()
    add_definitions(-DLARGE_FTP_TESTS)
  endif()

  # Copy this inventory from the test directory. It's the expected
  # inventory for one of the FTP files, after conversion to GRIB1.
  gu_copy_test_data(ref_test_WW3_West.grib1.inventory.txt)
  gu_copy_test_data(ref_test_WW3_West.grib2.idx)

  # Run these shell tests which use the FTP files.  
  gu_test(run_ftp_tests)
  gu_test(run_all_ftp_tests)
endif()
