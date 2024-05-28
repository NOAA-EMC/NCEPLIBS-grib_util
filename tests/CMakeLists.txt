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
gu_copy_test_data(ref_copygb2_test_gdaswave_2.ip.grib2.degrib2)
gu_copy_test_data(ref_copygb2_test_gdaswave.degrib2.txt)
gu_copy_test_data(ref_gdaswave.t00z.wcoast.0p16.f000.grib2)
gu_copy_test_data(ref_gdaswave.t00z.wcoast.0p16.f000.ip.grib2)
gu_copy_test_data(ref_gdaswave.t00z.wcoast.0p16.f000.grib1)
gu_copy_test_data(ref_gdaswave_2.idx)
gu_copy_test_data(ref_gdaswave.degrib2.txt)
gu_copy_test_data(ref_gdaswave_grib1_inventory.txt)
gu_copy_test_data(ref_gdaswave.grb2index.idx)
gu_copy_test_data(ref_gdaswave.grb2index.idx2)
gu_copy_test_data(ref_gdaswave.grbindex.grib1.idx)
gu_copy_test_data(ref_gdaswave_2.grib1.idx)
gu_copy_test_data(ref_gdaswave.t00z.wcoast.0p16.f000.grib2.idx)
gu_copy_test_data(ref_gdaswave.t00z.wcoast.0p16.f000_2.grib2.idx)
gu_copy_test_data(ref_gfs.landmask.grib1)
gu_copy_test_data(ref_grid_172.landmask.grib1)
gu_copy_test_data(ref_grid_220.landmask.grib1)
gu_copy_test_data(tocgrib2.nml)
gu_copy_test_data(tocgrib2_bad.nml)
if(FTP_TEST_FILES)
  message(STATUS "Getting FTP test files...")  
  gu_copy_test_data(ref_blend.t19z.core.f001.co.grib2.degrib2)
  gu_copy_test_data(ref_cmc_geavg.t12z.pgrb2a.0p50.f000.degrib2)
  gu_copy_test_data(ref_WW3_Regional_US_West_Coast_20220718_0000.grib2.degrib2)
  gu_copy_test_data(ref_WW3_Regional_US_East_Coast_20220717_0600.grib2.degrib2)
  gu_copy_test_data(ref_gdas.t12z.pgrb2.1p00.anl.grib2.degrib2)
  gu_copy_test_data(ref_flxf2022111712.01.2022111712.grb2.degrib2)
  gu_copy_test_data(ref_GLOBAL.grib2.2022103000.0000.degrib2)
  gu_copy_test_data(ref_hiresw.t00z.arw_5km.f00.hi.grib2.degrib2)
  gu_copy_test_data(ref_naefs_ge10pt.t12z.pgrb2a.0p50_bcf003.degrib2)
  gu_copy_test_data(ref_rap.t00z.awp130pgrbf00.grib2.degrib2)
  gu_copy_test_data(ref_seaice.t00z.grb.grib2.degrib2)
  gu_copy_test_data(ref_sgx_nwps_CG3_20221117_1200.grib2.degrib2)
  gu_copy_test_data(ref_aqm.t12z.max_8hr_o3.227.grib2.degrib2)
  if(FTP_LARGE_TEST_FILES)
    message(STATUS "Getting FTP large test file...")  
    gu_copy_test_data(ref_fv3lam.t00z.prslev.f000.grib2.degrib2)
  endif()
  if(FTP_EXTRA_TEST_FILES)
    message(STATUS "Getting extra FTP large test files...")  
#   gu_copy_test_data(ref_rrfs.t12z.prslevfaa.f010.na3km.grib2.degrib2)
    gu_copy_test_data(ref_GFSPRS.GrbF06.degrib2)
    gu_copy_test_data(ref_rrfs.t18z.prslev.f000.grib2.degrib2)
#    gu_copy_test_data(ref_grib2.awips.rrfs.010)
  endif()
endif()

# Run these shell tests.
gu_test(run_cnvgrib_tests)
gu_test(run_copygb_tests)
gu_test(run_copygb2_tests2)
gu_test(run_degrib2_tests)
gu_test(run_grbindex_tests)
gu_test(run_grb2index_tests)
gu_test(run_tocgrib2_tests)
gu_test(run_tocgrib2super_tests)

if(G2C_COMPARE)
  find_program(G2C_COMPARE g2c_compare)
  gu_test(run_copygb2_tests)
else()
  message(STATUS "g2c_compare not found.")
endif()

# Does the user want to get extra test files from the FTP site, and
# run extra tests on them?
if(FTP_TEST_FILES)
  set(G2_FTP_URL "https://ftp.emc.ncep.noaa.gov/static_files/public/NCEPLIBS-g2")

  set(FTP_TEST_FILES WW3_Regional_US_West_Coast_20220718_0000.grib2
    WW3_Regional_US_East_Coast_20220717_0600.grib2
    gdas.t12z.pgrb2.1p00.anl.grib2
    blend.t19z.core.f001.co.grib2
    cmc_geavg.t12z.pgrb2a.0p50.f000
    flxf2022111712.01.2022111712.grb2
    GLOBAL.grib2.2022103000.0000
    hiresw.t00z.arw_5km.f00.hi.grib2
    naefs_ge10pt.t12z.pgrb2a.0p50_bcf003
    rap.t00z.awp130pgrbf00.grib2
    seaice.t00z.grb.grib2
    sgx_nwps_CG3_20221117_1200.grib2
    aqm.t12z.max_8hr_o3.227.grib2
    rrfs.t12z.prslevfaa.f010.na3km.grib2 
    )
  foreach(THE_FILE IN LISTS FTP_TEST_FILES)
    PULL_DATA(${G2_FTP_URL} ${THE_FILE})
  endforeach()

  # Fetch a large test file from FTP if desired.
  set(LARGE_FTP_FILES fv3lam.t00z.prslev.f000.grib2)
  if(FTP_LARGE_TEST_FILES)
    foreach(THE_FILE IN LISTS LARGE_FTP_FILES) 
      PULL_DATA(${G2_FTP_URL} ${THE_FILE})
    endforeach()
    add_definitions(-DLARGE_FTP_TESTS)
  endif()

  # Fetch the even more large test files from FTP if desired.
  set(EXTRA_FTP_FILES  rrfs.t12z.prslevfaa.f010.na3km.grib2
    rrfs.t18z.prslev.f000.grib2 GFSPRS.GrbF06 grib2.awips.rrfs.010)
  if(FTP_EXTRA_TEST_FILES)
    foreach(THE_FILE IN LISTS EXTRA_FTP_FILES) 
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
  gu_test(run_degrib2_ftp_tests)
  if(FTP_LARGE_TEST_FILES)
    gu_test(run_degrib2_large_file_tests)
  endif()
  if(FTP_EXTRA_TEST_FILES)
    gu_test(run_degrib2_extra_file_tests)
    gu_test(run_tocgrib2_extra_file_tests)
    gu_test(run_tocgrib2super_extra_file_tests)
  endif()
endif()
