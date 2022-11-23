# This is the main CMake file for NCEPLIBS-grib_util.
#
# Mark Potts, Kyle Gerheiser, Ed Hartnett
cmake_minimum_required(VERSION 3.15)

file(STRINGS "VERSION" pVersion)

project(gributil VERSION ${pVersion} LANGUAGES C Fortran)

# Handle user options.
option(OPENMP "use OpenMP threading" OFF)
option(ENABLE_DOCS "Enable generation of doxygen-based documentation." OFF)
option(FTP_TEST_FILES "Fetch and test with files on FTP site." OFF)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                                               "MinSizeRel" "RelWithDebInfo")
endif()

include(GNUInstallDirs)

# Find openMP if we need it.
if(OPENMP)
  find_package(OpenMP REQUIRED COMPONENTS Fortran)
endif()

# Find these required compression libraries.
find_package(Jasper 2.0.25 REQUIRED)
find_package(PNG REQUIRED)
find_package(ZLIB REQUIRED)

# Find the NCEPLIBS libraries we need.
find_package(bacio 2.4.0 REQUIRED)
find_package(sp 2.3.3 REQUIRED)
find_package(ip 3.3.3 REQUIRED)
find_package(w3emc 2.9.2 REQUIRED)
find_package(g2 3.4.0 REQUIRED)
if (g2_VERSION GREATER_EQUAL 3.5.0)
  find_package(g2c 1.7.0 REQUIRED)
endif()

# The name of the bacio library changed with the 2.5.0 release. The
# "_4" was removed from the library and include directory name in the
# bacio-2.5.0 release. Set a name variable to be used in the rest of
# the cmake build.
if(bacio_VERSION GREATER_EQUAL 2.5.0)
  set(bacio_name bacio)
else()
  set(bacio_name bacio_4)
endif()
message(STATUS "Using bacio library ${bacio_name}")

# Determine whether or not to generate documentation.
if(ENABLE_DOCS)
  find_package(Doxygen REQUIRED)
  add_subdirectory(docs)  
endif()

add_subdirectory(src)

# Turn on tests.
include(CTest)
if(BUILD_TESTING)
  add_subdirectory(tests)
endif()


