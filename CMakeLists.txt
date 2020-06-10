cmake_minimum_required(VERSION 3.15)
project(grib_util VERSION 1.1.1 LANGUAGES C Fortran)

option(OPENMP "use OpenMP threading" OFF)

set(CMAKE_FIND_PACKAGE_PREFER_CONFIG true)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                                               "MinSizeRel" "RelWithDebInfo")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(Intel|GNU|Clang|AppleClang)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_C_COMPILER_ID}")
endif()

if(POLICY CMP0054)
  cmake_policy(SET CMP0054 NEW)
endif()
if(POLICY CMP0012)
  cmake_policy(SET CMP0012 NEW)
endif()

if(OPENMP)
  find_package(OpenMP)
endif()

find_package(LAPACK)
find_package(Jasper REQUIRED)
find_package(PNG REQUIRED)
find_package(ZLIB REQUIRED)

find_package(w3nco REQUIRED)
find_package(g2 REQUIRED)
find_package(bacio REQUIRED)
find_package(ip REQUIRED)
find_package(sp REQUIRED)

add_subdirectory(sorc)

if (ENABLE_TESTS)
  find_package(PFUNIT REQUIRED)
  enable_testing()
  add_subdirectory(tests)
endif()
