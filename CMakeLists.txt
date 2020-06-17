cmake_minimum_required(VERSION 3.15)

file(STRINGS "VERSION" pVersion)

project(grib_util VERSION ${pVersion} LANGUAGES C Fortran)

option(OPENMP "use OpenMP threading" OFF)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules")

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
  find_package(OpenMP REQUIRED COMPONENTS Fortran)
endif()

find_package(Jasper REQUIRED)
find_package(PNG REQUIRED)
find_package(ZLIB REQUIRED)

find_package(w3nco CONFIG REQUIRED)
find_package(g2 CONFIG REQUIRED)
find_package(bacio CONFIG REQUIRED)
find_package(ip CONFIG REQUIRED)
find_package(sp CONFIG REQUIRED)

add_subdirectory(sorc)
