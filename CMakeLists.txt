cmake_minimum_required(VERSION 3.15)
project(grib_util VERSION 1.1.1)
set(${PROJECT_NAME}_VERSION ${PROJECT_VERSION} CACHE INTERNAL "${PROJECT_NAME} version number")
enable_language (Fortran)

set(CMAKE_FIND_PACKAGE_PREFER_CONFIG true)

if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
    "Choose the type of build, options are: PRODUCTION Debug Release."
    FORCE)
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
  set(IntelComp true )
elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU*" OR CMAKE_CXX_COMPILER_ID MATCHES "Clang*")
  set(GNUComp true )
elseif(CMAKE_CXX_COMPILER_ID MATCHES "pgc*")
  set(PGIComp true )
endif()

STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RelWithDebInfo" BUILD_RELEASE)
if(NOT BUILD_RELEASE )
  STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RELEASE" BUILD_RELEASE)
endif()
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "PRODUCTION" BUILD_PRODUCTION)
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "DEBUG" BUILD_DEBUG)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

if(POLICY CMP0054)
  cmake_policy(SET CMP0054 NEW)
endif()
if(POLICY CMP0012)
  cmake_policy(SET CMP0012 NEW)
endif()

find_package(OpenMP)
find_package(LAPACK)
find_package(Jasper REQUIRED)
find_package(PNG REQUIRED)
find_package(ZLIB REQUIRED)

#Prefer config files, and if not available use the regular FindXXX file
set(CMAKE_FIND_PACKAGE_PREFER_CONFIG true)
if(NOT TARGET w3nco_4)
  find_package(w3nco REQUIRED)
endif()

if(NOT TARGET g2_4)
  find_package(g2 REQUIRED)
endif()

if(NOT TARGET bacio_4)
  find_package(bacio REQUIRED)
endif()

if(NOT TARGET ip_4)
  find_package(ip REQUIRED)
endif()

if (NOT TARGET sp_4)
  find_package(sp REQUIRED)
endif()

add_subdirectory(sorc/cnvgrib.fd)
add_subdirectory(sorc/copygb.fd)
add_subdirectory(sorc/copygb2.fd)
add_subdirectory(sorc/grb2index.fd)
add_subdirectory(sorc/degrib2.fd)
add_subdirectory(sorc/grib2grib.fd)
add_subdirectory(sorc/grbindex.fd)
add_subdirectory(sorc/tocgrib.fd)
add_subdirectory(sorc/tocgrib2.fd)
add_subdirectory(sorc/tocgrib2super.fd)
add_subdirectory(sorc/wgrib.cd)
