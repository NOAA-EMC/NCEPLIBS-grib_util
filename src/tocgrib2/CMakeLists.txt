# This is the CMake build file for the tocgrib2 utility in the
# NCEPLIBS-grib_util project.
#
# Mark Potts, Kyle Gerheiser

# Set the compiler flags.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g -axCORE-AVX2 ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${CMAKE_Fortran_FLAGS}")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${CMAKE_Fortran_FLAGS}")
endif()

# This is the source code for tocgrib2.
set(fortran_src tocgrib2.F90)

# Build the executable.
set(exe_name tocgrib2)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  g2::g2_4
  w3nco::w3nco_4
  bacio::${bacio_name}
  PNG::PNG
  ${JASPER_LIBRARIES}
  ${ZLIB_LIBRARY})

# Install the tocgrib2 utility.
install(TARGETS ${exe_name} RUNTIME DESTINATION bin)

# If doxygen documentation is enabled, build it.
if(ENABLE_DOCS)
  add_subdirectory(docs)  
endif()
