# This is the CMake build file for the degrib2 utility in the
# NCEPLIBS-grib_util project.
#
# George Gayno, Mark Potts

# Set compiler flags.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|IntelLLVM)$")
  set(CMAKE_Fortran_FLAGS "-g -convert big_endian -fpp ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${CMAKE_Fortran_FLAGS}")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g -fconvert=big-endian -cpp ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${CMAKE_Fortran_FLAGS}")
endif()

# These are the fortran source files.
set(fortran_src degrib2.F90 prlevel.F90 prvtime.F90)

# Build the executable.
set(exe_name degrib2)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE
  g2::g2_4
  w3emc::w3emc_4
  bacio::${bacio_name}
  PNG::PNG
  ${JASPER_LIBRARIES}
  ${ZLIB_LIBRARY})

# Install the degrib2 utility.
install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# If doxygen documentation is enabled, build it.
if(ENABLE_DOCS)
  add_subdirectory(docs)  
endif()
