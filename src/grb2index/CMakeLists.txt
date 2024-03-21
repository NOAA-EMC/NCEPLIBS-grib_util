# This is the CMake build file for the grb2index utility in the
# NCEPLIBS-grib_util project.
#
# Mark Potts, Kyle Gerheiser

# Set compiler flags.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|IntelLLVM)$")
  set(CMAKE_Fortran_FLAGS "-g -convert big_endian -fpp ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${CMAKE_Fortran_FLAGS}")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g -fconvert=big-endian -cpp ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${CMAKE_Fortran_FLAGS}")
endif()

# Build the executable.
set(exe_name grb2index)
add_executable(${exe_name} grb2index.F90)
target_link_libraries(${exe_name} PRIVATE g2::g2_4 w3emc::w3emc_4 bacio::${bacio_name})

# Install the utility.
install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# If doxygen documentation is enabled, build it.
if(ENABLE_DOCS)
  add_subdirectory(docs)  
endif()
