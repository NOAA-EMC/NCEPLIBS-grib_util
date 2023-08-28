# This is the CMake build file for the tocgrib utility in the
# NCEPLIBS-grib_util project.
#
# Mark Potts, Kyle Gerheiser

# Set compiler flags.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|IntelLLVM)$")
  set(CMAKE_Fortran_FLAGS "-g -assume noold_ldout_format -axCORE-AVX2 ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2 ${CMAKE_Fortran_FLAGS}")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2 ${CMAKE_Fortran_FLAGS}")
endif()

# This is the Fortran source code.
set(fortran_src makwmo.F90 mkfldsep.F90 tocgrib.F90)

# Build the executable.
set(exe_name tocgrib)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE w3emc::w3emc_4 bacio::${bacio_name})

# Install the utility.
install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# If doxygen documentation is enabled, build it.
if(ENABLE_DOCS)
  add_subdirectory(docs)  
endif()
