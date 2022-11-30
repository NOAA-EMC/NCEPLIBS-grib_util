# This is the CMake build file for the togrib2super utility in the
# NCEPLIBS-grib_util project.
#
# Mark Potts, Kyle Gerheiser

# Set the compiler flags.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g -assume noold_ldout_format -axCORE-AVX2 ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2 ${CMAKE_Fortran_FLAGS}")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2 ${CMAKE_Fortran_FLAGS}")
endif()

# This is the Fortran source code.
set(fortran_src makwmo.F90 tocgrib2super.F90)

# Build the executable.
set(exe_name tocgrib2super)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE
  g2::g2_4
  w3emc::w3emc_4
  bacio::${bacio_name}
  PNG::PNG
  ${JASPER_LIBRARIES}
  ${ZLIB_LIBRARY})

# Install the executable.
install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# If doxygen documentation is enabled, build it.
if(ENABLE_DOCS)
  add_subdirectory(docs)  
endif()
