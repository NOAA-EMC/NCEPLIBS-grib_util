# This is the CMake build file for the copygb utility in the
# NCEPLIBS-grib_util project.
#
# George Gayno, Mark Potts

# Set the compiler flags.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g -r8 -auto ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${CMAKE_Fortran_FLAGS}")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g -fdefault-real-8 ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${CMAKE_Fortran_FLAGS}")
endif()

# This is the fortran source code.
set(fortran_src copygb.F90)

# Build the executable.
set(exe_name copygb)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE bacio::${bacio_name} w3emc::w3emc_d ip::ip_d sp::sp_d)

# Use openMP if found.
if(OpenMP_Fortran_FOUND)
  target_link_libraries(${exe_name} PRIVATE OpenMP::OpenMP_Fortran)
endif()

if(ip_VERSION VERSION_GREATER_EQUAL 4.0.0)
  target_compile_definitions(${exe_name} PRIVATE USEIPMOD)
endif()

# Install the utility.
install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# If doxygen documentation is enabled, build it.
if(ENABLE_DOCS)
  add_subdirectory(docs)  
endif()
