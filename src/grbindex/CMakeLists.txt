# This is the CMake build file for the grbindex utility in the
# NCEPLIBS-grib_util project.
#
# George Gayno, Mark Potts

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS
      "-g -assume noold_ldout_format -convert big_endian -axCORE-AVX2 -fpp ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${CMAKE_Fortran_FLAGS}")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g -fconvert=big-endian -cpp ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${CMAKE_Fortran_FLAGS}")
endif()

set(fortran_src grbindex.f)

set(exe_name grbindex)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE w3nco::w3nco_4 bacio::${bacio_name})

install(TARGETS ${exe_name} RUNTIME DESTINATION bin)

# If doxygen documentation is enabled, build it.
if(ENABLE_DOCS)
  add_subdirectory(docs)  
endif()
