# This is the CMake build file for the tocgrib utility in the
# NCEPLIBS-grib_util project.
#
# Mark Potts, Kyle Gerheiser

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g -assume noold_ldout_format -axCORE-AVX2 ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2 ${CMAKE_Fortran_FLAGS}")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2 ${CMAKE_Fortran_FLAGS}")
endif()

set(fortran_src makwmo.f mkfldsep.f tocgrib.f)

set(exe_name tocgrib)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE w3emc::w3emc_4 bacio::${bacio_name})

install(TARGETS ${exe_name} RUNTIME DESTINATION bin)

# If doxygen documentation is enabled, build it.
if(ENABLE_DOCS)
  add_subdirectory(docs)  
endif()
