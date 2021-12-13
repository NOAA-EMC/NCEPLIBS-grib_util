# This is the CMake build file for the togrib2super utility in the
# NCEPLIBS-grib_util project.
#
# Mark Potts, Kyle Gerheiser

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g -assume noold_ldout_format -axCORE-AVX2")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
endif()

set(fortran_src makwmo.f tocgrib2super.f)

set(exe_name tocgrib2super)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE
  g2::g2_4
  w3nco::w3nco_4
  bacio::${bacio_name}
  PNG::PNG
  ${JASPER_LIBRARIES}
  ${ZLIB_LIBRARY})

install(TARGETS ${exe_name} RUNTIME DESTINATION bin)

# If doxygen documentation is enabled, build it.
if(ENABLE_DOCS)
  add_subdirectory(docs)  
endif()
