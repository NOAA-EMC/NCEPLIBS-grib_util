if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g -assume noold_ldout_format -axCORE-AVX2")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
endif()

set(fortran_src makwmo.f mkfldsep.f tocgrib.f)

set(exe_name tocgrib)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} w3nco::w3nco_4 bacio::bacio_4)

install(TARGETS ${exe_name} RUNTIME DESTINATION bin)
