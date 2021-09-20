if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g -r8 -auto")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g -fdefault-real-8")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

set(fortran_src copygb.F)

set(exe_name copygb)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE bacio::bacio_4 w3nco::w3nco_d ip::ip_d sp::sp_d)

if(OpenMP_Fortran_FOUND)
  target_link_libraries(${exe_name} PRIVATE OpenMP::OpenMP_Fortran)
endif()

install(TARGETS ${exe_name} RUNTIME DESTINATION bin)
