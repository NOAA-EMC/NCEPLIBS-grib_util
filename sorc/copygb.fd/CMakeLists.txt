if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g -r8 -auto")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_Fortran_FLAGS "-g -fdefault-real-8")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

set(fortran_src copygb.F)

set(exe_name copygb)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} bacio_4 w3nco_4 ip_4 sp_4 ${OpenMP_Fortran_FLAGS})

install(TARGETS ${exe_name} RUNTIME DESTINATION bin)
