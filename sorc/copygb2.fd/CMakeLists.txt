if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g -r8 -auto -convert big_endian -fpp")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g -fdefault-real-8 -fconvert=big-endian -cpp")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

set(fortran_src
  copygb2.f
  polates1.f90
  polates3.f90
  polatev0.f90
  polatev2.f90
  polatev6.f90
  polates0.f90
  polates2.f90
  polates6.f90
  polatev1.f90
  polatev3.f90)

set(exe_name copygb2)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE
  g2::g2_d
  PNG::PNG
  ${JASPER_LIBRARIES}
  bacio::bacio_4
  w3nco::w3nco_d
  ip::ip_d
  sp::sp_d)

if(OpenMP_Fortran_FOUND)
  target_link_libraries(${exe_name} PRIVATE OpenMP::OpenMP_Fortran)
endif()

install(TARGETS ${exe_name} RUNTIME DESTINATION bin)
