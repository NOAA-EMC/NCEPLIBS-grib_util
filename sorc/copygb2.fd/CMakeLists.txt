if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g -r8 -auto -convert big_endian -fpp")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_Fortran_FLAGS "-g -fdefault-real-8 -fconvert=big-endian -cpp")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

# For gfortran-10+ backward compatibility
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" AND ${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER 9.9)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w -fallow-argument-mismatch")
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
target_link_libraries(
  ${exe_name}
  g2_d
  ${PNG_LIBRARIES}
  ${JASPER_LIBRARIES}
  bacio_4
  w3nco_d
  ip_d
  sp_d
  ${OpenMP_Fortran_FLAGS})

install(TARGETS ${exe_name} RUNTIME DESTINATION bin)
