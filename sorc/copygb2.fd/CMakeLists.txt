
if(IntelComp)
  set(fortran_flags "-g" "-O3" "-r8" "-convert" "big_endian" "-auto" "-fpp" "${OpenMP_Fortran_FLAGS}")
  set(link_flags "-static-intel")
elseif(GNUComp)
  set(fortran_flags "-g" "-O3" "-fdefault-real-8" "-fconvert=big-endian" "-cpp" 
    "${OpenMP_Fortran_FLAGS}")
  set(link_flags "-static-libgfortran")
else()
  message("unknown compiler!")
  exit()
endif()

set(exe_name copygb2)

file(GLOB fortran_src
  ${CMAKE_CURRENT_SOURCE_DIR}/*.f
  ${CMAKE_CURRENT_SOURCE_DIR}/*.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/*.F)

add_executable(${exe_name} ${fortran_src})
target_compile_options(${exe_name} PRIVATE ${fortran_flags})

set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include_8")
set_target_properties(${exe_name} PROPERTIES Fortran_MODULE_DIRECTORY ${module_dir})
target_include_directories(${exe_name} PUBLIC ${module_dir})

target_link_libraries(${exe_name} g2_d ${PNG_LIBRARIES} ${JASPER_LIBRARIES}
  bacio_4 w3nco_d ip_d sp_d ${link_flags} ${OpenMP_Fortran_FLAGS})

install(TARGETS ${exe_name}
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib) 
install(DIRECTORY ${module_dir} DESTINATION include_8)

