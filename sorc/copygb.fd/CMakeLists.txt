
if(IntelComp)
  set(fortran_flags "-g" "-O3" "-r8" "-auto" "${OpenMP_Fortran_FLAGS}")
elseif(GNUComp)
  set(fortran_flags "-g" "-O3" "-fdefault-real-8" "${OpenMP_Fortran_FLAGS}")
else()
  message("unknown compiler!")
  exit()
endif()

set(exe_name copygb)

file(GLOB fortran_src
  ${CMAKE_CURRENT_SOURCE_DIR}/*.f
  ${CMAKE_CURRENT_SOURCE_DIR}/*.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/*.F)

add_executable(${exe_name} ${fortran_src})

target_compile_options(${exe_name} PRIVATE ${fortran_flags})
set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include_8")
set_target_properties(${exe_name} PROPERTIES Fortran_MODULE_DIRECTORY ${module_dir})
target_include_directories(${exe_name} PUBLIC ${module_dir})

target_link_libraries(${exe_name} bacio_4 w3nco_4 ip_4 sp_4 ${OpenMP_Fortran_FLAGS})

install(TARGETS ${exe_name}
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib) 
install(DIRECTORY ${module_dir} DESTINATION include_8)

