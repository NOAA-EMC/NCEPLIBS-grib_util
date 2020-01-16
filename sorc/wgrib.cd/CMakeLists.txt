
if(IntelComp)
  set(c_flags "-O3" "-m64")
elseif(GNUComp)
  set(c_flags "-O3" "-m64")
else()
  message("unknown compiler!")
  exit()
endif()

set(c_definitions "DEF_T62_NCEP_TABLE=opn" "FAST_GRIBTAB" "P_TABLE_FIRST")

set(exe_name wgrib)
file(GLOB c_src ${CMAKE_CURRENT_SOURCE_DIR}/*.c)

add_executable(${exe_name} ${c_src})
target_compile_definitions(${exe_name} PRIVATE ${c_definitions})
target_compile_options(${exe_name} PRIVATE ${c_flags})

target_link_libraries(${exe_name} w3nco_4 bacio_4)

install(TARGETS ${exe_name}
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib) 
install(DIRECTORY ${module_dir} DESTINATION include_4 )

