cmake_minimum_required(VERSION 2.6)
set(EXENAME tocgrib )
file(GLOB EXE_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.f ${CMAKE_CURRENT_SOURCE_DIR}/*.f90 ${CMAKE_CURRENT_SOURCE_DIR}/*.F)
#file(GLOB EXE_C_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.c)
if(IntelComp)
    message("setting intel flags")
    set(CMAKE_Fortran_4_FLAGS " -g -O2 -assume noold_ldout_format -axCORE-AVX2" CACHE INTERNAL "" )
elseif(GNUComp)
    message("setting gnu flags")
    set(CMAKE_Fortran_4_FLAGS " -g -O2" CACHE INTERNAL "" )
else()
    message("unknown compiler!")
    exit()
endif()


#set_source_files_properties( ${EXE_C_SRC} PROPERTIES COMPILE_FLAGS ${CMAKE_C_FLAGS} )
#add_definitions( ${CMAKE_Fortran_4_FLAGS} )
add_executable(${EXENAME} ${EXE_SRC} ${EXE_C_SRC} )
set_target_properties( ${EXENAME} PROPERTIES COMPILE_FLAGS ${CMAKE_Fortran_4_FLAGS} )
set_target_properties( ${EXENAME} PROPERTIES LINK_FLAGS ${CMAKE_Fortran_4_FLAGS} )
include_directories( ${G2_INC4} )
set(CMAKE_INCLUDE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include_4")
set_target_properties(${EXENAME} PROPERTIES Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include_4")
target_include_directories(${EXENAME} PUBLIC "${CMAKE_CURRENT_BINARY_DIR}/include_4")
target_link_libraries(${EXENAME} ${W3NCO_LIB4} ${BACIO_LIB4} )

file(MAKE_DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY})

install(TARGETS ${EXENAME}
        RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}/bin
        LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}/lib
        ARCHIVE DESTINATION ${CMAKE_INSTALL_PREFIX}/lib) 
install(DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY}/ DESTINATION include_4 )

