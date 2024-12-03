cmake_minimum_required(VERSION 3.28)
project(template)

set(CMAKE_CXX_STANDARD 23)
if(NOT ${FB_STDLIB} STREQUAL "")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=${FB_STDLIB}")
endif()

add_executable(${PROJECT_NAME} main.cpp)
target_include_directories(${PROJECT_NAME} PUBLIC include)
