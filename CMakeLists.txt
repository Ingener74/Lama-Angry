set(PROJECT_NAME LamaAngry)
project(${PROJECT_NAME})
cmake_minimum_required(VERSION 3.0)

set(
        ${PROJECT_NAME}_SOURCES
        main.cpp
)

add_executable(${PROJECT_NAME} ${${PROJECT_NAME}_SOURCES} Lexer.cpp Lexer.h)