cmake_minimum_required(VERSION 3.26)
project(tests)

set(CMAKE_CXX_STANDARD 20)

set(CPM_DOWNLOAD_VERSION 0.40.5)
if (NOT EXISTS "${CMAKE_BINARY_DIR}/cmake/CPM.cmake")
    file(DOWNLOAD https://github.com/cpm-cmake/CPM.cmake/releases/download/v${CPM_DOWNLOAD_VERSION}/CPM.cmake
            "${CMAKE_BINARY_DIR}/cmake/CPM.cmake")
endif ()
include("${CMAKE_BINARY_DIR}/cmake/CPM.cmake")

CPMAddPackage(
        NAME googletest
        GITHUB_REPOSITORY google/googletest
        GIT_TAG v1.15.2
)

add_executable(tests main.cpp
        ../ast/BaseNode.h
        ../ast/NumberNode.h
        ../ast/NumberNode.cpp
        ../ast/StringNode.h
        ../ast/StringNode.cpp
        ../ast/BooleanNode.h
        ../ast/BooleanNode.cpp
        ../ast/IdentNode.h
        ../ast/IdentNode.cpp
        ../ast/BinOpNode.h
        ../ast/BinOpNode.cpp
        ../ast/FunctionNode.h
        ../ast/FunctionNode.cpp
        ../ast/ProtoFunctionStatement.h
        ../ast/ProtoFunctionStatement.cpp
        ../ast/AssignmentNode.h
        ../ast/AssignmentNode.cpp
        ../ast/FunctionCallNode.h
        ../ast/FunctionCallNode.cpp
        ../ast/IfStatement.h
        ../ast/IfStatement.cpp
        ../ast/ForLoopNode.h
        ../ast/ForLoopNode.cpp
        ../ast/LoopCondNode.h
        ../ast/LoopCondNode.cpp
        ../ast/UnaryOpNode.h
        ../ast/UnaryOpNode.cpp
        ../ast/BlockNode.h
        ../ast/BlockNode.cpp
        ../Lexer.cpp
        ../Lexer.h
        ../Parser.h
        ../Parser.cpp
)

target_include_directories(tests PRIVATE ../)

target_compile_options(tests PRIVATE
        -Wall
        -Wpedantic
        -Wextra
        -Werror=unused-result
        -Werror=return-type
        -Wstrict-aliasing
)

enable_testing()
target_compile_options(tests PRIVATE -fsanitize=address -fno-omit-frame-pointer)
target_link_options(tests PRIVATE -fsanitize=address -fno-omit-frame-pointer)
target_link_libraries(tests PRIVATE GTest::gtest GTest::gtest_main)