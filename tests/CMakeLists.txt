cmake_minimum_required(VERSION 3.26)
project(tests)

set(CMAKE_CXX_STANDARD 20)

add_executable(tests main.cpp
        ../ast/BaseNode.h
        ../ast/NumberNode.h
        ../ast/NumberNode.cpp
        ../ast/VariableAccessNode.h
        ../ast/VariableAccessNode.cpp
        ../ast/BinOpNode.h
        ../ast/BinOpNode.cpp
        ../ast/FunctionNode.h
        ../ast/FunctionNode.cpp
        ../ast/ProtoFunctionStatement.h
        ../ast/ProtoFunctionStatement.cpp
        ../ast/VariableDefinitionStatement.h
        ../ast/VariableDefinitionStatement.cpp
        ../ast/CallFunctionNode.h
        ../ast/CallFunctionNode.cpp
        ../ast/IfStatement.h
        ../ast/IfStatement.cpp
        ../ast/ForLoopNode.h
        ../ast/ForLoopNode.cpp
        ../ast/UnaryOpNode.h
        ../ast/UnaryOpNode.cpp
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
)
target_compile_options(tests PRIVATE -fsanitize=address -fno-omit-frame-pointer)
target_link_options(tests PRIVATE -fsanitize=address -fno-omit-frame-pointer)
