cmake_minimum_required(VERSION 3.26)
project(tests)

set(CMAKE_CXX_STANDARD 23)

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
        ../ast/LoopCondNode.h
        ../ast/LoopCondNode.cpp
        ../ast/UnaryOpNode.h
        ../ast/UnaryOpNode.cpp
        ../ast/BlockNode.h
        ../ast/BlockNode.cpp
        ../ast/DeclarationNode.cpp
        ../ast/DeclarationNode.h
        ../ast/ArrayNode.cpp
        ../ast/ArrayNode.h
        ../ast/IndexAccessNode.h
        ../ast/IndexAccessNode.cpp
        ../ast/ReturnNode.h
        ../ast/ReturnNode.cpp
        ../ast/TernaryOperatorNode.h
        ../ast/TernaryOperatorNode.cpp
        ../ast/MemberAccessNode.h
        ../ast/MemberAccessNode.cpp
        ../ast/MethodCallNode.h
        ../ast/MethodCallNode.cpp
        ../ast/FieldAccessNode.h
        ../ast/FieldAccessNode.cpp
        ../ast/CommentNode.h
        ../ast/CommentNode.cpp
        ../type/Type.cpp
        ../type/Type.h
        ../type/NumericType.cpp
        ../type/NumericType.h
        ../type/StrType.cpp
        ../type/StrType.h
        ../type/PointerType.cpp
        ../type/PointerType.h
        ../type/BoolType.cpp
        ../type/BoolType.h
        ../type/VoidType.cpp
        ../type/VoidType.h
        ../type/ByteType.cpp
        ../type/ByteType.h
        ../type/CharType.cpp
        ../type/CharType.h
        ../type/TypeFactory.cpp
        ../type/TypeFactory.h
        ../type/FunctionType.h
        ../type/FunctionType.cpp
        ../type/ArrayType.h
        ../type/ArrayType.cpp
        ../Lexer.cpp
        ../Lexer.h
        ../Parser.h
        ../Parser.cpp
        ../ast/StructDeclarationNode.h
        ../ast/StructDeclarationNode.cpp
        ../ast/StructInitNode.h
        ../ast/StructInitNode.cpp
        ../type/StructType.h
        ../type/StructType.cpp
        ../type/UnresolvedType.h
        ../type/UnresolvedType.cpp
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