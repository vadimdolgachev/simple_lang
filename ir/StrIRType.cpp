//
// Created by vadim on 25.03.25.
//

#include "StrIRType.h"

#include "ast/StringNode.h"

StrIRType::StrIRType(const bool isPointer):
    IRType(isPointer) {
    if (!isPointer) {
        throw std::invalid_argument("String type must be a pointer");
    }
}

bool StrIRType::isOperationSupported(TokenType op, const IRType *rhs) const {
    if (!dynamic_cast<const StrIRType *>(rhs)) {
        return false;
    }

    return op == TokenType::Equal ||
           op == TokenType::NotEqual ||
           op == TokenType::Plus;
}

llvm::Value *StrIRType::createBinaryOp(llvm::IRBuilder<> &builder,
                                       const TokenType op,
                                       llvm::Value *lhs,
                                       llvm::Value *rhs,
                                       const std::string &name) const {
    llvm::Module *module = builder.GetInsertBlock()->getModule();

    switch (op) {
        case TokenType::Equal:
        case TokenType::NotEqual: {
            llvm::Function *strcmpFunc = getOrDeclareStrcmp(module);
            llvm::Value *cmpResult = builder.CreateCall(
                    strcmpFunc, {lhs, rhs}, "strcmp.result");
            llvm::Value *zero = llvm::ConstantInt::get(builder.getInt32Ty(), 0);
            llvm::Value *result = builder.CreateICmp(
                    op == TokenType::Equal ? llvm::CmpInst::ICMP_EQ : llvm::CmpInst::ICMP_NE,
                    cmpResult,
                    zero,
                    name);
            return result;
        }
        case TokenType::Plus: {
            llvm::Function *strcatFunc = getOrDeclareStrcat(module);
            return builder.CreateCall(strcatFunc, {lhs, rhs}, name);
        }
        default:
            throw std::invalid_argument("Unsupported string operation");
    }
}

bool StrIRType::isUnaryOperationSupported(TokenType op) const {
    // return op == TokenType::SizeOf;
    return false;
}

llvm::Value *StrIRType::createUnaryOp(llvm::IRBuilder<> &builder,
                                      TokenType op,
                                      llvm::Value *operand,
                                      llvm::Value *storage,
                                      const std::string &name) const {
    // if (op == TokenType::SizeOf) {
    //     llvm::Function *strlenFunc = getOrDeclareStrlen(
    //             builder.GetInsertBlock()->getModule()
    //             );
    //     return builder.CreateCall(strlenFunc, {operand}, name);
    // }
    throw std::invalid_argument("Unsupported unary operation");
}

llvm::Type *StrIRType::getLLVMType(llvm::LLVMContext &context) const {
    return llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
}

llvm::Value *StrIRType::createValue(const BaseNode *node, llvm::IRBuilder<> &builder, llvm::Module &module) {
    const auto *strNode = dynamic_cast<const StringNode *>(node);
    auto *gv = new llvm::GlobalVariable(module,
                                getLLVMType(module.getContext()),
                                true,
                                llvm::GlobalValue::ExternalLinkage,
                                llvm::ConstantDataArray::getString(module.getContext(), strNode->str),
                                ".str");
    return llvm::ConstantExpr::getBitCast(gv, getLLVMType(module.getContext()));
}

llvm::Function *StrIRType::getOrDeclareStrcmp(llvm::Module *module) {
    // llvm::Function *func = module->getFunction("strcmp");
    // if (!func) {
    //     llvm::FunctionType *funcType = llvm::FunctionType::get(
    //             llvm::Type::getInt32Ty(module->getContext()),
    //             {llvm::Type::getInt8PtrTy(module->getContext()),
    //              llvm::Type::getInt8PtrTy(module->getContext())},
    //             false
    //             );
    //     func = llvm::Function::Create(
    //             funcType,
    //             llvm::Function::ExternalLinkage,
    //             "strcmp",
    //             module
    //             );
    // }
    // return func;
    return nullptr;
}

llvm::Function *StrIRType::getOrDeclareStrcat(llvm::Module *module) {
    return nullptr;
}

llvm::Function *StrIRType::getOrDeclareStrlen(llvm::Module *module) {
    return nullptr;
}
