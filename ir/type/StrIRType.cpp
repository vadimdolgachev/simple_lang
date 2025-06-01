//
// Created by vadim on 25.03.25.
//

#include "StrIRType.h"
#include "../IRValue.h"
#include "ast/StringNode.h"
#include "../../type/Type.h"

StrIRType::StrIRType(const bool isPointer):
    IRType(isPointer) {
    if (!isPointer) {
        throw std::invalid_argument("String type must be a pointer");
    }
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
            auto *const strcmpFunc = getOrDeclareStrcmp(module);
            auto *const cmpResult = builder.CreateCall(strcmpFunc,
                                                       {lhs, rhs},
                                                       "strcmp.result");
            auto *const zero = llvm::ConstantInt::get(builder.getInt32Ty(), 0);
            auto *const result = builder.CreateICmp(
                    op == TokenType::Equal ? llvm::CmpInst::ICMP_EQ : llvm::CmpInst::ICMP_NE,
                    cmpResult,
                    zero,
                    name);
            return result;
        }
        default:
            throw std::invalid_argument("Unsupported string operation");
    }
}

llvm::Value *StrIRType::createUnaryOp(llvm::IRBuilder<> &builder,
                                      TokenType op,
                                      llvm::Value *operand,
                                      llvm::Value *storage,
                                      const std::string &name) const {
    throw std::invalid_argument("Unsupported unary operation");
}

llvm::Type *StrIRType::getLLVMType(llvm::LLVMContext &context) const {
    return llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
}

llvm::Type * StrIRType::getLLVMElementType(llvm::LLVMContext &context) const {
    throw std::runtime_error("Unsupported for str type");
}

llvm::Constant *StrIRType::createConstant(const BaseNode *node,
                                          llvm::IRBuilder<> & /*builder*/,
                                          llvm::Module &module) {
    const auto *strNode = dynamic_cast<const StringNode *>(node);
    auto *gv = new llvm::GlobalVariable(module,
                                        getLLVMType(module.getContext()),
                                        true,
                                        llvm::GlobalValue::PrivateLinkage,
                                        llvm::ConstantDataArray::getString(module.getContext(), strNode->str),
                                        ".str");
    return llvm::ConstantExpr::getBitCast(gv, getLLVMType(module.getContext()));
}

llvm::Value *StrIRType::createMethodCall(llvm::IRBuilder<> &builder,
                                         const MethodInfoPtr &methodInfo,
                                         llvm::Value *object,
                                         const std::vector<llvm::Value *> &args) const {
    if (methodInfo->name == "len") {
        return builder.CreateCall(getOrDeclareStrlen(builder.GetInsertBlock()->getModule()), {object});
    }
    return IRType::createMethodCall(builder, methodInfo, object, args);
}

llvm::Value *StrIRType::createLoad(llvm::IRBuilder<> & /*irBuilder*/, const IRValue &value) const {
    return value.getRawValue();
}

llvm::Function *StrIRType::getOrDeclareStrcmp(llvm::Module *module) const {
    auto &context = module->getContext();
    auto *const funcType = llvm::FunctionType::get(llvm::Type::getInt32Ty(context),
                                                   {getLLVMType(context), getLLVMType(context)},
                                                   false);
    return llvm::cast<llvm::Function>(module->getOrInsertFunction("strcmp", funcType).getCallee());
}

llvm::Function *StrIRType::getOrDeclareStrlen(llvm::Module *module) const {
    auto &context = module->getContext();
    auto *const funcType = llvm::FunctionType::get(llvm::Type::getInt64Ty(context),
                                                   {getLLVMType(context)},
                                                   false);
    return llvm::cast<llvm::Function>(module->getOrInsertFunction("strlen", funcType).getCallee());
}
