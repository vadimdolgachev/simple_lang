#include "CallFunctionNode.h"
#include "BinOpNode.h"

#include <sstream>

std::string CallFunctionNode::toString() const {
    std::stringstream ss;
    ss << "call func: " << callee << "(";
    for (const auto &arg: args) {
        const bool isBinOp = dynamic_cast<BinOpNode *>(arg.get()) != nullptr;
        if (isBinOp) {
            ss << "(";
        }
        ss << arg->toString() << ",";
        if (isBinOp) {
            ss << ")";
        }
    }
    ss << ")";
    return ss.str();
}

void CallFunctionNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}
