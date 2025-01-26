#include "FunctionCallNode.h"
#include "BinOpNode.h"

#include <sstream>

std::string FunctionCallNode::toString() const {
    std::stringstream ss;
    ss << "call func: " << ident->name << "(";
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

void FunctionCallNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}
