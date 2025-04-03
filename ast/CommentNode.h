//
// Created by vadim on 29.03.25.
//

#ifndef COMMENTNODE_H
#define COMMENTNODE_H

#include "BaseNode.h"

class CommentNode final : public BaseNode {
public:
    explicit CommentNode(std::string text);

    void visit(NodeVisitor *visitor) const override;
    [[nodiscard]] std::string toString() const override;

    const std::string text;
};



#endif //COMMENTNODE_H
