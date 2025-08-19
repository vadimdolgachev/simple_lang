//
// Created by vadim on 27.03.25.
//

#ifndef MEMBERACCESSNODE_H
#define MEMBERACCESSNODE_H

#include "BaseNode.h"
#include "type/StructType.h"

class MemberAccessNode : public ExpressionNode {
public:
    explicit MemberAccessNode(ExprNodePtr object);

    [[nodiscard]] StructTypePtr getObjectType() const;

    ExprNodePtr object;
};

#endif //MEMBERACCESSNODE_H
