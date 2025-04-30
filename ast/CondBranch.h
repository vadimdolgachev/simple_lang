//
// Created by vadim on 19.03.25.
//

#ifndef CONDBRANCH_H
#define CONDBRANCH_H

#include "BlockNode.h"

struct CondBranch {
    ExprNodePtr cond;
    std::unique_ptr<BlockNode> then;
};

#endif //CONDBRANCH_H
