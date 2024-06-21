#pragma once

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing function-return nodes.
   */
  class return_node : public cdk::basic_node {

    cdk::expression_node *_returnValue;

  public:
    return_node(int lineno, cdk::expression_node *returnValue = nullptr) :
        cdk::basic_node(lineno), _returnValue(returnValue) {
    }

    cdk::expression_node *returnValue() { return _returnValue; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_return_node(this, level); }

  };

} // til