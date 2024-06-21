#pragma once

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing alloc nodes.
   */
  class alloc_node : public cdk::expression_node {

    cdk::expression_node *_expression;

  public:
    alloc_node(int lineno, cdk::expression_node *expression) :
        cdk::expression_node(lineno), _expression(expression) {
    }

    cdk::expression_node* expression() { return _expression; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_alloc_node(this, level); }

  };

} // til