#pragma once

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing size_of nodes.
   */
  class size_of_node : public cdk::expression_node {

    cdk::expression_node *_expression;

  public:
    size_of_node(int lineno, cdk::expression_node *expression = nullptr) :
        cdk::expression_node(lineno), _expression(expression) {
    }

    cdk::expression_node* expression() { return _expression; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_size_of_node(this, level); }

  };

} // til