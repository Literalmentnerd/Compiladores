#pragma once

#include <cdk/ast/lvalue_node.h>

namespace til {

  /**
   * Class for describing block nodes.
   */
  class address_node : public cdk::expression_node {

    cdk::lvalue_node *_argument;

  public:
    address_node(int lineno, cdk::lvalue_node *argument) :
        cdk::expression_node(lineno), _argument(argument) {
    }

    cdk::lvalue_node* argument() { return _argument; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_address_node(this, level); }

  };

} // til