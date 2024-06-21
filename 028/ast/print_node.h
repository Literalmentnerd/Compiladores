#pragma once

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing print nodes.
   */
  class print_node : public cdk::basic_node {
    cdk::sequence_node *_argument;
    bool _ln;

  public:
    print_node(int lineno, cdk::sequence_node *argument, bool ln = true) :
        cdk::basic_node(lineno), _argument(argument), _ln(ln) {
    }

    cdk::sequence_node *argument() { return _argument; }

    bool ln() { return _ln; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_print_node(this, level); }

  };

} // til
