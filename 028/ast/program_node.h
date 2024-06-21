#pragma once

#include <cdk/ast/basic_node.h>

namespace til {

  /**
   * Class for describing program nodes.
   */
  class program_node : public cdk::basic_node {
    cdk::sequence_node *_declarations;
    cdk::sequence_node *_statements;

  public:
    program_node(int lineno, cdk::sequence_node *declarations, cdk::sequence_node *statements) :
        cdk::basic_node(lineno), _declarations(declarations), _statements(statements) {
    }

    cdk::sequence_node *declarations() { return _declarations; }

    cdk::sequence_node *statements() { return _statements; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_program_node(this, level); }

  };

} // til
