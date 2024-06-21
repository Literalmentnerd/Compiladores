#pragma once

#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing function call nodes.
   */
  class call_node : public cdk::expression_node {
    
    cdk::expression_node *_name;
    cdk::sequence_node *_arguments;

  public:

    /*Function call without arguments*/
    call_node(int lineno, cdk::expression_node *name) :
        cdk::expression_node(lineno), _name(name), _arguments(new cdk::sequence_node(lineno)) {
    }

    /*Function call with arguments*/
    call_node(int lineno, cdk::expression_node *name, cdk::sequence_node *arguments) :
        cdk::expression_node(lineno), _name(name), _arguments(arguments) {
    }

    cdk::expression_node *name() { return _name; }
    
    cdk::sequence_node *arguments() { return _arguments; }

    cdk::expression_node *argument(size_t ix) { return dynamic_cast<cdk::expression_node*>(_arguments->node(ix)); }

    void accept(basic_ast_visitor *sp, int level) { sp->do_call_node(this, level); }

  };

} // til