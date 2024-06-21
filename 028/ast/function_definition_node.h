#pragma once

#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing function definition nodes.
   */
  class function_definition_node : public cdk::expression_node {

    cdk::sequence_node *_arguments;
    cdk::sequence_node *_argument;
    cdk::sequence_node *_instructions;
    
  public:

    function_definition_node(int lineno, cdk::sequence_node *arguments, cdk::sequence_node *argument, cdk::sequence_node *instructions) :
        cdk::expression_node(lineno), _arguments(arguments), _argument(argument), _instructions(instructions) {
      type(cdk::primitive_type::create(0, cdk::TYPE_VOID));
    }

    function_definition_node(int lineno, std::shared_ptr<cdk::basic_type> Vartype, cdk::sequence_node *arguments, cdk::sequence_node *argument, cdk::sequence_node *instructions) :
        cdk::expression_node(lineno), _arguments(arguments), _argument(argument), _instructions(instructions) {
      type(Vartype);
    }

    cdk::sequence_node* argument() { return _argument; }

    cdk::sequence_node* arguments() { return _arguments; }

    cdk::sequence_node* instructions() { return _instructions; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_function_definition_node(this, level); }

  };

} // til