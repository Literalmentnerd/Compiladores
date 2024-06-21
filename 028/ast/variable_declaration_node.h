#pragma once

#include <cdk/ast/typed_node.h>
#include <cdk/ast/expression_node.h>

namespace til {

  class variable_declaration_node: public cdk::typed_node {

    int _qualifier;
    std::string _identifier;
    cdk::expression_node *_initializer;

  public:
    variable_declaration_node(int lineno, int qualifier, const std::string &identifier, cdk::expression_node *initializer,
                              std::shared_ptr<cdk::basic_type> varType) :
        cdk::typed_node(lineno), _qualifier(qualifier), _identifier(identifier), _initializer(initializer) {
      type(varType);
    }

    bool constant() { return false; }

    int qualifier() { return _qualifier; }

    const std::string &identifier() { return _identifier; }
    
    cdk::expression_node* initializer() { return _initializer; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_variable_declaration_node(this, level); }

  };

} // til