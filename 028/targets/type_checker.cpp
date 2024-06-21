#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

//---------------------------------------------------------------------------

bool til::type_checker::deepTypeComparison(std::shared_ptr<cdk::basic_type> left,
      std::shared_ptr<cdk::basic_type> right, bool lax) {
  if (left->name() == cdk::TYPE_UNSPEC || right->name() == cdk::TYPE_UNSPEC) {
    return false;
  } else if (left->name() == cdk::TYPE_FUNCTIONAL) {
    if (right->name() != cdk::TYPE_FUNCTIONAL) {
      return false;
    }

    auto left_func = cdk::functional_type::cast(left);
    auto right_func = cdk::functional_type::cast(right);

    if (left_func->input_length() != right_func->input_length()
          || left_func->output_length() != right_func->output_length()) {
      return false;
    }

    for (size_t i = 0; i < left_func->input_length(); i++) {
      if (!deepTypeComparison(right_func->input(i), left_func->input(i), lax)) {
        return false;
      }
    }

    for (size_t i = 0; i < left_func->output_length(); i++) {
      if (!deepTypeComparison(left_func->output(i), right_func->output(i), lax)) {
        return false;
      }
    }

    return true;
  } else if (right->name() == cdk::TYPE_FUNCTIONAL) {
    return false;
  } else if (left->name() == cdk::TYPE_POINTER) {
    if (right->name() != cdk::TYPE_POINTER) {
      return false;
    }

    return deepTypeComparison(cdk::reference_type::cast(left)->referenced(),
        cdk::reference_type::cast(right)->referenced(), false);
  } else if (right->name() == cdk::TYPE_POINTER) {
      return false;
  } else if (lax && left->name() == cdk::TYPE_DOUBLE) {
    return right->name() == cdk::TYPE_DOUBLE || right->name() == cdk::TYPE_INT;
  } else {
    return left == right;
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++)
    node->node(i)->accept(this, lvl);
}

//---------------------------------------------------------------------------

void til::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}
void til::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (node->argument()->is_typed(cdk::TYPE_INT))
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  else if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    til::read_node *input = dynamic_cast<til::read_node*>(node->argument());

    if(input != nullptr) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
    else
      throw std::string("Unknown node with unspecified type.");
  }
  else
    throw std::string("Wrong type in argument of unary expression (Integer expected).");

}
void til::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  
  if(node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    til::read_node *inputl = dynamic_cast<til::read_node *>(node->left());

    if(inputl != nullptr)
      node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    else
      throw std::string("Unknown node with unspecified type.");
  }
  else if(!node->left()->is_typed(cdk::TYPE_INT))
    throw std::string("Integer expression expected in (left and right) binary operators.");

  node->right()->accept(this, lvl + 2);
  if(node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    til::read_node *inputr = dynamic_cast<til::read_node *>(node->right());

    if(inputr != nullptr)
       node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    else
      throw std::string("Unknown node with unspecified type.");
  }
  else if(!node->right()->is_typed(cdk::TYPE_INT))
    throw std::string("Integer expression expected in (left and right) binary operators.");

   node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}
void til::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
    ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  
  if(node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    til::read_node *inputl = dynamic_cast<til::read_node *>(node->left());

    if(inputl != nullptr)
      node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    else
      throw std::string("Unknown node with unspecified type.");
  }
  else if(!node->left()->is_typed(cdk::TYPE_INT))
    throw std::string("Integer expression expected in (left and right) binary operators.");

  node->right()->accept(this, lvl + 2);
  if(node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    til::read_node *inputr = dynamic_cast<til::read_node *>(node->right());

    if(inputr != nullptr)
       node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    else
      throw std::string("Unknown node with unspecified type.");
  }
  else if(!node->right()->is_typed(cdk::TYPE_INT))
    throw std::string("Integer expression expected in (left and right) binary operators.");

   node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

void til::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

//---------------------------------------------------------------------------

void til::type_checker::processUnaryExpression(cdk::unary_operation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in argument of unary expression");

  // in til, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_unary_minus_node(cdk::unary_minus_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

void til::type_checker::do_unary_plus_node(cdk::unary_plus_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void til::type_checker::processBinaryExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in left argument of binary expression");

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in right argument of binary expression");

  // in til, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void til::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<til::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->type());
  } else {
    throw id;
  }
}

void til::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

void til::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 4);
  node->rvalue()->accept(this, lvl + 4);

  if (node->lvalue()->is_typed(cdk::TYPE_INT)) {
    if (node->rvalue()->is_typed(cdk::TYPE_INT)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else {
      throw std::string("wrong assignment to integer");
    }
  } else if (node->lvalue()->is_typed(cdk::TYPE_POINTER)) {

    if (node->rvalue()->is_typed(cdk::TYPE_POINTER)) {
      node->type(node->rvalue()->type());
    } else if (node->rvalue()->is_typed(cdk::TYPE_INT)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_POINTER));
    } else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
    } else {
      throw std::string("wrong assignment to pointer");
    }

  } else if (node->lvalue()->is_typed(cdk::TYPE_DOUBLE)) {

    if (node->rvalue()->is_typed(cdk::TYPE_DOUBLE) || node->rvalue()->is_typed(cdk::TYPE_INT)) {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      node->rvalue()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } else {
      throw std::string("wrong assignment to real");
    }

  } else if (node->lvalue()->is_typed(cdk::TYPE_STRING)) {

    if (node->rvalue()->is_typed(cdk::TYPE_STRING)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
    } else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
    } else {
      throw std::string("wrong assignment to string");
    }

  } else if (node->lvalue()->is_typed(cdk::TYPE_VOID)) {

    if (node->rvalue()->is_typed(cdk::TYPE_VOID)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_VOID));
    } else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_VOID));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_VOID));
    } else {
      throw std::string("wrong assignment to void");
    }
  }
  else {
    throw std::string("wrong types in assignment");
  }


}

//---------------------------------------------------------------------------

void til::type_checker::do_program_node(til::program_node *const node, int lvl) {
  auto type = cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_INT));
  auto function = std::make_shared<til::symbol>(type, "@");

  if (!_symtab.insert(function->name(), function)) {
    _symtab.replace(function->name(), function);
  }
}

void til::type_checker::do_evaluation_node(til::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void til::type_checker::do_print_node(til::print_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

void til::type_checker::do_read_node(til::read_node *const node, int lvl) {
  try {
    node->accept(this, lvl);
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_while_node(til::while_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

void til::type_checker::do_stop_node(til::stop_node *const node, int lvl) {
  //! EMPTY
}

void til::type_checker::do_next_node(til::next_node *const node, int lvl) {
  //! EMPTY
}

//---------------------------------------------------------------------------

void til::type_checker::do_if_node(til::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (node->condition()->is_typed(cdk::TYPE_UNSPEC))
  {
    til::read_node *input = dynamic_cast<til::read_node *>(node->condition());

    if(input != nullptr) {
      node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
    else {
      throw std::string("Unknown node with unspecified type.");
    }

  }
  else if (!node->condition()->is_typed(cdk::TYPE_INT)) 
    throw std::string("Expected integer condition.");
}

void til::type_checker::do_if_else_node(til::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (node->condition()->is_typed(cdk::TYPE_UNSPEC))
  {
    til::read_node *input = dynamic_cast<til::read_node *>(node->condition());

    if(input != nullptr) {
      node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
    else {
      throw std::string("Unknown node with unspecified type.");
    }

  }
  else if (!node->condition()->is_typed(cdk::TYPE_INT)) 
    throw std::string("Expected integer condition.");
}

//---------------------------------------------------------------------------

void til::type_checker::do_return_node(til::return_node *const node, int lvl) {
  auto symbol = _symtab.find("@", 1);
  std::shared_ptr<cdk::functional_type> functype;

  if(symbol != nullptr) {
    functype = cdk::functional_type::cast(symbol->type());
  } else {
    throw std::string("return not in function");
  }

  auto rettype = functype->output(0);
  auto rettype_name = rettype->name();

  if (node->returnValue() == nullptr) {
    if (rettype_name != cdk::TYPE_VOID) {
      throw std::string("no return value specified for non-void function");
    }
    return;
  }

  // return has expression

  if (rettype_name == cdk::TYPE_VOID) {
    throw std::string("return value specified for void function");
  }

  node->returnValue()->accept(this, lvl + 2);

  if (!deepTypeComparison(rettype, node->returnValue()->type(), true)) {
    throw std::string("wrong type for return expression");
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_alloc_node(til::alloc_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->expression()->accept(this, lvl + 2);
  if (!node->expression()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type for expression in alloc");
  }
  auto type = cdk::reference_type::create(4, cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  node->type(type);
}

void til::type_checker::do_size_of_node(til::size_of_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->expression()->accept(this, lvl + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_null_node(til::null_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC)));
}

//---------------------------------------------------------------------------

void til::type_checker::do_call_node(til::call_node * const node, int lvl) {
  ASSERT_UNSPEC;

  std::shared_ptr<cdk::functional_type> functype;

  if (node->name() == nullptr) { // recursive call; "@"
    auto symbol = _symtab.find("@", 1);
    if (symbol == nullptr) {
      throw std::string("recursive call outside function");
    }

    functype = cdk::functional_type::cast(symbol->type());
  } else {
    node->name()->accept(this, lvl);

    if (!node->name()->is_typed(cdk::TYPE_FUNCTIONAL)) {
      throw std::string("wrong type in function call");
    }

    functype = cdk::functional_type::cast(node->name()->type());
  }

  if (functype->input()->length() != node->arguments()->size()) {
    throw std::string("wrong number of arguments in function call");
  }

  for (size_t i = 0; i < node->arguments()->size(); i++) {
    auto arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(i));
    arg->accept(this, lvl);

    auto paramtype = functype->input(i);

    if (arg->is_typed(cdk::TYPE_UNSPEC)) {
      if (paramtype->name() == cdk::TYPE_DOUBLE) {
        arg->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      } else {
        arg->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      }
    } else if (arg->is_typed(cdk::TYPE_POINTER) && paramtype->name() == cdk::TYPE_POINTER) {
      auto paramref = cdk::reference_type::cast(paramtype);
      auto argref = cdk::reference_type::cast(arg->type());

      if (argref->referenced()->name() == cdk::TYPE_UNSPEC
            || argref->referenced()->name() == cdk::TYPE_VOID
            || paramref->referenced()->name() == cdk::TYPE_VOID) {
        arg->type(paramtype);
      }
    }

    if (!deepTypeComparison(paramtype, arg->type(), true)) {
      throw std::string("wrong type for argument " + std::to_string(i + 1) + " in function call");
    }
  }

  // note this may result in this node being typed TYPE_VOID
  node->type(functype->output(0));
}

void til::type_checker::do_function_definition_node(til::function_definition_node * const node, int lvl) {
  auto function = std::make_shared<til::symbol>(node->type(), "@");

  if (!_symtab.insert(function->name(), function)) {
    _symtab.replace(function->name(), function);
  }
}

void til::type_checker::do_block_node(til::block_node * const node, int lvl) {
  if (node->declarations())
    node->declarations()->accept(this, lvl + 2);
  if (node->instructions())
    node->instructions()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

void til::type_checker::do_index_node(til::index_node * const node, int lvl) {
  ASSERT_UNSPEC;
  std::shared_ptr <cdk::reference_type> ref; 

  if(node->base()) {
    node->base()->accept(this, lvl + 2);
    ref = cdk::reference_type::cast(node->base()->type());
    if (!node->base()->is_typed(cdk::TYPE_POINTER)) {
      throw std::string("wrong type in pointer index's base");
    }
  }

  if(node->index()) {
    node->index()->accept(this, lvl + 2);
    if (!node->index()->is_typed(cdk::TYPE_INT)) {
      throw std::string("wrong type in pointer index's index");
    }
  }
  
  node->type(ref->referenced());
}

void til::type_checker::do_variable_declaration_node(til::variable_declaration_node * const node, int lvl) {
  if (node->initializer() != nullptr) {
    node->initializer()->accept(this, lvl + 2);

    if (node->type() == nullptr) {
      node->type(cdk::primitive_type::create(node->initializer()->type()->size(), node->initializer()->type()->name()));
    }
    if (node->is_typed(cdk::TYPE_INT)) {
      if (!node->initializer()->is_typed(cdk::TYPE_INT)) {
        throw std::string("wrong type for initializer (integer expected).");
      }
    } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
      if (!node->initializer()->is_typed(cdk::TYPE_INT) && !node->initializer()->is_typed(cdk::TYPE_DOUBLE)) {
        throw std::string("wrong type for initializer (integer or double expected).");
      }
    } else if (node->is_typed(cdk::TYPE_STRING)) {
      if (!node->initializer()->is_typed(cdk::TYPE_STRING)) {
        throw std::string("wrong type for initializer (string expected).");
      }
    } else if (node->is_typed(cdk::TYPE_POINTER)) {
      if (!node->initializer()->is_typed(cdk::TYPE_POINTER)) {
        throw std::string("wrong type for initializer (pointer expected).");
      }
    } else {
      throw std::string("unknown type for initializer.");
    }
  }

  const std::string &id = node->identifier();
  auto symbol= std::make_shared<til::symbol>(node->type(), id);
  if (_symtab.insert(id, symbol)) {
    _parent->set_new_symbol(symbol);
  } else {
    throw std::string("variable '" + id + "' redeclared");
  }
}

void til::type_checker::do_address_node(til::address_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  node->type(cdk::reference_type::create(4, node->argument()->type()));
}