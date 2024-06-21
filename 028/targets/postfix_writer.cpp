#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include "targets/frame_size_calculator.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated

//---------------------------------------------------------------------------

void til::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}
void til::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}
void til::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
    if(_inFunction) {
    _pf.DOUBLE(node->value());
  } else {
    _pf.SDOUBLE(node->value());
  }
}
void til::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->argument()->accept(this, lvl + 2);
  _pf.INT(0);
  _pf.EQ();
}
void til::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  int lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JZ(mklbl(lbl = ++_lbl));
  node->right()->accept(this, lvl + 2);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}
void til::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  int lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JNZ(mklbl(lbl = ++_lbl));
  node->right()->accept(this, lvl + 2);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  if(_inFunction) {
    _pf.INT(node->value());
  } else {
    _pf.SINT(node->value());
  }
}

void til::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
 int lbl1;

  /* generate the string */
  _pf.RODATA(); // strings are DATA readonly
  _pf.ALIGN(); // make sure we are aligned
  _pf.LABEL(mklbl(lbl1 = ++_lbl)); // give the string a name
  std::string str = node->value();
  std::string c_str;
  for(size_t i = 0; i < str.size(); ++i){
    if(str[i] == '\\' && i+1 < str.size()) {
      if (isdigit(str[i + 1])){
        size_t j = i+1;
        while(j < str.size() && isdigit(str[j]) && j < i + 4){
          j++;
        }
        int value = std::stoi(str.substr(i + 1,j - (i + 1)), nullptr, 8);
        c_str.push_back(static_cast<char>(value));
        i = j - 1;
      } else if( str[i + 1] == 't'){
        c_str.push_back('\t');
        i++;
      } else if( str[i + 1] == 'n'){
        c_str.push_back('\n');
        i++;
      } else {
        c_str.push_back(str[i]);
      }
    } else{
      c_str.push_back(str[i]);
    }
  }
  _pf.SSTRING(c_str); // output string characters
  if(_stringInitialization){
    _pf.DATA();
    _pf.SADDR(mklbl(lbl1));
    _stringInitialization = false;
  }

  /* leave the address on the stack */
  _pf.TEXT(); // return to the TEXT segment
  _pf.ADDR(mklbl(lbl1)); // the string to be printed
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_unary_minus_node(cdk::unary_minus_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  _pf.NEG(); // 2-complement
}

void til::postfix_writer::do_unary_plus_node(cdk::unary_plus_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.ADD();
}
void til::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.SUB();
}
void til::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MUL();
}
void til::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.DIV();
}
void til::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}
void til::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.LT();
}
void til::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.LE();
}
void til::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.GE();
}
void til::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.GT();
}
void til::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.NE();
}
void til::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.EQ();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  const std::string &id = node->name();
  auto symbol = _symtab.find(id);
  if (symbol->offset() == 0) {
    _pf.ADDR(symbol->name());
  } else {
    _pf.LOCAL(symbol->offset());
  }
}

void til::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
  if(node->lvalue()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDDOUBLE();
  } else {
    _pf.LDINT();
  }
}

void til::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->rvalue()->accept(this, lvl);
  if(node->rvalue()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }
  node->lvalue()->accept(this, lvl);
  if (node->lvalue()->is_typed(cdk::TYPE_INT)) {
    _pf.STINT();
  } else if (node->lvalue()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.STDOUBLE();
  } else if (node->lvalue()->is_typed(cdk::TYPE_STRING)) {
    _pf.STINT();
  } else if(node->lvalue()->is_typed(cdk::TYPE_POINTER)) {
    _pf.STINT();
  } else if (node->lvalue()->is_typed(cdk::TYPE_VOID)) {
    _pf.STINT();
  } else {
    std::cerr << "ERROR: cannot happen" << std::endl;
    exit(1);
  }

}

//---------------------------------------------------------------------------

void til::postfix_writer::do_program_node(til::program_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  // The ProgramNode (representing the whole program) doubles as a
  // main function node.

  _functionLabels.push("_main");

  // generate the main function (RTS mandates that its name be "_main")
  _pf.TEXT();
  _pf.ALIGN();
  _pf.GLOBAL("_main", _pf.FUNC());
  _pf.LABEL("_main");

  _inFunction = true;
  _symtab.push();

  frame_size_calculator fsc(_compiler, _symtab);
  node->accept(&fsc, lvl);
  _pf.ENTER(fsc.localsize());

  _currentFunctionLoopLabels = new std::vector<std::pair<std::string, std::string>>();
  _currentFunctionRetLabel = mklbl(++_lbl);

  if(node->declarations()) {
    node->declarations()->accept(this, lvl);
  }
  if(node->statements()) {
    node->statements()->accept(this, lvl);
  }

  // end the main function
  _pf.INT(0);
  _pf.STFVAL32();

  _pf.ALIGN();
  _pf.LABEL(_currentFunctionRetLabel);
  _pf.LEAVE();
  _pf.RET();

  delete _currentFunctionLoopLabels;
  _inFunction = false;
  _symtab.pop();
  _functionLabels.pop();

  // these are just a few library function imports
  _pf.EXTERN("readi");
  _pf.EXTERN("printi");
  _pf.EXTERN("prints");
  _pf.EXTERN("printd");
  _pf.EXTERN("println");

}

//---------------------------------------------------------------------------

void til::postfix_writer::do_evaluation_node(til::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  if (node->argument()->is_typed(cdk::TYPE_INT)) {
    _pf.TRASH(4); // delete the evaluated value
  } else if (node->argument()->is_typed(cdk::TYPE_STRING)) {
    _pf.TRASH(4); // delete the evaluated value's address
  } else if(node->argument()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.TRASH(8); // delete the evaluated value
  } else if(node->argument()->is_typed(cdk::TYPE_POINTER)) {
    _pf.TRASH(4); // delete the evaluated value
  } else if (node->argument()->is_typed(cdk::TYPE_VOID)){
    _pf.TRASH(4); // delete the evaluated value
  }
  else {
    std::cerr << "ERROR: CANNOT HAPPEN!" << std::endl;
    exit(1);
  }
}

void til::postfix_writer::do_print_node(til::print_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  for (size_t i = 0; i < node->argument()->size(); i++) {
    cdk::expression_node* arg = dynamic_cast<cdk::expression_node*>(node->argument()->node(i));

    arg->accept(this, lvl);
    if (arg->is_typed(cdk::TYPE_INT)) {
      _pf.CALL("printi");
      _pf.TRASH(4);
    } else if (arg->is_typed(cdk::TYPE_STRING)) {
      _pf.CALL("prints");
      _pf.TRASH(4);
    } else if (arg->is_typed(cdk::TYPE_DOUBLE)) {
      _pf.CALL("printd");
      _pf.TRASH(8);
    } else {
      std::cerr << "ERROR: CANNOT HAPPEN!" << std::endl;
      exit(1);
    }

  }
  if(node->ln()) {
    _pf.CALL("println");
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_read_node(til::read_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _pf.CALL("readi");
  _pf.LDFVAL32();
  node->accept(this, lvl);
  _pf.STINT();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_while_node(til::while_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int condLabel, endLabel;

  _pf.ALIGN();
  _pf.LABEL(mklbl(condLabel = ++_lbl));
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(endLabel = ++_lbl));

  _currentFunctionLoopLabels->push_back(std::make_pair(mklbl(condLabel), mklbl(endLabel)));
  node->block()->accept(this, lvl + 2);
  _visitedFinalInstruction = false;
  _currentFunctionLoopLabels->pop_back();

  _pf.JMP(mklbl(condLabel));
  _pf.ALIGN();
  _pf.LABEL(mklbl(endLabel));
}

void til::postfix_writer::do_stop_node(til::stop_node * const node, int lvl) {
ASSERT_SAFE_EXPRESSIONS;

  auto level = static_cast<size_t>(node->level());

  if (level == 0) {
    throw std::string("invalid stop instruction level");
  } else if (_currentFunctionLoopLabels->size() < level) {
    throw std::string("stop not within sufficient loops (expected at most " +
         std::to_string(_currentFunctionLoopLabels->size()) + ")");
  }

  auto index = _currentFunctionLoopLabels->size() - level;
  auto label = std::get<1>(_currentFunctionLoopLabels->at(index));
  _pf.JMP(label);

  _visitedFinalInstruction = true;
}

void til::postfix_writer::do_next_node(til::next_node * const node, int lvl) {
  if (_whileIni.size() != 0) {
    _pf.JMP(mklbl(_whileIni[_whileIni.size() - node->level()]));
  } else {
    std::cerr << "'next' outside 'while'" << std::endl;
  }
} 

//---------------------------------------------------------------------------

void til::postfix_writer::do_if_node(til::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _visitedFinalInstruction = false;
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_if_else_node(til::if_else_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _visitedFinalInstruction = false;
  _pf.JMP(mklbl(lbl2 = ++_lbl));
  _pf.LABEL(mklbl(lbl1));
  node->elseblock()->accept(this, lvl + 2);
  _visitedFinalInstruction = false;
  _pf.LABEL(mklbl(lbl1 = lbl2));
}


//---------------------------------------------------------------------------

void til::postfix_writer::do_return_node(til::return_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  auto symbol = _symtab.find("@", 1);
  auto rettype = cdk::functional_type::cast(symbol->type())->output(0);
  auto rettype_name = rettype->name();

  if (rettype_name != cdk::TYPE_VOID) {
    node->returnValue()->accept(this, lvl + 2);

    if (rettype_name == cdk::TYPE_DOUBLE) {
      _pf.STFVAL64();
    } else {
      _pf.STFVAL32();
    }
  }

  _pf.JMP(_currentFunctionRetLabel);

  _visitedFinalInstruction = true;
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_alloc_node(til::alloc_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->expression()->accept(this, lvl);
  _pf.INT(3);
  _pf.SHTL();
  _pf.ALLOC();
  _pf.SP(); 
  }

void til::postfix_writer::do_size_of_node(til::size_of_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _pf.INT(node->expression()->type()->size());
}

void til::postfix_writer::do_null_node(til::null_node * const node, int lvl) {
  if (_inFunction) {
    _pf.INT(0);
  } else {
    _pf.SINT(0);
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_call_node(til::call_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  std::shared_ptr<cdk::functional_type> func_type;
  if (node->name() == nullptr) { // recursive call; "@"
    auto symbol = _symtab.find("@", 1);
    func_type = cdk::functional_type::cast(symbol->type());
  } else {
    func_type = cdk::functional_type::cast(node->name()->type());
  }

  int args_size = 0;
  // arguments must be visited in reverse order since the first argument has to be
  // on top of the stack
  for (size_t i = node->arguments()->size(); i > 0; --i) {
    auto arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(i - 1));

    args_size += arg->type()->size();
    node->arguments()->node(i - 1)->accept(this, lvl);
  }

  _externalFunctionName = std::nullopt;
  if (node->name() == nullptr) { // recursive call; "@"
    _pf.ADDR(_functionLabels.top());
  } else {
    node->name()->accept(this, lvl);
  }

  if (_externalFunctionName) {
    _pf.CALL(*_externalFunctionName);
    _externalFunctionName = std::nullopt;
  } else {
    _pf.BRANCH();
  }

  if (args_size > 0) {
    _pf.TRASH(args_size);
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDFVAL64();
  } else if (!node->is_typed(cdk::TYPE_VOID)) {
    _pf.LDFVAL32();
  }
}

void til::postfix_writer::do_function_definition_node(til::function_definition_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  std::string functionLabel;
  
  functionLabel = mklbl(++_lbl);
  _functionLabels.push(functionLabel);

  _pf.TEXT(_functionLabels.top());
  _pf.ALIGN();

  _pf.LABEL(_functionLabels.top());

  auto oldOffset = _offset;
  _offset = 8; // function arguments start at offset 8
  _symtab.push();

  _inFunctionArgs = true;
  node->arguments()->accept(this, lvl);
  _inFunctionArgs = false;

  // compute stack size to be reserved for local variables
  frame_size_calculator fsc(_compiler, _symtab);
  node->arguments()->accept(&fsc, lvl);
  node->instructions()->accept(&fsc, lvl);
  _pf.ENTER(fsc.localsize());

  auto oldFunctionRetLabel = _currentFunctionRetLabel;
  _currentFunctionRetLabel = mklbl(++_lbl);

  auto oldFunctionLoopLabels = _currentFunctionLoopLabels;
  _currentFunctionLoopLabels = new std::vector<std::pair<std::string, std::string>>();

  _offset = 0; // local variables start at offset 0

  node->arguments()->accept(this, lvl);
  node->instructions()->accept(this, lvl);

  _pf.ALIGN();
  _pf.LABEL(_currentFunctionRetLabel);
  _pf.LEAVE();
  _pf.RET();

  delete _currentFunctionLoopLabels;
  _currentFunctionLoopLabels = oldFunctionLoopLabels;
  _currentFunctionRetLabel = oldFunctionRetLabel;
  _offset = oldOffset;
  _symtab.pop();
  _functionLabels.pop();

  // Since a function is also an expression, we need to push its address to the stack.
  // We should return to a text segment if this function is a local variable of another function
  // or to the data segment if it is global variable (except for the main function, which is not an expression).
  if (_inFunction) {
    _pf.TEXT(_functionLabels.top());
    _pf.ADDR(functionLabel);
  } else {
    _pf.DATA();
    _pf.SADDR(functionLabel);
  }
}

void til::postfix_writer::do_block_node(til::block_node * const node, int lvl) {
  _symtab.push();
  if (node->declarations()) node->declarations()->accept(this, lvl);
  _visitedFinalInstruction = false;
  if (node->instructions()) node->instructions()->accept(this, lvl);
  _visitedFinalInstruction = false;
  _symtab.pop();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_index_node(til::index_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if(node->base()) {
    node->base()->accept(this, lvl);
  }
  if(node->index()) {
    node->index()->accept(this, lvl);
  }
  _pf.INT(3);
  _pf.SHTL();
  _pf.ADD(); 
}

void til::postfix_writer::do_variable_declaration_node(til::variable_declaration_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int offset = 0;
  int typesize = node->type()->size();
  if (_inFunction) {
    _offset -= typesize;
    offset = _offset;
  } else if (_inFunctionArgs) {
    _offset += typesize;
    offset = _offset;
  } else {
    offset = 0;
  }

  auto symbol = new_symbol();
  if (symbol) {
    symbol->set_offset(offset);
    reset_new_symbol();
  }

  if (_inFunction) {
    if(node->initializer() != nullptr) {
      node->initializer()->accept(this, lvl);
      if(node->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.DUP64();
      } else {
        _pf.DUP32();
      }
      _pf.LOCAL(symbol->offset());
      if(node->is_typed(cdk::TYPE_INT)) {
        _pf.STINT();
      } else if(node->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.STDOUBLE();
      } else if(node->is_typed(cdk::TYPE_STRING)) {
        _pf.STINT();
      }
    }
  } else {
    if (node->initializer() !=nullptr){
      if(node->initializer()->is_typed(cdk::TYPE_STRING)){
        _pf.DATA();
        _pf.ALIGN();
        _pf.GLOBAL(node->identifier(), _pf.OBJ());
        _pf.LABEL(node->identifier());
        _stringInitialization = true;
        node->initializer()->accept(this, lvl);
      }
      else{
        _pf.DATA();
        _pf.ALIGN();
        _pf.GLOBAL(node->identifier(), _pf.OBJ());
        _pf.LABEL(node->identifier());
        node->initializer()->accept(this, lvl);
      }
    }else{
      _pf.BSS();
      _pf.ALIGN();
      _pf.GLOBAL(node->identifier(), _pf.OBJ());
      _pf.LABEL(node->identifier());
      _pf.SALLOC(node->type()->size());
      }
  }
}

void til::postfix_writer::do_address_node(til::address_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS
  node->argument()->accept(this, lvl);
}