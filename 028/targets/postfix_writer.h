#pragma once

#include "targets/basic_ast_visitor.h"

#include <sstream>
#include <stack>
#include <cdk/emitters/basic_postfix_emitter.h>
#include <optional>
#include <set>

namespace til {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class postfix_writer: public basic_ast_visitor {
    cdk::symbol_table<til::symbol> &_symtab;
    cdk::basic_postfix_emitter &_pf;
    std::vector<int> _whileIni, _whileEnd;
    int _lbl;
    int _offset;
    bool _inFunction, _inFunctionArgs, _stringInitialization;
    std::optional<std::string> _externalFunctionName;
    std::set<std::string> _externalFunctionsToDeclare;
    std::string _currentFunctionRetLabel; //label to jump to when returning from a function
    std::vector<std::pair<std::string, std::string>> *_currentFunctionLoopLabels; //history of labels of loops in the current function
    std::stack<std::string> _functionLabels; //stack of function context labels
    bool _visitedFinalInstruction = false; //flag to check if the last instruction was visited

  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<til::symbol> &symtab,
                   cdk::basic_postfix_emitter &pf) :
        basic_ast_visitor(compiler), _symtab(symtab), _pf(pf), _lbl(0), _offset(0), _inFunction(false), _inFunctionArgs(false), _stringInitialization(false) {
    }

  public:
    ~postfix_writer() {
      os().flush();
    }

  private:
    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
      std::ostringstream oss;
      if (lbl < 0)
        oss << ".L" << -lbl;
      else
        oss << "_L" << lbl;
      return std::move(oss).str();
    }

  public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

  };

} // til
