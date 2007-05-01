#include <CxxToken.hxx>
#include <memory.h>

//CxxToken::CxxToken()
//:
//	_value(0)
//{}

int indentation = 0;

static void
emit_subtoken (CxxToken * subtoken)
{
  std::cout << std::flush;
  if (subtoken != 0) {
      std::cout << " ";
      subtoken->emit();
      }
  else std::cout << " #F";
  std::cout << std::flush;
}

void
CxxAbstractFunctionExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aParenthesis);
  std::cout << ")";
}

void
CxxAccessBaseSpecifier::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (baseSpecifier);
  emit_subtoken (accessSpecifier);
  std::cout << ")";
}

void
CxxAccessibilitySpecifier::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (accessSpecifier);
  std::cout << ")";
}

void
CxxArrowExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (anExpr);
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxAsmDefinition::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aString);
  std::cout << ")";
}

void
CxxAssignmentExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  std::cout << " " << assOp->token_type;
  emit_subtoken (leftExpr);
  emit_subtoken (rightExpr);
  std::cout << ")";
}

void
CxxAtomicStatement::emit()
{
  std::cout << "(" << token_type << ")" << std::flush;
}

void
CxxBaseSpecifier::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxBinaryExpression::emit()
{
  std::cout << "(" << operator_name;
  emit_subtoken (left_operand);
  emit_subtoken (right_operand);
  std::cout << ")";
}

void
CxxBuiltInId::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxBuiltInName::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  emit_subtoken (anElement);
  std::cout << ")";
}

void
CxxCallExpression::emit ()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (anExpr);
  emit_subtoken (aParenthesis);
  std::cout << ")";
}

void
CxxCaseStatement::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (anExpr);
  emit_subtoken (aStmt);
  std::cout << ")";
}

void
CxxCharacterLiteralExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aLiteral);
  std::cout << ")";
}

void
CxxClassMemberList::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aClass);
  emit_subtoken (DeclarationList);
  std::cout << ")";
}

void
CxxClassSpecifierId::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (classKey);
  emit_subtoken (aName);
  emit_subtoken (baseSpecifiers);
  std::cout << ")";
}

void
CxxClassTypeParameter::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxCompoundExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aList);
  std::cout << ")";
}

void
CxxCompoundStatement::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (statementList);
  std::cout << ")";
}

void
CxxCondition::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aList);
  std::cout << ")";
}

void
CxxConditionalExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (testExpr);
  emit_subtoken (trueExpr);
  emit_subtoken (falseExpr);
  std::cout << ")";
}

void
CxxConversionFunctionId::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (typeId);
  std::cout << ")";
}

void
CxxCtorDefinitionExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (anExpr);
  emit_subtoken (functionBody);
  std::cout << ")";
}

void
CxxCtorFunctionBlock::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (functionBody);
  emit_subtoken (ctorList);
  std::cout << ")";
}

void
CxxCvDeclarator::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aDeclarator);
  emit_subtoken (cvQualifierList);
  std::cout << ")";
}

void
CxxCvDeclSpecifier::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (cvQualifierList);
  std::cout << ")";
}

void
CxxCvQualifierList::emit()
{
  if ((aList == 0) && (anElement == 0))
      std::cout << "()";
  else {
      std::cout << "(" << token_type << std::flush;
      emit_subtoken (aList);
      emit_subtoken (anElement);
      std::cout << ")";
      }
}

void
CxxDeclarationList::emit()
{
  std::cout << std::endl << std::flush;
  for (int i = 0; i < indentation; i++) std::cout << " ";
  std::cout << "(" << token_type << std::flush;
  indentation += 4;
  if (aList) {
      aList->emit1();
      }
  if (anElement) {
      std::cout << " ";
      anElement->emit();
      }
  else std::cout << " NULL";
  std::cout << ")";
  indentation -= 4;
}

void
CxxDeclarationStatement::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aDecl);
  std::cout << ")";
}

void
CxxDeclSpecifierDeclaration::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aDeclaration);
  emit_subtoken (declSpecifier);
  std::cout << ")";
}

void
CxxDeclSpecifierExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (anExpr);
  emit_subtoken (declSpecifier);
  std::cout << ")";
}

void
CxxDeclSpecifierName::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  emit_subtoken (declSpecifier);
  std::cout << ")";
}

void
CxxDeclSpecifierParameter::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  emit_subtoken (declSpecifier);
  std::cout << ")";
}

void
CxxDefaultStatement::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aStmt);
  std::cout << ")";
}

void
CxxDotExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (anExpr);
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxDoWhileStatement::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aStmt);
  emit_subtoken (testExpr);
  std::cout << ")";
}

void
CxxElaboratedTypeSpecifier::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (classKey);
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxEllipsesExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  std::cout << ")";
}

void
CxxEnumerator::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  emit_subtoken (anExpr);
  std::cout << ")";
}

void
CxxEnumSpecifierId::emit()
{
  std::cout << "(enum";
  emit_subtoken (aName);
  emit_subtoken (aList);
  std::cout << ")";
}

void
CxxExceptionDeclaration::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aParameter);
  std::cout << ")";
}

void
CxxExceptionSpecification::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (typeIds);
  std::cout << ")";
}

void
CxxExplicitSpecialization::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aDeclaration);
  std::cout << ")";
}

void
CxxExpressionList::emit1()
{
  if ((aList == 0) && (anElement == 0)) return;
  if (aList != 0) aList->emit1();
  emit_subtoken (anElement);
}

void
CxxExpressionList::emit()
{
  std::cout << "(" << token_type << std::flush;
  if (aList) {
      aList->emit1();
      }
  if (anElement) {
      emit_subtoken (anElement);
      }
  std::cout << ")";
}

void
CxxExpressionParameter::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (anExpr);
  std::cout << ")";
}

void
CxxFalseExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  std::cout << ")";
}

void
CxxFloatingLiteralExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aLiteral);
  std::cout << ")";
}

void
CxxForStatement::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (initExpr);
  emit_subtoken (testExpr);
  emit_subtoken (stepExpr);
  emit_subtoken (aStmt);
  std::cout << ")";
}

void
CxxFunctionBlock::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aStatement);
  std::cout << ")";
}

void
CxxFunctionDefinitionExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (anExpr);
  emit_subtoken (functionBody);
  std::cout << ")";
}

void
CxxGotoStatement::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aLabel);
  std::cout << ")";
}

void
CxxHandler::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (exceptionDeclaration);
  emit_subtoken (aStatement);
  std::cout << ")";
}


void
CxxIfStatement::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (testExpr);
  emit_subtoken (trueStmt);
  emit_subtoken (falseStmt);
  std::cout << ")";
}

void
CxxInitializerClauseList::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aList);
  emit_subtoken (anElement);
  std::cout << ")";
}

void
CxxInitializerExpressionClause::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (anExpr);
  std::cout << ")";
}

void
CxxInitializerListClause::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aList);
  std::cout << ")";
}

void
CxxInitSimpleTypeParameter::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxInitTemplatedParameter::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (typeParameter);
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxIntegerLiteralExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aLiteral);
  std::cout << ")";
}

void
CxxLabelStatement::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aLabel);
  emit_subtoken (aStmt);
  std::cout << ")";
}

void
CxxLine::emit()
{
//  std::cout << "(" << token_type << std::flush;
//  std::cout << ")";
}

void
CxxLinedDeclaration::emit()
{
  std::cout << std::endl << std::flush;
  for (int i = 0; i < indentation; i++) std::cout << " ";
  indentation += 4;
  //  std::cout << "(" << token_type << std::flush;
  emit_subtoken (declaration);
  // emit_subtoken (line);
  // std::cout << ")";
  indentation -= 4;
}

void
CxxLinedStatement::emit()
{
  std::cout << std::endl << std::flush;
  for (int i = 0; i < indentation; i++) std::cout << " ";
  indentation += 4;
  // std::cout << "(" << token_type << std::flush;
  emit_subtoken (aStatement);
  // emit_subtoken (aLine);
  // std::cout << ")";
  indentation -= 4;
}

void
CxxLinedToken::emit()
{
  emit_subtoken (aToken);
}

void
CxxLinkageSpecifier::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aString);
  emit_subtoken (aDeclarationList);
  std::cout << ")";
}
void
CxxLinkageSpecification::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxMemInitializer::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  emit_subtoken (anExpr);
  std::cout << ")";
}

void
CxxMultiplyExpression::emit()
{
  std::cout << "(" << operator_name;
  emit_subtoken (aDeclarator);
  emit_subtoken (left_operand);
  emit_subtoken (right_operand);
  std::cout << ")";
}

void
CxxName::emit()
{
  std::cout << "(" << token_type << std::flush;
  std::cout << ")";
}

void
CxxNameExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxNamespaceAliasDefinition::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  emit_subtoken (forId);
  std::cout << ")";
}

void
CxxNameSpaceDeclaration::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxNameSpaceDefinition::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  emit_subtoken (aDeclaration);
  std::cout << ")";
}

void
CxxNestedDeclarator::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  emit_subtoken (aDeclarator);
  std::cout << ")";
}

void
CxxNestedId::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (nestingId);
  emit_subtoken (nestedId);
  std::cout << ")";
}

void
CxxNestedScope::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (nestingId);
  std::cout << ")";
}

void
CxxNewExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aPlace);
  emit_subtoken (aTypeParams);
  emit_subtoken (anInit);
  std::cout << ")";
}

void
CxxNewTypeIdExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aPlace);
  emit_subtoken (aTypeExpr);
  emit_subtoken (anInit);
  std::cout << ")";
}

void
CxxOperatorFunctionId::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (operatorId);
  std::cout << ")";
}

void
CxxParameterList::emit1()
{
  if ((aList == 0) && (anElement == 0)) return;
  if (aList != 0) aList->emit1();
  emit_subtoken (anElement);
}

void
CxxParameterList::emit()
{
  std::cout << "(" << token_type << std::flush;
  if (aList) {
      aList->emit1();
      }
  if (anElement) {
      emit_subtoken (anElement);
      }
  std::cout << ")";
}

void
CxxParenthesised::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aList);
  emit_subtoken (cvQualifierList);
  emit_subtoken (exceptionSpecification);
  std::cout << ")";
}

void
CxxPointerDeclarator::emit()
{
  std::cout << "(" << token_type << std::flush;
  std::cout << ")";
}

void
CxxPseudoDestructorId::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aScope);
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxPointerExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aDeclarator);
  emit_subtoken (anExpr);
  std::cout << ")";
}

void
CxxReferenceDeclarator::emit()
{
  std::cout << "(" << token_type << std::flush;
  std::cout << ")";
}

void
CxxReturnStatement::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (anExpr);
  std::cout << ")";
}

void
CxxScopedId::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (globalId);
  emit_subtoken (nestedId);
  std::cout << ")";
}

void
CxxScopedPointerExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aScope);
  emit_subtoken (aDeclarator);
  emit_subtoken (anExpr);
  std::cout << ")";
}

void
CxxSetTemplateDeclSpecifier::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxSetTemplateDeclaration::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aDeclaration);
  std::cout << ")";
}

void
CxxSetTemplateExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (anExpr);
  std::cout << ")";
}

void
CxxSetTemplateId::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxSetTemplateName::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxSetTemplateScope::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxThrowExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (anExpr);
  std::cout << ")";
}

void
CxxTrueExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  std::cout << ")";
}

void
CxxTryBlock::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aStatement);
  emit_subtoken (exceptionHandlerList);
  std::cout << ")";
}

void
CxxTryBlockStatement::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (tryBlock);
  std::cout << ")";
}

void
CxxTryFunctionBlock::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (functionBody);
  emit_subtoken (exceptionHandlerList);
  std::cout << ")";
}

void
CxxType1Expression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (functionName);
  emit_subtoken (aParenthesis);
  emit_subtoken (type1ParameterList);
  std::cout << ")";
}

void
CxxTypeidExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aList);
  std::cout << ")";
}

void
CxxTypenameTypeParameter::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxSimpleDeclaration::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (anExpr);
  std::cout << ")";
}

void
CxxSimpleTypeParameter::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxStatementList::emit1()
{
  if ((aList == 0) && (anElement == 0)) return;
  if (aList) aList->emit1();
  emit_subtoken (anElement);
}

void
CxxStatementList::emit()
{
  std::cout << std::endl << std::flush;
  for (int i = 0; i < indentation; i++) std::cout << " ";
  indentation += 4;
  std::cout << "(" << token_type << std::flush;
  if (aList) {
      std::cout << " ";
      aList->emit1();
      }
  if (anElement) {
      std::cout << " ";
      anElement->emit();
      }
  else std::cout << " NULL";
  std::cout << ")";
  indentation -= 4;
}

void
CxxStringList::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (anElement);
  emit_subtoken (aList);
  std::cout << ")";
}

void
CxxStringLiteralExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aString);
  std::cout << ")";
}

void
CxxSwitchStatement::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (testExpr);
  emit_subtoken (aStmt);
  std::cout << ")";
}

void
CxxTemplateArgument::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aParameter);
  std::cout << ")";
}

void
CxxTemplatedTypeParameter::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (templateParameterList);
  emit_subtoken (aName);
  std::cout << ")";
}

void
CxxTemplateName::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aName);
  emit_subtoken (templateArguments);
  std::cout << ")";
}

void
CxxTemplateParameter::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (aParameter);
  std::cout << ")";
}

void
CxxThisExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  std::cout << ")";
}

void
CxxToken::emit()
{
  if ((_value >= 258) && (_value <= 345)) {
      std::cout << token_type << std::flush;
      }
  else
      std::cout << "(" << token_type << " ...)";
}

void
CxxTokenList::emit1()
{
  if ((more_tokens == 0) && (this_token == 0)) return;

  if (more_tokens) {more_tokens->emit1();}
  emit_subtoken (this_token);
  return;
}

void
CxxTokenList::emit()
{
  std::cout << "(" << token_type << std::flush;
  if (more_tokens)
      more_tokens->emit1();
  if (this_token)
      emit_subtoken (this_token);
  std::cout << ")";
}

void
CxxTypedExpression::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (frontName);
  emit_subtoken (backName);
  std::cout << ")";
}

void
CxxTypedName::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (frontName);
  emit_subtoken (backName);
  std::cout << ")";
}

void
CxxUnaryExpression::emit()
{
  std::cout << "(" << operator_name;
  emit_subtoken (operand);
  std::cout << ")";
}

void
CxxWhileStatement::emit()
{
  std::cout << "(" << token_type << std::flush;
  emit_subtoken (testExpr);
  emit_subtoken (aStmt);
  std::cout << ")";
}

CxxNaffToken::CxxNaffToken(int tokenValue, const char *yyText, int yyLeng)
:
    Super(tokenValue, "CxxNaffToken"), _text(new char[yyLeng+1]), _leng(yyLeng)
{
	memcpy(_text, yyText, yyLeng);
	_text[_leng] = 0;
}

void
CxxNaffToken::emit()
{
  // std::cout << "(" << token_type << " " << _text << ")";
  std::cout << "\"" << _text << "\"";
}

CxxNaffToken::~CxxNaffToken() { delete[] _text; }
