namespace Microsoft.Dafny.CustomizedTreeVisitor {
    using System;
  using System.Collections.Generic;
  using System.Collections.ObjectModel;
  using System.Diagnostics.Contracts;
  using System.IO;
  using System.Linq;
  using System.Numerics;

  using Bpl = Microsoft.Boogie;
  /// <summary>
  /// Base syntax tree visitor implementation that visits all nodes.
  /// </summary>
  public abstract class SyntaxTreeVisitor {
    // Double-dispatching would be convenient here, but requirees adaptions to the AST.
    // TODO Is visiting Attributes necessary, i.e., does it belong to the AST?

    /// <summary>
    /// This method is invoked as soon as the visitor encounters an unknown syntax node.
    /// </summary>
    /// <param name="node">The unknown node that is being visited.</param>
    /// <param name="token">The token asociated with the unknown node.</param>
    public abstract void VisitUnknown(object node, Boogie.IToken token);

    public virtual void Visit(Dafny.Program program) {
      foreach (var module in program.Modules()) {
        Visit(module);
      }
    }

    public virtual void Visit(ModuleDefinition moduleDefinition) {
      foreach (var topLevelDeclaration in moduleDefinition.TopLevelDecls) {
        Visit(topLevelDeclaration);
      }
    }

    public virtual void Visit(TopLevelDecl topLevelDeclaration) {
      switch (topLevelDeclaration) {
        case ClassDecl classDeclaration:
          Visit(classDeclaration);
          break;
        case DatatypeDecl dataTypeDeclaration:
          Visit(dataTypeDeclaration);
          break;
        case ModuleDecl moduleDeclaration:
        case ValuetypeDecl valueTypeDeclaration:
        case OpaqueTypeDecl opaqueTypeDeclaration:
        case NewtypeDecl newTypeDeclaration:
        case TypeSynonymDecl typeSynonymDeclaration:
        default:
          VisitUnknown(topLevelDeclaration, topLevelDeclaration.tok);
          break;
      }
    }

    public virtual void Visit(ExprRhs expressionRhs) {
      VisitNullableAttributes(expressionRhs.Attributes);
      Visit(expressionRhs.Expr);
    }
    public virtual void Visit(ClassDecl classDeclaration) {
      foreach (var member in classDeclaration.Members) {
        Visit(member);
      }
    }

    public virtual void Visit(DatatypeDecl dataTypeDeclaration) {
      foreach (var member in dataTypeDeclaration.Members) {
        Visit(member);
      }
    }

    public virtual void Visit(MemberDecl memberDeclaration) {
      switch (memberDeclaration) {
        case Field field:
          Visit(field);
          break;
        case Function function:
          Visit(function);
          break;
        case Method method:
          Visit(method);
          break;
        default:
          VisitUnknown(memberDeclaration, memberDeclaration.tok);
          break;
      }
    }

    public virtual void Visit(Field field) {
      foreach (var expression in field.SubExpressions) {
        Visit(expression);
      }
    }

    public virtual void Visit(Method method) {
      foreach (var typeArgument in method.TypeArgs) {
        Visit(typeArgument);
      }
      foreach (var inDefinition in method.Ins) {
        Visit(inDefinition);
      }
      foreach (var outDefinition in method.Outs) {
        Visit(outDefinition);
      }
      VisitNullableAttributes(method.Attributes);
      foreach (var requirement in method.Req) {
        Visit(requirement);
      }
      foreach (var ensurement in method.Ens) {
        Visit(ensurement);
      }
      Visit(method.Decreases);
      Visit(method.Mod);
      VisitNullableBlock(method.Body);
    }

    public virtual void Visit(Constructor constructor) {
      foreach (var outDefinition in constructor.Outs) {
        Visit(outDefinition);
      }
      VisitNullableBlock(constructor.Body);
    }

    public virtual void Visit(Function function) {
      foreach (var typeArgument in function.TypeArgs) {
        Visit(typeArgument);
      }
      foreach (var formal in function.Formals) {
        Visit(formal);
      }
      foreach (var read in function.Reads) {
        Visit(read);
      }
      foreach (var requirement in function.Req) {
        Visit(requirement);
      }
      foreach (var ensurement in function.Ens) {
        Visit(ensurement);
      }
      Visit(function.Decreases);
      VisitNullableExpression(function.Body);
    }

    public virtual void Visit(NonglobalVariable nonGlobalVariable) {
    }

    public virtual void Visit(Formal formal) {
    }

    public virtual void Visit(LocalVariable localVariable) {
      VisitNullableAttributes(localVariable.Attributes);
    }

    public void VisitNullableAttributes(Attributes? attributes) {
      if (attributes != null) {
        Visit(attributes);
      }
    }

    public virtual void Visit(Attributes attributes) {
      foreach (var argument in attributes.Args) {
        Visit(argument);
      }
    }

    public virtual void Visit(AssignStmt assignStatement) {
      VisitNullableExpression(assignStatement.Lhs);
      Visit(assignStatement.Rhs);
    }
    public virtual void Visit(Statement statement) {
      switch (statement) {
        case WhileStmt whileStatement:
          Visit(whileStatement);
          break;
        case ForLoopStmt forStatement:
          Visit(forStatement);
          break;
        case AlternativeLoopStmt alternativeLoopStmt:
          Visit(alternativeLoopStmt);
          break;
        case IfStmt ifStatement:
          Visit(ifStatement);
          break;
        case AlternativeStmt alternativeStatement:
          Visit(alternativeStatement);
          break;
        case VarDeclStmt variableDeclarationStatement:
          Visit(variableDeclarationStatement);
          break;
        case UpdateStmt updateStatement:
          Visit(updateStatement);
          break;
        case AssertStmt assertStatement:
          Visit(assertStatement);
          break;
        case ReturnStmt returnStatement:
          Visit(returnStatement);
          break;
        case BlockStmt blockStatement:
          Visit(blockStatement);
          break;
        case MatchStmt matchStatement:
          Visit(matchStatement);
          break;
        case NestedMatchStmt nestedMatchStatement:
          Visit(nestedMatchStatement);
          break;
        case ForallStmt forAllStatement:
          Visit(forAllStatement);
          break;
        case PrintStmt printStatement:
          Visit(printStatement);
          break;
        case AssignStmt assignStatement:
          Visit(assignStatement);
          break;
        default:
          // Console.WriteLine("found unknown statement");
          // Console.WriteLine(statement.GetType());
          VisitUnknown(statement, statement.Tok);
          break;
      }
    }

    public void VisitNullableStatement(Statement? statement) {
      if (statement != null) {
        Visit(statement);
      }
    }

    

    public virtual void Visit(AssignmentRhs assignmentRhs) {
      switch (assignmentRhs) {
        case ExprRhs expressionRhs:
          Visit(expressionRhs);
          break;
        case TypeRhs typeRhs:
          Visit(typeRhs);
          break;
        default:
          VisitUnknown(assignmentRhs, assignmentRhs.Tok);
          break;
      }
    }

    public virtual void Visit(TypeRhs typeRhs) {
      VisitNullableAttributes(typeRhs.Attributes);
      if (typeRhs.Bindings != null) {
        Visit(typeRhs.Bindings);
      }
      if (typeRhs.ArrayDimensions != null) {
        foreach (var dimension in typeRhs.ArrayDimensions) {
          Visit(dimension);
        }
      }
    }

    public virtual void Visit(BlockStmt blockStatement) {
      VisitNullableAttributes(blockStatement.Attributes);
      foreach (var statement in blockStatement.Body) {
        Visit(statement);
      }
    }

    public void VisitNullableBlock(BlockStmt? blockStatement) {
      if (blockStatement != null) {
        Visit(blockStatement);
      }
    }

    public virtual void Visit(WhileStmt whileStatement) {
      Visit(whileStatement.Guard);
      VisitNullableAttributes(whileStatement.Attributes);
      foreach (var invariant in whileStatement.Invariants) {
        Visit(invariant);
      }
      // TODO Problematic GenericSort test case. Automatically generated decrease specifications
      //      of a while-statement re-use nodes of the statement's guard. This causes the problem
      //      that the same node is visited multiple times. It could just be skipped. Nevertheless,
      //      it'd be more robust if we could identify such cases a-priori to prevent introducing
      //      other programatic errors.
      //Visit(whileStatement.Decreases);
      Visit(whileStatement.Mod);
      VisitNullableBlock(whileStatement.Body);
    }

    public virtual void Visit(ForLoopStmt forStatement) {
      VisitNullableAttributes(forStatement.Attributes);
      Visit(forStatement.Start);
      VisitNullableExpression(forStatement.End);
      VisitNullableAttributes(forStatement.Attributes);
      foreach (var invariant in forStatement.Invariants) {
        Visit(invariant);
      }
      Visit(forStatement.Decreases);
      Visit(forStatement.Mod);
      VisitNullableBlock(forStatement.Body);
    }

    public virtual void Visit(AlternativeLoopStmt alternativeLoopStatement) {
      VisitNullableAttributes(alternativeLoopStatement.Attributes);
      Visit(alternativeLoopStatement.Decreases);
      Visit(alternativeLoopStatement.Mod);
      foreach (var guardedAlternative in alternativeLoopStatement.Alternatives) {
        Visit(guardedAlternative);
      }
    }

    public virtual void Visit(IfStmt ifStatement) {
      // A guard may be null when using an asterisk for non-deterministic choices.
      VisitNullableExpression(ifStatement.Guard);
      VisitNullableAttributes(ifStatement.Attributes);
      VisitNullableBlock(ifStatement.Thn);
      VisitNullableStatement(ifStatement.Els);
    }

    public virtual void Visit(AlternativeStmt alternativeStatement) {
      VisitNullableAttributes(alternativeStatement.Attributes);
      foreach (var guardedAlternative in alternativeStatement.Alternatives) {
        Visit(guardedAlternative);
      }
    }

    public virtual void Visit(GuardedAlternative guardedAlternative) {
      Visit(guardedAlternative.Guard);
      foreach (var statement in guardedAlternative.Body) {
        Visit(statement);
      }
    }

    public virtual void Visit(VarDeclStmt variableDeclarationStatement) {
      foreach (var localVariable in variableDeclarationStatement.Locals) {
        Visit(localVariable);
      }
      if (variableDeclarationStatement.Update != null) {
        Visit(variableDeclarationStatement.Update);
      }
    }

    public virtual void Visit(UpdateStmt updateStatement) {
      VisitNullableAttributes(updateStatement.Attributes);
      foreach (var leftHandSide in updateStatement.Lhss) {
        Visit(leftHandSide);
      }
      foreach (var rightHandSide in updateStatement.Rhss) {
        Visit(rightHandSide);
      }
    }

    public virtual void Visit(AssertStmt assertStatement) {
      VisitNullableAttributes(assertStatement.Attributes);
      Visit(assertStatement.Expr);
      VisitNullableStatement(assertStatement.Proof);
    }

    public virtual void Visit(ReturnStmt returnStatement) {
      VisitNullableAttributes(returnStatement.Attributes);
      if (returnStatement.rhss != null) {
        // In integration test run on ubuntu showed that this might be null.
        // https://github.com/DafnyVSCode/language-server-csharp/runs/1390714082?check_suite_focus=true#step:9:907
        // At the time of this writing, there is no contract in dafny-lang enforcing non-null - so this should be true.
        foreach (var rhs in returnStatement.rhss) {
          Visit(rhs);
        }
      }
    }

    public virtual void Visit(MatchStmt matchStatement) {
      VisitNullableAttributes(matchStatement.Attributes);
      Visit(matchStatement.Source);
      foreach (var matchCase in matchStatement.Cases) {
        Visit(matchCase);
      }
    }

    public virtual void Visit(MatchCaseStmt matchCaseStatement) {
      foreach (var argument in matchCaseStatement.Arguments) {
        Visit(argument);
      }
      foreach (var body in matchCaseStatement.Body) {
        Visit(body);
      }
    }

    public virtual void Visit(NestedMatchStmt nestedMatchStatement) {
      VisitNullableAttributes(nestedMatchStatement.Attributes);
      Visit(nestedMatchStatement.Source);
      foreach (var nestedMatchCase in nestedMatchStatement.Cases) {
        Visit(nestedMatchCase);
      }
    }

    public virtual void Visit(NestedMatchCaseStmt nestedMatchCaseStatement) {
      foreach (var body in nestedMatchCaseStatement.Body) {
        Visit(body);
      }
    }

    public virtual void Visit(ForallStmt forAllStatement) {
      VisitNullableAttributes(forAllStatement.Attributes);
      VisitNullableStatement(forAllStatement.Body);
    }

    public virtual void Visit(PrintStmt printStatement) {
      VisitNullableAttributes(printStatement.Attributes);
      foreach (var argument in printStatement.Args) {
        Visit(argument);
      }
    }

    public virtual void Visit(ActualBindings bindings) {
      bindings.ArgumentBindings.ForEach(Visit);
    }

    public virtual void Visit(ActualBinding binding) {
      Visit(binding.Actual);
    }

    public virtual void Visit(Expression expression) {
      switch (expression) {
        case LiteralExpr literalExpression:
          Visit(literalExpression);
          break;
        case ThisExpr thisExpression:
          Visit(thisExpression);
          break;
        case IdentifierExpr identifierExpression:
          Visit(identifierExpression);
          break;
        case SeqSelectExpr sequenceSelectExpression:
          Visit(sequenceSelectExpression);
          break;
        case UnaryExpr unaryExpression:
          Visit(unaryExpression);
          break;
        case BinaryExpr binaryExpression:
          Visit(binaryExpression);
          break;
        case TernaryExpr ternaryExpression:
          Visit(ternaryExpression);
          break;
        case NameSegment nameSegment:
          Visit(nameSegment);
          break;
        case ParensExpression parenthesesExpression:
          Visit(parenthesesExpression);
          break;
        case ExprDotName expressionDotName:
          Visit(expressionDotName);
          break;
        case ApplySuffix applySuffix:
          Visit(applySuffix);
          break;
        case ChainingExpression chainingExpression:
          Visit(chainingExpression);
          break;
        case NegationExpression negationExpression:
          Visit(negationExpression);
          break;
        case OldExpr oldExpression:
          Visit(oldExpression);
          break;
        case ITEExpr ifThenElseExpression:
          Visit(ifThenElseExpression);
          break;
        case ForallExpr forAllExpression:
          Visit(forAllExpression);
          break;
        case NestedMatchExpr nestedMatchExpression:
          Visit(nestedMatchExpression);
          break;
        case SetDisplayExpr setDisplayExpression:
          Visit(setDisplayExpression);
          break;
        case MultiSetDisplayExpr multiSetDisplayExpression:
          Visit(multiSetDisplayExpression);
          break;
        case SeqDisplayExpr sequenceDisplayExpression:
          Visit(sequenceDisplayExpression);
          break;
        case StmtExpr statementExpression:
          Visit(statementExpression);
          break;
        default:
          VisitUnknown(expression, expression.tok);
          break;
      }
    }

    public void VisitNullableExpression(Expression? expression) {
      if (expression != null) {
        Visit(expression);
      }
    }

    public virtual void Visit(AutoGhostIdentifierExpr autoGhostIdentifierExpression) {
    }

    public virtual void Visit(LiteralExpr literalExpression) {
    }

    public virtual void Visit(IdentifierExpr identifierExpression) {
    }

    public virtual void Visit(ApplySuffix applySuffix) {
      Visit(applySuffix.Lhs);
      Visit(applySuffix.Bindings);
    }

    public virtual void Visit(NameSegment nameSegment) {
    }

    public virtual void Visit(AliasModuleDecl aliasModuleDecl) {
    }

    public virtual void Visit(ExprDotName expressionDotName) {
      Visit(expressionDotName.Lhs);
    }

    public virtual void Visit(ThisExpr thisExpression) {
    }

    public virtual void Visit(DisplayExpression displayExpression) {
    }

    public virtual void Visit(ComprehensionExpr comprehensionExpression) {
    }

    public virtual void Visit(AttributedExpression attributedExpression) {
      VisitNullableAttributes(attributedExpression.Attributes);
      Visit(attributedExpression.E);
    }

    public virtual void Visit(SeqSelectExpr sequenceSelectExpression) {
      VisitNullableExpression(sequenceSelectExpression.Seq);
      VisitNullableExpression(sequenceSelectExpression.E0);
      VisitNullableExpression(sequenceSelectExpression.E1);
    }

    public virtual void Visit(TypeParameter typeParameter) {
    }

    public virtual void Visit(ParensExpression parenthesesExpression) {
      Visit(parenthesesExpression.E);
    }

    public virtual void Visit(UnaryExpr unaryExpression) {
      VisitNullableExpression(unaryExpression.E);
    }

    public virtual void Visit(BinaryExpr binaryExpression) {
      VisitNullableExpression(binaryExpression.E0);
      VisitNullableExpression(binaryExpression.E1);
    }

    public virtual void Visit(TernaryExpr ternaryExpression) {
      VisitNullableExpression(ternaryExpression.E0);
      VisitNullableExpression(ternaryExpression.E1);
      VisitNullableExpression(ternaryExpression.E2);
    }

    public virtual void Visit(ChainingExpression chainingExpression) {
      foreach (var operand in chainingExpression.Operands) {
        Visit(operand);
      }
    }

    public virtual void Visit(NegationExpression negationExpression) {
      Visit(negationExpression.E);
    }

    public virtual void Visit(OldExpr oldExpression) {
      Visit(oldExpression.E);
    }

    public virtual void Visit(ITEExpr ifThenElseExpression) {
      Visit(ifThenElseExpression.Test);
      Visit(ifThenElseExpression.Thn);
      Visit(ifThenElseExpression.Els);
    }

    public virtual void Visit(ForallExpr forAllExpression) {
      VisitNullableAttributes(forAllExpression.Attributes);
      VisitNullableExpression(forAllExpression.Range);
      Visit(forAllExpression.Term);
    }

    public virtual void Visit(NestedMatchExpr nestedMatchExpression) {
      Visit(nestedMatchExpression.Source);
      foreach (var nestedMatchCaseExpression in nestedMatchExpression.Cases) {
        Visit(nestedMatchCaseExpression);
      }
    }

    public virtual void Visit(NestedMatchCaseExpr nestedMatchCaseExpression) {
      Visit(nestedMatchCaseExpression.Pat);
      Visit(nestedMatchCaseExpression.Body);
    }

    public virtual void Visit(SetDisplayExpr setDisplayExpression) {
      foreach (var element in setDisplayExpression.Elements) {
        Visit(element);
      }
    }

    public virtual void Visit(MultiSetDisplayExpr multiSetDisplayExpression) {
      foreach (var element in multiSetDisplayExpression.Elements) {
        Visit(element);
      }
    }

    public virtual void Visit(SeqDisplayExpr sequenceDisplayExpression) {
      foreach (var element in sequenceDisplayExpression.Elements) {
        Visit(element);
      }
    }

    public virtual void Visit(Specification<Expression> specification) {
      VisitNullableAttributes(specification.Attributes);
      if (specification.Expressions != null) {
        foreach (var expression in specification.Expressions) {
          Visit(expression);
        }
      }
    }

    public virtual void Visit(Specification<FrameExpression> specification) {
      VisitNullableAttributes(specification.Attributes);
      if (specification.Expressions != null) {
        foreach (var expression in specification.Expressions) {
          Visit(expression);
        }
      }
    }

    public virtual void Visit(FrameExpression frameExpression) {
    }

    public virtual void Visit(ExtendedPattern extendedPattern) {
      // TODO Visit the various pattern types.
    }

    public virtual void Visit(StmtExpr statementExpression) {
      Visit(statementExpression.S);
      Visit(statementExpression.E);
    }
  }

  public class HaosASTVisitor : SyntaxTreeVisitor {

    Dafny.Program program = null;

    public bool Test() {
      TextWriter tmp = Console.Out;
      Console.SetOut(StreamWriter.Null);
      var testResult =  Microsoft.Dafny.DafnyDriver.Varify(this.program);
      Console.SetOut(tmp);
      return testResult;
    }

    public override void Visit(ExprRhs expressionRhs) {
      // Console.WriteLine("Visited rhs");
      // Console.WriteLine(expressionRhs.Expr.tok.val);
      if (expressionRhs.Expr.tok.val == "2"){
        
          // Console.WriteLine("found 2");
          expressionRhs.Expr.tok.val = "5";
          

      }
      // Console.WriteLine(expressionRhs.Expr.tok.val);
      VisitNullableAttributes(expressionRhs.Attributes);
      Visit(expressionRhs.Expr);
    }
    public override void Visit(Dafny.Program program) {
      
      this.program = program;
      // foreach (var module in program.CompileModules) {
      //   Visit(module);
      // }
      foreach (var module in program.RawModules()) {
        Visit(module);
      }
    }

    public virtual void Visit(ModuleDefinition moduleDefinition) {
      foreach (var topLevelDeclaration in moduleDefinition.TopLevelDecls) {
        Visit(topLevelDeclaration);
      }
    }

      public override void VisitUnknown(object node, Boogie.IToken token){
          return;
      }
    
    public virtual void Visit(DatatypeDecl dataTypeDeclaration) {
      foreach (var member in dataTypeDeclaration.Members) {
        Visit(member);
      }
      // Visit(dataTypeDeclaration)
      VisitNullableAttributes(dataTypeDeclaration.Attributes);
      // Console.WriteLine(dataTypeDeclaration.Attributes);
    }
    public void Visit(ModuleDecl d) {
      // foreach (var member in d.Members) {
      //   Visit(member);
      // }
    }

    public void Visit(ValuetypeDecl d) {
      // foreach (var member in d.Members) {
      //   Visit(member.value);
      // }
    }

    public void Visit(OpaqueTypeDecl d) {
      foreach (var member in d.Members) {
        Visit(member);
      }
    }

    public void Visit(NewtypeDecl d) {
      foreach (var member in d.Members) {
        Visit(member);
      }
      VisitNullableExpression(d.Witness);
      VisitNullableExpression(d.Constraint);
      VisitNullableAttributes(d.Attributes);
    }

    public void Visit(TypeSynonymDecl d) {
      VisitNullableAttributes(d.Attributes);
    }

    public void Visit(DatatypeCtor d) {
      VisitNullableAttributes(d.Attributes);
    }

    public void Visit(ClassDecl classDeclaration) {
      foreach (var member in classDeclaration.Members) {
        Visit(member);
      }
      
      foreach (var member in classDeclaration.InheritedMembers) {
        Visit(member);
      }
      VisitNullableAttributes(classDeclaration.Attributes);
    }
    

    public override void Visit(LiteralExpr literalExpression) {
      
      if (literalExpression.Value is BigInteger i) {
        // if (i == 2) {
        //   literalExpression.Value = i + 3;
        // }
        literalExpression.Value = i + 1;
        if (this.Test()) {
          // Console.Write(this.Test());
          Console.Write("Increment on line ");
          Console.Write(literalExpression.tok.line);
          Console.Write(" col ");
          Console.Write(literalExpression.tok.col);
          Console.WriteLine(" passed the test");
        }
        literalExpression.Value = i - 1;

        if (this.Test()) {
          Console.Write("Decrement on line ");
          Console.Write(literalExpression.tok.line);
          Console.Write(" col ");
          Console.Write(literalExpression.tok.col);
          Console.WriteLine(" passed the test");
        }
        literalExpression.Value = i;
      }
      else if (literalExpression is CharLiteralExpr) {
        // Console.WriteLine(literalExpression.Value);
        string s = "" + literalExpression.Value;
        char c = s[0];
        literalExpression.Value = "" + ((char)(Convert.ToUInt16(c) + 1));
        if (this.Test()) {
          Console.Write("Increment char on line ");
          Console.Write(literalExpression.tok.line);
          Console.Write(" col ");
          Console.Write(literalExpression.tok.col);
          Console.WriteLine(" passed the test");
        }
        Console.WriteLine("after change");
        Console.WriteLine(literalExpression.Value);
        literalExpression.Value = "" + ((char)(Convert.ToUInt16(c) - 1));

        if (this.Test()) {
          Console.Write("Decrement char on line ");
          Console.Write(literalExpression.tok.line);
          Console.Write(" col ");
          Console.Write(literalExpression.tok.col);
          Console.WriteLine(" passed the test");
        }

        literalExpression.Value = s;
      }
      else if (literalExpression is StringLiteralExpr) {
        // Console.WriteLine(literalExpression.Value);
        string s = "" + literalExpression.Value;

        literalExpression.Value = "";
        if (this.Test()) {
          Console.Write("empty string on line ");
          Console.Write(literalExpression.tok.line);
          Console.Write(" col ");
          Console.Write(literalExpression.tok.col);
          Console.WriteLine(" passed the test");
        }
        if (s.Length == 0) {
          literalExpression.Value = "a";
          if (this.Test()) {
            Console.Write("Adding char on line ");
            Console.Write(literalExpression.tok.line);
            Console.Write(" col ");
            Console.Write(literalExpression.tok.col);
            Console.WriteLine(" passed the test");
          }
        }
        else if (s.Length == 1) {
          literalExpression.Value = s + s;
          if (this.Test()) {
            Console.Write("dup char on line ");
            Console.Write(literalExpression.tok.line);
            Console.Write(" col ");
            Console.Write(literalExpression.tok.col);
            Console.WriteLine(" passed the test");
          }

          literalExpression.Value = "a";
          if (this.Test()) {
            Console.Write("replace char on line ");
            Console.Write(literalExpression.tok.line);
            Console.Write(" col ");
            Console.Write(literalExpression.tok.col);
            Console.WriteLine(" passed the test");
          }
        }
        else if (s.Length > 1) {
          literalExpression.Value = s[s.Length - 1] + s.Substring(0, s.Length-1);
          if (this.Test()) {
            Console.Write("left shift char on line ");
            Console.Write(literalExpression.tok.line);
            Console.Write(" col ");
            Console.Write(literalExpression.tok.col);
            Console.WriteLine(" passed the test");
          }

          literalExpression.Value = s.Substring(1, s.Length-1) + s[0];

          if (this.Test()) {
            Console.Write("right shift char on line ");
            Console.Write(literalExpression.tok.line);
            Console.Write(" col ");
            Console.Write(literalExpression.tok.col);
            Console.WriteLine(" passed the test");
          }

          if (s.Length > 2) {
            string ss = "" + s;
            ss  = ss.Replace(ss[ss.Length/2], ((char)(Convert.ToUInt16(ss[ss.Length/2]) - 1)));
            literalExpression.Value = ss;
            if (this.Test()) {
              Console.Write("Add char on line ");
              Console.Write(literalExpression.tok.line);
              Console.Write(" col ");
              Console.Write(literalExpression.tok.col);
              Console.WriteLine(" passed the test");
            }

            int l = s.Length / 2; 
            literalExpression.Value = s.Substring(0, l) + s.Substring(l+1, l - 1);

            if (this.Test()) {
              Console.Write("Remove char on line ");
              Console.Write(literalExpression.tok.line);
              Console.Write(" col ");
              Console.Write(literalExpression.tok.col);
              Console.WriteLine(" passed the test");
            }
          }

        } 
        
        literalExpression.Value = s;
      }
      else if (literalExpression.Value is bool) {
        bool b = !((bool) literalExpression.Value);
        literalExpression.Value = b;
        if (this.Test()) {
          Console.Write("Invert bool on line ");
          Console.Write(literalExpression.tok.line);
          Console.Write(" col ");
          Console.Write(literalExpression.tok.col);
          Console.WriteLine(" passed the test");
        }
        literalExpression.Value = !b;
      }
      
    }


    public virtual void Visit(TopLevelDecl topLevelDeclaration) {
      switch (topLevelDeclaration) {
        case ClassDecl classDeclaration:
          Visit(classDeclaration);
          break;
        case DatatypeDecl dataTypeDeclaration:
          Visit(dataTypeDeclaration);
          break;
        case ModuleDecl moduleDeclaration:
          // Visit(moduleDeclaration);
          break;
        case ValuetypeDecl valueTypeDeclaration:
          // Visit(valueTypeDeclaration);
          // break;
          break;
        case OpaqueTypeDecl opaqueTypeDeclaration:
          Visit(opaqueTypeDeclaration);
          break;
        case NewtypeDecl newTypeDeclaration:
          Visit(newTypeDeclaration);
          break;
        case TypeSynonymDecl typeSynonymDeclaration:
          Visit(typeSynonymDeclaration);
          break;
        default:
          VisitUnknown(topLevelDeclaration, topLevelDeclaration.tok);
          break;
      }
    }

  }

}
