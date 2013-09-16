using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Roslyn.Compilers;
using Roslyn.Compilers.CSharp;
using Roslyn.Services;
using Roslyn.Services.CSharp;

namespace UnitTestParsing
{
    static class WithExt
    {
        public static T WithTfmIfTrue<T>(this T input, Func<T, T> tfm, Func<bool> pred)
        {
            return pred() ? tfm(input) : input;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var syntaxTree = SyntaxTree.ParseText(
@"using System;
using NUnit.Framework;
using Moq;

namespace UnitTestTesting
{
    [TestFixture]
    public class TestsInHere
    {
        [Test]
        public void ThisIsATest1()
        {
            var a = Mock<AnInterface>();
            Assert.IsTrue(a.DoIt());
        }

        [Test]
        public void ThisIsATest2()
        {
            var a = Mock<AnInterface>();
            Assert.IsTrue(a.Verify(1));
        }

        private static bool Predicate(int a)
        {
            return a>=0;
        }
    }
}
");

            syntaxTree.Dump();
            /*
            [-] Roslyn.Compilers.CSharp.CompilationUnitSyntax
             [-] Roslyn.Compilers.CSharp.UsingDirectiveSyntax -- using System;
              [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
             [-] Roslyn.Compilers.CSharp.UsingDirectiveSyntax -- using NUnit.Framework;
              [-] Roslyn.Compilers.CSharp.QualifiedNameSyntax
               [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
               [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
             [-] Roslyn.Compilers.CSharp.UsingDirectiveSyntax -- using Moq;
              [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
             [-] Roslyn.Compilers.CSharp.NamespaceDeclarationSyntax -- namespace UnitTestTesting
              [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
              [-] Roslyn.Compilers.CSharp.ClassDeclarationSyntax -- [TestFixture] public class TestsInHere
               [-] Roslyn.Compilers.CSharp.AttributeListSyntax
                [-] Roslyn.Compilers.CSharp.AttributeSyntax
                 [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
               [-] Roslyn.Compilers.CSharp.MethodDeclarationSyntax -- [Test] public void ThisIsATest1
                [-] Roslyn.Compilers.CSharp.AttributeListSyntax
                 [-] Roslyn.Compilers.CSharp.AttributeSyntax
                  [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
                [.] Roslyn.Compilers.CSharp.PredefinedTypeSyntax
                [.] Roslyn.Compilers.CSharp.ParameterListSyntax
                [-] Roslyn.Compilers.CSharp.BlockSyntax
                 [-] Roslyn.Compilers.CSharp.LocalDeclarationStatementSyntax -- var a = Mock<AnInterface>();
                  [-] Roslyn.Compilers.CSharp.VariableDeclarationSyntax
                   [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
                   [-] Roslyn.Compilers.CSharp.VariableDeclaratorSyntax
                    [-] Roslyn.Compilers.CSharp.EqualsValueClauseSyntax
                     [-] Roslyn.Compilers.CSharp.InvocationExpressionSyntax
                      [-] Roslyn.Compilers.CSharp.GenericNameSyntax
                       [-] Roslyn.Compilers.CSharp.TypeArgumentListSyntax
                        [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
                      [.] Roslyn.Compilers.CSharp.ArgumentListSyntax
                 [-] Roslyn.Compilers.CSharp.ExpressionStatementSyntax -- Assert.IsTrue(a.DoIt());
                  [-] Roslyn.Compilers.CSharp.InvocationExpressionSyntax
                   [-] Roslyn.Compilers.CSharp.MemberAccessExpressionSyntax
                    [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
                    [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
                   [-] Roslyn.Compilers.CSharp.ArgumentListSyntax
                    [-] Roslyn.Compilers.CSharp.ArgumentSyntax
                     [-] Roslyn.Compilers.CSharp.InvocationExpressionSyntax -- a.DoIt()
                      [-] Roslyn.Compilers.CSharp.MemberAccessExpressionSyntax
                       [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
                       [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
                      [.] Roslyn.Compilers.CSharp.ArgumentListSyntax
               [-] Roslyn.Compilers.CSharp.MethodDeclarationSyntax
                [-] Roslyn.Compilers.CSharp.AttributeListSyntax
                 [-] Roslyn.Compilers.CSharp.AttributeSyntax
                  [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
                [.] Roslyn.Compilers.CSharp.PredefinedTypeSyntax
                [.] Roslyn.Compilers.CSharp.ParameterListSyntax
                [-] Roslyn.Compilers.CSharp.BlockSyntax
                 [-] Roslyn.Compilers.CSharp.LocalDeclarationStatementSyntax
                  [-] Roslyn.Compilers.CSharp.VariableDeclarationSyntax
                   [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
                   [-] Roslyn.Compilers.CSharp.VariableDeclaratorSyntax
                    [-] Roslyn.Compilers.CSharp.EqualsValueClauseSyntax
                     [-] Roslyn.Compilers.CSharp.InvocationExpressionSyntax
                      [-] Roslyn.Compilers.CSharp.GenericNameSyntax
                       [-] Roslyn.Compilers.CSharp.TypeArgumentListSyntax
                        [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
                      [.] Roslyn.Compilers.CSharp.ArgumentListSyntax
                 [-] Roslyn.Compilers.CSharp.ExpressionStatementSyntax
                  [-] Roslyn.Compilers.CSharp.InvocationExpressionSyntax
                   [-] Roslyn.Compilers.CSharp.MemberAccessExpressionSyntax
                    [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
                    [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
                   [-] Roslyn.Compilers.CSharp.ArgumentListSyntax
                    [-] Roslyn.Compilers.CSharp.ArgumentSyntax
                     [-] Roslyn.Compilers.CSharp.InvocationExpressionSyntax
                      [-] Roslyn.Compilers.CSharp.MemberAccessExpressionSyntax
                       [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
                       [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
                      [-] Roslyn.Compilers.CSharp.ArgumentListSyntax
                       [-] Roslyn.Compilers.CSharp.ArgumentSyntax
                        [.] Roslyn.Compilers.CSharp.LiteralExpressionSyntax
               [-] Roslyn.Compilers.CSharp.MethodDeclarationSyntax
                [.] Roslyn.Compilers.CSharp.PredefinedTypeSyntax
                [-] Roslyn.Compilers.CSharp.ParameterListSyntax
                 [-] Roslyn.Compilers.CSharp.ParameterSyntax
                  [.] Roslyn.Compilers.CSharp.PredefinedTypeSyntax
                [-] Roslyn.Compilers.CSharp.BlockSyntax
                 [-] Roslyn.Compilers.CSharp.ReturnStatementSyntax
                  [-] Roslyn.Compilers.CSharp.BinaryExpressionSyntax
                   [.] Roslyn.Compilers.CSharp.IdentifierNameSyntax
                   [.] Roslyn.Compilers.CSharp.LiteralExpressionSyntax              
            */

            var root = (CompilationUnitSyntax)syntaxTree.GetRoot();
            var compilation = Compilation.Create("UnitTesting").
                AddReferences(MetadataReference.CreateAssemblyReference("mscorlib")).
                //AddReferences(MetadataReference.CreateAssemblyReference("nunit.framework")).
                AddSyntaxTrees(syntaxTree);
            var model = compilation.GetSemanticModel(syntaxTree);

            var diagnostics = compilation.GetDiagnostics();
            foreach (var d in diagnostics)
            {
                var lineSpan = d.Location.GetLineSpan(usePreprocessorDirectives: true);
                var startLine = lineSpan.StartLinePosition.Line;
                Console.WriteLine("Line {0}: {1}", startLine, d.Info.GetMessage());
            }

            ReportMethods(compilation.Assembly.GlobalNamespace);

            var methodDecls = syntaxTree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>();

            var methodsHavingTestAttributes = syntaxTree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().
                Where(mdecl=>mdecl.AttributeLists.
                    Any(attrListSyntax=>attrListSyntax.Attributes.
                        Any(attrSyntax=>(attrSyntax.Name as IdentifierNameSyntax).Identifier.ValueText=="Test")));

            //var testMethodsAttrLists = testMethods.Select(md => md.AttributeLists);
            //var b = testMethodsAttrLists.Where(all => all.Any());
            //var c = b.First().Select(al => al.DescendantNodes().OfType<IdentifierNameSyntax>());
            //var attributeName = (b.First().First().Attributes.First().Name as IdentifierNameSyntax).Identifier.ValueText;

//            var vardecl = (VariableDeclarationSyntax)(((LocalDeclarationStatementSyntax)(testMethod.Body.Statements[0])).Declaration);

            // this should be a list of structures, each of which contains all the expected methods the named interface should expose
            var interfacesAndMethodDecls = ConstructMethodDeclarations(model, methodsHavingTestAttributes);

            var interfaceProgramCode = interfacesAndMethodDecls.Select(
                o => Syntax.InterfaceDeclaration(
                        attributeLists: Syntax.List<AttributeListSyntax>(),
                        modifiers: Syntax.TokenList(Syntax.Token(SyntaxKind.PublicKeyword)),
                        typeParameterList: null, //TypeParameterListSyntax,
                        baseList: null, //new BaseListSyntax(),
                        constraintClauses: Syntax.List<TypeParameterConstraintClauseSyntax>(),
                        identifier: Syntax.Identifier(o.Key),
                        members: Syntax.List<MemberDeclarationSyntax>(o.ToList().SelectMany(i=>i.MethodDecls))));

            var interfaceProgramCode_asString = interfaceProgramCode.Select(pc => pc.NormalizeWhitespace().ToFullString());
            
            var newStatement = Syntax.ExpressionStatement(
                Syntax.InvocationExpression(
                    Syntax.MemberAccessExpression(
                        SyntaxKind.MemberAccessExpression,
                        Syntax.IdentifierName("Console"),
                        name: Syntax.IdentifierName("WriteLine")),
                    Syntax.ArgumentList(
                    arguments: Syntax.SeparatedList<ArgumentSyntax>(
                        Syntax.Argument(
                        expression: Syntax.LiteralExpression(
                            SyntaxKind.StringLiteralExpression,
                            Syntax.Literal(
                            text: @"""This is it!""",
                            value: "This is it!")))))));

            var classDeclaration = interfaceProgramCode.Select(CreateImplementingClass);
            var classDecl_asString = classDeclaration.Select(pc => pc.NormalizeWhitespace().ToFullString());
        }

        // Given an interface description create an implementing class
        private static ClassDeclarationSyntax CreateImplementingClass(InterfaceDeclarationSyntax iface)
        {
            var memberDecls = iface.Members.OfType<MethodDeclarationSyntax>().Select(ConstructImplementingFunction);
            return Syntax.ClassDeclaration(attributeLists:Syntax.List<AttributeListSyntax>(),
                modifiers:Syntax.TokenList(Syntax.Token(SyntaxKind.PublicKeyword)),
                identifier: Syntax.Identifier("className"),
                typeParameterList: null,
                baseList: Syntax.BaseList(Syntax.SeparatedList<TypeSyntax>().Add(new [] {Syntax.IdentifierName(iface.Identifier)})),
                constraintClauses: null,
                members: Syntax.List<MemberDeclarationSyntax>(memberDecls));
        }

        private static MemberDeclarationSyntax ConstructImplementingFunction(MethodDeclarationSyntax method)
        {
            var assertionAttrs = method.AttributeLists.First();
            var expectedReturnValue = DeriveExpectedReturnExpression((InvocationExpressionSyntax)(assertionAttrs.Attributes.First().ArgumentList.Arguments.First().Expression));
            // DeriveExpectedReturnValue from the method attribute list
            var returnStmt = Syntax.ReturnStatement(Syntax.Token(SyntaxKind.ReturnKeyword), expectedReturnValue,
                //Syntax.ParseExpression("new " + method.ReturnType.ToString() + "()"), 
                Syntax.Token(SyntaxKind.SemicolonToken));
            return Syntax.MethodDeclaration(attributeLists: Syntax.List<AttributeListSyntax>(),
                modifiers: Syntax.TokenList(Syntax.Token(SyntaxKind.PublicKeyword)),
                returnType: method.ReturnType,
                explicitInterfaceSpecifier: null,
                identifier: method.Identifier,
                typeParameterList: null,
                parameterList: method.ParameterList,
                constraintClauses: null,
                body: Syntax.Block(statements: new[] { returnStmt }));
        }

        private class NameAndInterface
        {
            public string Name { get; set; }
            public IEnumerable<string> MockedTypes { get; set; }
        }

        /// <summary>
        /// Given a sequence of test methods, find all the interfaces and methods expected to be defined by an implementing class
        /// </summary>
        /// <param name="model"></param>
        /// <param name="methodsHavingTestAttributes"></param>
        /// <returns></returns>
        public static IEnumerable<IGrouping<string,InterfaceNameAndMethodDeclarations>> ConstructMethodDeclarations(SemanticModel model, IEnumerable<MethodDeclarationSyntax> methodsHavingTestAttributes)
        {
            var interfaceMethodDecls = new List<InterfaceNameAndMethodDeclarations>();
            foreach (var testMethodDeclaration in methodsHavingTestAttributes)
            {
                var variableDecls = testMethodDeclaration.Body.Statements.OfType<LocalDeclarationStatementSyntax>().
                    Select(stmt => stmt.Declaration).Cast<VariableDeclarationSyntax>();

                var mocks = variableDecls.SelectMany(varDecl => varDecl.DescendantNodes().OfType<GenericNameSyntax>()).
                    Where(nm => nm.Identifier.ValueText == "Mock");
                var mockedInterfaceTypes = mocks.SelectMany(mock => mock.TypeArgumentList.Arguments.OfType<IdentifierNameSyntax>()).
                    Select(ident => ident.Identifier.ValueText); // "AnInterface"

                var variableName = variableDecls.First().Variables.OfType<VariableDeclaratorSyntax>().First().Identifier.ValueText; // "a"

                var theMockVariableDeclarations = variableDecls.
                    Where(varDecl => varDecl.DescendantNodes().OfType<GenericNameSyntax>().Any(nm => nm.Identifier.ValueText == "Mock"));
                var varNamesAndInterfaces = theMockVariableDeclarations.
                    SelectMany(varDecl => varDecl.DescendantNodes().OfType<GenericNameSyntax>()).
                    Where(nm => nm.Identifier.ValueText == "Mock").
                    Select(mock => new NameAndInterface
                    {
                        Name = (mock.Parent.Parent.Parent as VariableDeclaratorSyntax).Identifier.ValueText,
                        MockedTypes = mock.TypeArgumentList.Arguments.OfType<IdentifierNameSyntax>().Select(ident => ident.Identifier.ValueText)
                    }); // "a", {"AnInterface"}

                var variableAccesses = testMethodDeclaration.Body.DescendantNodes().OfType<MemberAccessExpressionSyntax>();

                interfaceMethodDecls.AddRange(ConstructMethodDeclarations(model, variableAccesses, varNamesAndInterfaces));
            }
            return interfaceMethodDecls.GroupBy(ifn => ifn.Name);
        }

        public class InterfaceNameAndMethodDeclarations
        {
            public string Name { get; set; }
            public IEnumerable<MethodDeclarationSyntax> MethodDecls { get; set; }
        }

        /// <summary>
        ///  Given a sequence of member access expressions and variables names, match the variables and interfaces and return a sequence of
        ///  interface name / method declaration pairs.
        /// </summary>
        /// <param name="model"></param>
        /// <param name="variableAccesses"></param>
        /// <param name="varNamesAndInterfaces"></param>
        /// <returns></returns>
        private static IEnumerable<InterfaceNameAndMethodDeclarations> ConstructMethodDeclarations(SemanticModel model, IEnumerable<MemberAccessExpressionSyntax> variableAccesses, IEnumerable<NameAndInterface> varNamesAndInterfaces)
        {
            foreach (var nameAndType in varNamesAndInterfaces)
            {
                var varName = nameAndType.Name;
                var mockedInterfaceTypes = nameAndType.MockedTypes;

                var accesses = variableAccesses.Where(acc => (acc.Expression as IdentifierNameSyntax).Identifier.ValueText == varName);
                var functionNames = accesses.Select(acc => (acc.Name as IdentifierNameSyntax).Identifier.ValueText); // "DoIt" // "Verify"

                var invocationExpr = accesses.First().Parent as InvocationExpressionSyntax;
                // Could call model.GetTypeInfo(invocationExpr) but none of the names involved are defined so the function returns ErrorType, ie unknown
                var invocationArgList = invocationExpr.ArgumentList as ArgumentListSyntax;
                var invocationArgs = invocationArgList.Arguments;
                var functionParameterTypes = invocationArgs.Select(argSyntax =>
                {
                    var argType = model.GetTypeInfo(argSyntax.Expression).Type;
                    return Syntax.Parameter(Syntax.Identifier(@"a")).WithType(Syntax.ParseTypeName(argType.Name));
                });
                var fpt = functionParameterTypes.ToList();

                var callingAssertionExpr = invocationExpr.Parent.Parent.Parent as InvocationExpressionSyntax; // Assert.IsTrue(a.DoIt())
                var returnType = ExtractAssertionReturnType(callingAssertionExpr);

                yield return new InterfaceNameAndMethodDeclarations
                {
                    Name = mockedInterfaceTypes.First(),
                    MethodDecls = new[]{
                    Syntax.MethodDeclaration(returnType, functionNames.First()).
                        WithModifiers(Syntax.TokenList(Syntax.Token(SyntaxKind.PublicKeyword))).
                        WithTfmIfTrue(
                            t => t.WithParameterList(Syntax.ParameterList(
                                    Syntax.SeparatedList<ParameterSyntax>(fpt, Enumerable.Repeat(Syntax.Token(SyntaxKind.CommaToken), fpt.Count - 1)))),
                            () => functionParameterTypes.Any()).
                        WithSemicolonToken(Syntax.Token(SyntaxKind.SemicolonToken)).
                        WithAttributeLists(Syntax.List<AttributeListSyntax>(Syntax.AttributeList().WithAttributes(Syntax.SeparatedList<AttributeSyntax>(Syntax.Attribute(Syntax.ParseName("AssertionOriginAttribute"),Syntax.AttributeArgumentList(callingAssertionExpr.ToAttributeArgumentSyntaxList()))))))}
                };
            }
        }

        public static TypeSyntax ExtractAssertionReturnType(InvocationExpressionSyntax expr)
        {
            // Need SemanticModel here to determine type of the assertion function parameter. For now, we know that IsTrue expects a bool
            var method = ExtractAssertionMethod(expr);
            var returnType = Syntax.ParseTypeName(method == "IsTrue" ? "bool" : "void");
            return returnType;
        }

        public static string ExtractAssertionMethod(InvocationExpressionSyntax expr)
        {
            var method = (expr.Expression as MemberAccessExpressionSyntax).Name.Identifier.ValueText;
            return method;
        }

        public static ExpressionSyntax DeriveExpectedReturnExpression(InvocationExpressionSyntax expr)
        {
            var method = ExtractAssertionMethod(expr);

            var assertionArgList = expr.DescendantNodes().OfType<ArgumentListSyntax>(); // a.DoIt() or a.Verify(1)
            var functionUnderTestArgList = assertionArgList.SelectMany(args => args.DescendantNodes().OfType<ArgumentListSyntax>()); // () or (1)
            if (functionUnderTestArgList.First().Arguments.Any())
            {
                return Syntax.BinaryExpression(SyntaxKind.EqualsExpression,Syntax.IdentifierName("a"),functionUnderTestArgList.First().Arguments.First().Expression);
            }
            else
                switch (method)
                {
                    case "IsTrue":
                        return Syntax.LiteralExpression(
                                SyntaxKind.TrueLiteralExpression,
                                Syntax.Token(SyntaxKind.TrueKeyword));
                    case "IsFalse":
                        return Syntax.LiteralExpression(
                                SyntaxKind.FalseLiteralExpression,
                                Syntax.Token(SyntaxKind.FalseKeyword));
                    default:
                        return Syntax.LiteralExpression(
                                SyntaxKind.FalseLiteralExpression,
                                Syntax.Token(SyntaxKind.FalseKeyword));
                }

        }

        public static IEnumerable<T> Repeat<T>(Func<int, T> f)
        {
            for (int i = 0; ; ++i)
                yield return f(i);
        }

        private static void ReportMethods(NamespaceSymbol namespaceSymbol)
        {
            foreach (var type in namespaceSymbol.GetTypeMembers())
            {
                ReportMethods(type);
            }

            foreach (var childNs in namespaceSymbol.GetNamespaceMembers())
            {
                ReportMethods(childNs);
            }
        }

        private static void ReportMethods(NamedTypeSymbol type)
        {
            foreach (var member in type.GetMembers())
            {
                if (member.CanBeReferencedByName)
                {
                    var name = member.ToDisplayString();
                }
            }

            foreach (var nested in type.GetTypeMembers())
            {
                ReportMethods(nested);
            }
        }
    }

    // See more at: http://www.amazedsaint.com/2012/07/bending-your-code-like-anders-with-c.html#sthash.IvwWIQ1T.dpuf
    public static class SyntaxTreeExtensions 
    {
        public static void Dump(this SyntaxTree tree) 
        {
            var writer = new ConsoleDumpWalker(); 
            writer.Visit(tree.GetRoot()); 
        }
        
        class ConsoleDumpWalker : SyntaxWalker 
        {
            public override void Visit(SyntaxNode node)
            {
                int padding = node.Ancestors().Count(); 
                //To identify leaf nodes vs nodes with children 
                string prepend = node.ChildNodes().Count() > 0 ? "[-]" : "[.]"; 
                //Get the type of the node 
                string line = new String(' ', padding) + prepend + " " + node.GetType().ToString(); 
                //Write the line 
                System.Console.WriteLine(line); 
                base.Visit(node);
            }
        }
    }

    public class AssertionOriginAttribute : Attribute
    {
        public AssertionOriginAttribute(ExpressionSyntax expr):base()
        {
            Expression = expr;
        }

        public ExpressionSyntax Expression { get; private set; }
    }

    public static class Convert
    {
        public static SeparatedSyntaxList<AttributeArgumentSyntax> ToAttributeArgumentSyntaxList(this ExpressionSyntax expr)
        {
            return Syntax.SeparatedList<AttributeArgumentSyntax>(Syntax.AttributeArgument(expr));
        }
    }
}


