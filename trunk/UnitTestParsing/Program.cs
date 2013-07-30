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
                        attributeLists: new SyntaxList<AttributeListSyntax> { },
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
                // Need SemanticModel here to determine type of the assertion function parameter. For now, we know that IsTrue expects a bool
                var n = (callingAssertionExpr.Expression as MemberAccessExpressionSyntax).Name.Identifier.ValueText;
                var returnType = Syntax.ParseTypeName(n == "IsTrue" ? "bool" : "void");

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
                        WithSemicolonToken(Syntax.Token(SyntaxKind.SemicolonToken))}
                };
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
}


