import syspro.tm.lexer.*;
import syspro.tm.parser.*;

import syspro.tm.parser.ParseResult;
import syspro.tm.parser.Parser;

import java.util.*;


import java.util.List;

public class ParserSysPro implements Parser {
    private List<Token> tokens;
    private int currentIndex;
    List<TextSpan> ranges;
    List<Diagnostic> diagnostics;

    @Override
    public ParseResult parse(String input) {
        tokens = new LexerSysPro().lex(input);
        currentIndex = 0;
        ranges = new ArrayList<>();
        diagnostics = new ArrayList<>();

        SyntaxNode rootNode = parseSourceText();

        for (Diagnostic diagnostic : diagnostics) {
            System.out.println(diagnostic.errorCode().name());
        }
        return new ParseResult() {
            @Override
            public SyntaxNode root() {
                return rootNode;
            }

            @Override
            public Collection<TextSpan> invalidRanges() {
                return ranges;
            }

            @Override
            public Collection<Diagnostic> diagnostics() {
                return diagnostics;
            }
        };
    }

    private SyntaxNode parseSourceText() {
        List<SyntaxNode> typeDefinitions = new ArrayList<>();
        while (isNotEnd()) {
            SyntaxNode typeDef = parseTypeDef();
            if (typeDef != null) {
                typeDefinitions.add(typeDef);
            }
        }

        return typeDefinitions.isEmpty() ? null : createNode(SyntaxKind.SOURCE_TEXT, List.of(createNode(SyntaxKind.LIST, typeDefinitions)));
    }

    private SyntaxNode parseTypeName() {
        Token optional = null;
        Token lessThen = null;
        Token greaterThen = null;
        List<SyntaxNode> typeArguments = new ArrayList<>();

        if (match(Symbol.QUESTION)) {
            optional = tokens.get(currentIndex - 1);
        }

        Token identifier;
        if (match(SyntaxKind.IDENTIFIER)) {
            identifier = ((IdentifierToken) tokens.get(currentIndex - 1)).withContextualKeyword(null);
        } else {
            ranges.add(tokens.get(currentIndex).span());
            addDiagnostic(String.format("mismatched input %s expecting IDENTIFIER", tokens.get(currentIndex)), tokens.get(currentIndex).span());
            return null;
        }

        if (match(Symbol.LESS_THAN)) {
            lessThen = tokens.get(currentIndex - 1);
            do {
                SyntaxNode typeArgument = parseTypeName();
                if (typeArgument != null) {
                    typeArguments.add(typeArgument);
                }
            } while (match(Symbol.COMMA));

            if (!match(Symbol.GREATER_THAN)) {
                if (typeArguments.size() > 1) {
                    ranges.add(tokens.get(currentIndex).span());
                    addDiagnostic(String.format("mismatched input %s expecting '>'", tokens.get(currentIndex)), tokens.get(currentIndex).span());
                    return null;
                } else if (typeArguments.isEmpty()) {
                    lessThen = null;
                    if (!ranges.isEmpty()) {
                        ranges.removeLast();
                        diagnostics.removeLast();
                    }
                    currentIndex--;
                } else {
                    lessThen = null;
                    currentIndex -= typeArguments.getFirst().slotCount() + 1;
                }
            }
            greaterThen = tokens.get(currentIndex - 1);
        }

        if (optional != null) {
            if (lessThen != null) {
                return createNode(SyntaxKind.OPTION_NAME_EXPRESSION, Arrays.asList(
                        createTerminalNode(SyntaxKind.IDENTIFIER, optional),
                        createNode(SyntaxKind.GENERIC_NAME_EXPRESSION, Arrays.asList(
                                createTerminalNode(SyntaxKind.IDENTIFIER, identifier),
                                createTerminalNode(SyntaxKind.IDENTIFIER, lessThen),
                                createNode(SyntaxKind.SEPARATED_LIST, typeArguments),
                                createTerminalNode(SyntaxKind.IDENTIFIER, greaterThen)))));
            } else {
                return createNode(SyntaxKind.OPTION_NAME_EXPRESSION, Arrays.asList(
                        createTerminalNode(SyntaxKind.IDENTIFIER, optional),
                        createTerminalNode(SyntaxKind.IDENTIFIER, identifier)));
            }
        } else if (lessThen != null) {
            return createNode(SyntaxKind.GENERIC_NAME_EXPRESSION, Arrays.asList(
                    createTerminalNode(SyntaxKind.IDENTIFIER, identifier),
                    createTerminalNode(SyntaxKind.IDENTIFIER, lessThen),
                    createNode(SyntaxKind.SEPARATED_LIST, typeArguments),
                    createTerminalNode(SyntaxKind.IDENTIFIER, greaterThen)));
        } else {
            return createNode(SyntaxKind.IDENTIFIER_NAME_EXPRESSION, List.of(createTerminalNode(SyntaxKind.IDENTIFIER, identifier)));
        }
    }

    private SyntaxNode parseMemberBlock() {
        if (!matchIndent()) {
            return null;
        }

        List<SyntaxNode> memberDefinitions = new ArrayList<>();

        while (isNotEnd() && !matchDedent()) {
            SyntaxNode memberDef = parseMemberDef();
            if (memberDef != null) {
                memberDefinitions.add(memberDef);
            }
        }

        return memberDefinitions.isEmpty() ? null : createNode(SyntaxKind.LIST, memberDefinitions);
    }

    private SyntaxNode parseStatementBlock() {
        // statement_block := INDENT statement+ DEDENT | ε
        if (!matchIndent()) {
            return null;
        }

        List<SyntaxNode> statements = new ArrayList<>();
        while (isNotEnd() && !matchDedent()) {
            SyntaxNode statement = parseStatement();
            if (statement != null) {
                statements.add(statement);
            }
        }

        return statements.isEmpty() ? null : createNode(SyntaxKind.LIST, statements);
    }

    private SyntaxNode parseMemberDef() {
        if (match(Keyword.VAR) || match(Keyword.VAL)) {
            currentIndex--;
            return parseVariableDef();
        } else {
            return parseFunctionDef();
        }
    }

    private SyntaxNode parseStatement() {
        if (match(Keyword.VAR) || match(Keyword.VAL)) {
            return parseVariableDefStmt();
        } else if (match(Keyword.RETURN)) {
            return parseReturnStmt();
        } else if (match(Keyword.BREAK)) {
            return parseBreakStmt();
        } else if (match(Keyword.CONTINUE)) {
            return parseContinueStmt();
        } else if (match(Keyword.IF)) {
            return parseIfStmt();
        } else if (match(Keyword.WHILE)) {
            return parseWhileStmt();
        } else if (match(Keyword.FOR)) {
            return parseForStmt();
        } else {
            return parseExpressionOrAssignmentStmt();
        }
    }

    private SyntaxNode parseTypeDef() {
        if (matchContextualKeyword(Keyword.CLASS) || matchContextualKeyword(Keyword.OBJECT) || matchContextualKeyword(Keyword.INTERFACE)) {
            IdentifierToken keyword = (IdentifierToken) tokens.get(currentIndex - 1);
            Token identifier;
            if (match(SyntaxKind.IDENTIFIER)) {
                identifier = ((IdentifierToken) tokens.get(currentIndex - 1)).withContextualKeyword(null);
            } else {
                ranges.add(tokens.get(currentIndex).span());
                addDiagnostic(String.format("mismatched input %s expecting IDENTIFIER", tokens.get(currentIndex)), tokens.get(currentIndex).span());
                return null;
            }

            SyntaxNode typeParams = null;
            SyntaxNode lessThan = null;
            SyntaxNode greatThan = null;
            if (match(Symbol.LESS_THAN)) {
                lessThan = createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1));
                typeParams = parseTypeParams();
                greatThan = createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1));
            }

            SyntaxNode typeBound = null;
            if (match(Symbol.BOUND)) {
                typeBound = parseTypeBound();
            }

            Token indent = tokens.get(currentIndex);
            SyntaxNode memberBlock = parseMemberBlock();
            Token dedent = tokens.get(currentIndex - 1);
            
            return createNode(SyntaxKind.TYPE_DEFINITION, Arrays.asList(
                    createTerminalNode(SyntaxKind.IDENTIFIER, new KeywordToken(keyword.start, keyword.end, keyword.leadingTriviaLength, keyword.trailingTriviaLength, keyword.contextualKeyword)),
                    createTerminalNode(SyntaxKind.IDENTIFIER, identifier),
                    lessThan,
                    typeParams,
                    greatThan,
                    typeBound,
                    memberBlock != null ? createTerminalNode(SyntaxKind.INDENT, indent) : null,
                    memberBlock,
                    memberBlock != null ? createTerminalNode(SyntaxKind.IDENTIFIER, dedent) : null));
        }
        addDiagnostic(String.format("mismatched input %s expecting {'class', 'object', 'interface'}", tokens.get(currentIndex)), tokens.get(currentIndex).span());
        ranges.add(tokens.get(currentIndex++).span());
        return null;
    }

    private SyntaxNode parseVariableDef() {
        SyntaxNode keyword;
        if (match(Keyword.VAR) || match(Keyword.VAL)) {
            keyword = createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1));
        } else {
            addDiagnostic(String.format("mismatched input %s expecting {'var', 'val'}", tokens.get(currentIndex)), tokens.get(currentIndex).span());
            ranges.add(tokens.get(currentIndex).span());
            return null;
        }

        Token identifier;
        if (match(SyntaxKind.IDENTIFIER)) {
            identifier = ((IdentifierToken) tokens.get(currentIndex - 1)).withContextualKeyword(null);
        } else {
            addDiagnostic(String.format("mismatched input %s expecting IDENTIFIER", tokens.get(currentIndex)), tokens.get(currentIndex).span());
            ranges.add(tokens.get(currentIndex).span());
            return null;
        }

        SyntaxNode typeName = null;
        SyntaxNode colon = null;
        if (match(Symbol.COLON)) {
            colon = createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1));
            typeName = parseTypeName();
        }

        SyntaxNode expression = null;
        SyntaxNode equals = null;
        if (match(Symbol.EQUALS)) {
            equals = createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1));
            expression = parseExpression();
        }

        return createNode(SyntaxKind.VARIABLE_DEFINITION, Arrays.asList(keyword, createTerminalNode(SyntaxKind.IDENTIFIER, identifier), colon, typeName, equals, expression));
    }

    private SyntaxNode parseFunctionDef() {
        List<Token> modifiers = new ArrayList<>();
        while (match(Keyword.ABSTRACT) || match(Keyword.VIRTUAL) || match(Keyword.OVERRIDE) || match(Keyword.NATIVE)) {
            modifiers.add(tokens.get(currentIndex - 1));
        }

        if (!match(Keyword.DEF)) {
            addDiagnostic(String.format("mismatched input %s expecting 'def'", tokens.get(currentIndex)), tokens.get(currentIndex).span());
            ranges.add(tokens.get(currentIndex++).span());
            return null;
        }

        SyntaxNode def = createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1));

        Token identifier;
        if (match(Keyword.THIS)) {
            identifier = tokens.get(currentIndex - 1);
        } else {
            if (match(SyntaxKind.IDENTIFIER)) {
                identifier = ((IdentifierToken) tokens.get(currentIndex - 1)).withContextualKeyword(null);
            } else {
                addDiagnostic(String.format("mismatched input %s expecting IDENTIFIER", tokens.get(currentIndex)), tokens.get(currentIndex).span());
                ranges.add(tokens.get(currentIndex).span());
                return null;
            }
        }

        if (!match(Symbol.OPEN_PAREN)) {
            addDiagnostic(String.format("mismatched input %s expecting '('", tokens.get(currentIndex)), tokens.get(currentIndex).span());
            ranges.add(tokens.get(currentIndex).span());
            return null;
        }
        SyntaxNode open = createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1));
        List<SyntaxNode> parameters = new ArrayList<>();
        if (!match(Symbol.CLOSE_PAREN)) {
            SyntaxNode param = parseParam();
            if (param != null) {
                parameters.add(param);
            }
            while (match(Symbol.COMMA)) {
                parameters.add(createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1)));
                param = parseParam();
                if (param != null) {
                    parameters.add(param);
                }
            }
            if (!match(Symbol.CLOSE_PAREN)) {
                addDiagnostic(String.format("mismatched input %s expecting ')'", tokens.get(currentIndex)), tokens.get(currentIndex).span());
                ranges.add(tokens.get(currentIndex).span());
                return null;
            }
        }
        SyntaxNode close = createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1));

        SyntaxNode returnType = null;
        SyntaxNode colon = null;
        if (match(Symbol.COLON)) {
            colon = createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1));
            returnType = parseTypeName();
        }

        Token indent = tokens.get(currentIndex);
        SyntaxNode statementBlock = parseStatementBlock();
        Token dedent = tokens.get(currentIndex - 1);

        return createNode(SyntaxKind.FUNCTION_DEFINITION, Arrays.asList(
                modifiers.isEmpty() ? null : createNode(SyntaxKind.LIST, modifiers.stream().map(mod -> createTerminalNode(SyntaxKind.IDENTIFIER, mod)).toList()),
                def,
                createTerminalNode(SyntaxKind.IDENTIFIER, identifier),
                open,
                parameters.isEmpty() ? null : createNode(SyntaxKind.SEPARATED_LIST, parameters),
                close,
                colon,
                returnType,
                statementBlock != null ? createTerminalNode(SyntaxKind.INDENT, indent) : null,
                statementBlock,
                statementBlock != null ? createTerminalNode(SyntaxKind.IDENTIFIER, dedent) : null));
    }

    private SyntaxNode parseTypeParams() {
        List<SyntaxNode> typeParams = new ArrayList<>();
        SyntaxNode typeParam = parseTypeParam();
        if (typeParam != null) {
            typeParams.add(typeParam);
        }
        while (match(Symbol.COMMA)) {
            typeParams.add(createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1)));
            typeParam = parseTypeParam();
            if (typeParam != null) {
                typeParams.add(typeParam);
            }
        }

        if (!match(Symbol.GREATER_THAN)) {
            return null;
        }

        return typeParams.isEmpty() ? null : createNode(SyntaxKind.SEPARATED_LIST, typeParams);
    }

    private SyntaxNode parseTypeParam() {
        Token identifier;
        if (match(SyntaxKind.IDENTIFIER)) {
            identifier = ((IdentifierToken) tokens.get(currentIndex - 1)).withContextualKeyword(null);
        } else {
            addDiagnostic(String.format("mismatched input %s expecting IDENTIFIER", tokens.get(currentIndex)), tokens.get(currentIndex).span());
            ranges.add(tokens.get(currentIndex).span());
            return null;
        }

        SyntaxNode typeBound = null;
        if (match(Symbol.BOUND)) {
            typeBound = parseTypeBound();
        }

        return createNode(SyntaxKind.TYPE_PARAMETER_DEFINITION, Arrays.asList(createTerminalNode(SyntaxKind.IDENTIFIER, identifier), typeBound));
    }

    private SyntaxNode parseParam() {
        // IDENTIFIER ':' type_name
        Token identifier;
        if (match(SyntaxKind.IDENTIFIER)) {
            identifier = ((IdentifierToken) tokens.get(currentIndex - 1)).withContextualKeyword(null);
        } else {
            addDiagnostic(String.format("mismatched input %s expecting IDENTIFIER", tokens.get(currentIndex)), tokens.get(currentIndex).span());
            ranges.add(tokens.get(currentIndex).span());
            return null;
        }
        if (!match(Symbol.COLON)) {
            addDiagnostic(String.format("mismatched input %s expecting ':'", tokens.get(currentIndex)), tokens.get(currentIndex).span());
            ranges.add(tokens.get(currentIndex).span());
            return null;
        }
        Token colon = tokens.get(currentIndex - 1);
        SyntaxNode typeName = parseTypeName();

        return createNode(SyntaxKind.PARAMETER_DEFINITION, Arrays.asList(createTerminalNode(SyntaxKind.IDENTIFIER, identifier), createTerminalNode(SyntaxKind.IDENTIFIER, colon), typeName));
    }

    private SyntaxNode parseTypeBound() {
        Token boundToken = tokens.get(currentIndex - 1);
        List<SyntaxNode> typeBounds = new ArrayList<>();
        SyntaxNode typeName = parseTypeName();
        if (typeName != null) {
            typeBounds.add(typeName);
        }
        while (match(Symbol.AMPERSAND)) {
            typeBounds.add(createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1)));
            typeName = parseTypeName();
            if (typeName != null) {
                typeBounds.add(typeName);
            }
        }

        return createNode(SyntaxKind.TYPE_BOUND, List.of(createTerminalNode(SyntaxKind.IDENTIFIER, boundToken), createNode(SyntaxKind.SEPARATED_LIST, typeBounds)));
    }

    private SyntaxNode parseVariableDefStmt() {
        currentIndex--;
        SyntaxNode variableDef = parseVariableDef();
        return createNode(SyntaxKind.VARIABLE_DEFINITION_STATEMENT, Collections.singletonList(variableDef));
    }

    private SyntaxNode parseExpressionOrAssignmentStmt() {
        //assignment_stmt:= primary'='expression
        //expression_stmt:= expression
        SyntaxNode expressionOrPrimary = parseExpression();
        if (expressionOrPrimary == null) {
            return null;
        }
        if (match(Symbol.EQUALS)) {
            Token equals = tokens.get(currentIndex - 1);
            SyntaxNode expression = parseExpression();

            return createNode(SyntaxKind.ASSIGNMENT_STATEMENT, Arrays.asList(expressionOrPrimary, createTerminalNode(SyntaxKind.IDENTIFIER, equals), expression));
        } else {
            return createNode(SyntaxKind.EXPRESSION_STATEMENT, List.of(expressionOrPrimary));
        }
    }

    private SyntaxNode parseReturnStmt() {
        Token ret = tokens.get(currentIndex - 1);
        SyntaxNode expression = null;
        if (!matchDedent() && isNotEnd()) {
            expression = parseExpression();
        }

        return createNode(SyntaxKind.RETURN_STATEMENT, Arrays.asList(createTerminalNode(SyntaxKind.IDENTIFIER, ret), expression));
    }

    private SyntaxNode parseBreakStmt() {
        return createTerminalNode(SyntaxKind.BREAK_STATEMENT, tokens.get(currentIndex - 1));
    }

    private SyntaxNode parseContinueStmt() {
        return createTerminalNode(SyntaxKind.CONTINUE_STATEMENT, tokens.get(currentIndex - 1));
    }

    private SyntaxNode parseIfStmt() {
        SyntaxNode ifNode = createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1));

        SyntaxNode condition = parseExpression();

        SyntaxNode indent1 = createTerminalNode(SyntaxKind.INDENT, tokens.get(currentIndex));
        SyntaxNode thenBlock = parseStatementBlock();
        SyntaxNode dedent1 = createTerminalNode(SyntaxKind.DEDENT, tokens.get(currentIndex - 1));

        SyntaxNode elseBlock = null;
        SyntaxNode indent2 = null;
        SyntaxNode dedent2 = null;
        SyntaxNode elseNode = null;
        if (match(Keyword.ELSE)) {
            elseNode = createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1));
            indent2 = createTerminalNode(SyntaxKind.INDENT, tokens.get(currentIndex));
            elseBlock = parseStatementBlock();
            dedent2 = createTerminalNode(SyntaxKind.DEDENT, tokens.get(currentIndex - 1));
        }

        return createNode(SyntaxKind.IF_STATEMENT, Arrays.asList(
                ifNode,
                condition,
                indent1,
                thenBlock,
                dedent1,
                elseNode,
                indent2,
                elseBlock,
                dedent2));
    }

    private SyntaxNode parseWhileStmt() {
        SyntaxNode condition = parseExpression();
        SyntaxNode body = parseStatementBlock();

        return createNode(SyntaxKind.WHILE_STATEMENT, Arrays.asList(condition, body));
    }

    private SyntaxNode parseForStmt() {
        Token forToken = tokens.get(currentIndex - 1);
        SyntaxNode primary = parsePrimary();
        if (!match(Keyword.IN)) {
            addDiagnostic(String.format("mismatched input %s expecting 'in'", tokens.get(currentIndex)), tokens.get(currentIndex).span());
            ranges.add(tokens.get(currentIndex).span());
            return null;
        }
        Token inToken = tokens.get(currentIndex - 1);
        SyntaxNode iterable = parseExpression();

        Token indent = tokens.get(currentIndex);
        SyntaxNode body = parseStatementBlock();
        Token dedent = tokens.get(currentIndex - 1);

        return createNode(SyntaxKind.FOR_STATEMENT, Arrays.asList(
                createTerminalNode(SyntaxKind.IDENTIFIER, forToken),
                primary,
                createTerminalNode(SyntaxKind.IDENTIFIER, inToken),
                iterable,
                createTerminalNode(SyntaxKind.IDENTIFIER, indent),
                body,
                createTerminalNode(SyntaxKind.IDENTIFIER, dedent)));
    }

    private SyntaxNode parsePrimary() {
        SyntaxNode primary = parseAtom();
        if (primary == null) {
            currentIndex++;
            return null;
        }
        while (true) {
            //  primary('.'IDENTIFIER|'('(expression(','expression)*)?')'|'['expression']')
            if (match(Symbol.DOT)) {
                // primary '.' IDENTIFIER
                Token dot = tokens.get(currentIndex - 1);
                Token identifier;
                if (match(SyntaxKind.IDENTIFIER)) {
                    identifier = ((IdentifierToken) tokens.get(currentIndex - 1)).withContextualKeyword(null);
                } else {
                    addDiagnostic(String.format("mismatched input %s expecting IDENTIFIER", tokens.get(currentIndex)), tokens.get(currentIndex).span());
                    ranges.add(tokens.get(currentIndex).span());
                    return null;
                }
                primary = createNode(SyntaxKind.MEMBER_ACCESS_EXPRESSION, Arrays.asList(primary, createTerminalNode(SyntaxKind.IDENTIFIER, dot), createTerminalNode(SyntaxKind.IDENTIFIER, identifier)));
            } else if (match(Symbol.OPEN_PAREN)) {
                // primary '(' (expression (',' expression)*)? ')'
                Token open = tokens.get(currentIndex - 1);
                List<SyntaxNode> arguments = new ArrayList<>();
                if (!match(Symbol.CLOSE_PAREN)) {
                    SyntaxNode expression = parseExpression();
                    if (expression != null) {
                        arguments.add(expression);
                    }
                    while (match(Symbol.COMMA)) {
                        arguments.add(createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1)));
                        expression = parseExpression();
                        if (expression != null) {
                            arguments.add(expression);
                        }
                    }
                    if (!match(Symbol.CLOSE_PAREN)) {
                        addDiagnostic(String.format("mismatched input %s expecting ')'", tokens.get(currentIndex)), tokens.get(currentIndex).span());
                        ranges.add(tokens.get(currentIndex).span());
                        return null;
                    }
                }
                Token close = tokens.get(currentIndex - 1);
                primary = createNode(SyntaxKind.INVOCATION_EXPRESSION, Arrays.asList(
                        primary,
                        createTerminalNode(SyntaxKind.IDENTIFIER, open),
                        arguments.isEmpty() ? null : createNode(SyntaxKind.SEPARATED_LIST, arguments),
                        createTerminalNode(SyntaxKind.IDENTIFIER, close)));
            } else if (match(Symbol.OPEN_BRACKET)) {
                // primary '[' expression ']'
                Token open = tokens.get(currentIndex - 1);
                SyntaxNode indexExpression = parseExpression();
                if (!match(Symbol.CLOSE_BRACKET)) {
                    addDiagnostic(String.format("mismatched input %s expecting ']'", tokens.get(currentIndex)), tokens.get(currentIndex).span());
                    ranges.add(tokens.get(currentIndex).span());
                    return null;
                }
                Token close = tokens.get(currentIndex - 1);
                primary = createNode(SyntaxKind.INDEX_EXPRESSION, Arrays.asList(
                        primary,
                        createTerminalNode(SyntaxKind.IDENTIFIER, open),
                        indexExpression,
                        createTerminalNode(SyntaxKind.IDENTIFIER, close)));
            } else {
                break;
            }
        }

        return primary;
    }

    private SyntaxNode parseAtom() {
        if (match(Keyword.THIS)) {
            return createNode(SyntaxKind.THIS_EXPRESSION, List.of(createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1))));
        } else if (match(Keyword.SUPER)) {
            return createNode(SyntaxKind.SUPER_EXPRESSION, List.of(createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex - 1))));
        } else if (tokens.get(currentIndex) instanceof BooleanLiteralToken booleanToken) {
            if (booleanToken.value) {
                return createNode(SyntaxKind.TRUE_LITERAL_EXPRESSION, List.of(createTerminalNode(SyntaxKind.BOOLEAN, tokens.get(currentIndex++))));
            }
            return createNode(SyntaxKind.FALSE_LITERAL_EXPRESSION, List.of(createTerminalNode(SyntaxKind.BOOLEAN, tokens.get(currentIndex++))));
        } else if (match(SyntaxKind.INTEGER)) {
            return createNode(SyntaxKind.INTEGER_LITERAL_EXPRESSION, List.of(createTerminalNode(SyntaxKind.INTEGER, tokens.get(currentIndex - 1))));
        } else if (match(SyntaxKind.STRING)) {
            return createNode(SyntaxKind.STRING_LITERAL_EXPRESSION, List.of(createTerminalNode(SyntaxKind.STRING, tokens.get(currentIndex - 1))));
        } else if (match(SyntaxKind.RUNE)) {
            return createNode(SyntaxKind.RUNE_LITERAL_EXPRESSION, List.of(createTerminalNode(SyntaxKind.RUNE, tokens.get(currentIndex - 1))));
        } else if (match(Symbol.OPEN_PAREN)) {
            Token open = tokens.get(currentIndex - 1);
            SyntaxNode expression = null;
            if (!match(Symbol.CLOSE_PAREN)) {
                expression = parseExpression();
                if (!match(Symbol.CLOSE_PAREN)) {
                    addDiagnostic(String.format("mismatched input %s expecting ')'", tokens.get(currentIndex)), tokens.get(currentIndex).span());
                    ranges.add(tokens.get(currentIndex).span());
                    return null;
                }
            }
            Token close = tokens.get(currentIndex - 1);
            return createNode(SyntaxKind.PARENTHESIZED_EXPRESSION, Arrays.asList(
                    createTerminalNode(SyntaxKind.IDENTIFIER, open),
                    expression,
                    createTerminalNode(SyntaxKind.IDENTIFIER, close)));
        } else {
            return parseTypeName();
        }
    }

    private SyntaxNode parseExpression() {
        return parseBinaryExpression(12);
    }

    private SyntaxNode parseBinaryExpression(int precedence) {
        SyntaxNode left = parseUnaryExpression();

        while (isNotEnd() && isBinaryOperator()) {
            int operatorPrecedence = getOperatorPrecedence();
            if (operatorPrecedence > precedence) {
                break;
            }

            Token operatorToken = tokens.get(currentIndex++);

            SyntaxNode right;
            if (operatorToken.toSyntaxKind() == Keyword.IS) {
                SyntaxNode isExpression1 = parseTypeName();
                SyntaxNode isExpression2 = null;
                if (tokens.get(currentIndex).toSyntaxKind() == SyntaxKind.IDENTIFIER) {
                    isExpression2 = createTerminalNode(SyntaxKind.IDENTIFIER, tokens.get(currentIndex++));
                }
                right = createNode(SyntaxKind.IS_EXPRESSION, Arrays.asList(isExpression1, isExpression2));
            } else {
                right = parseBinaryExpression(operatorPrecedence);
            }
            left = createBinaryExpressionNode(operatorToken, left, right);
        }

        return left;
    }

    private SyntaxNode parseUnaryExpression() {
        if (match(Symbol.PLUS) || match(Symbol.MINUS) || match(Symbol.EXCLAMATION) || match(Symbol.TILDE)) {
            Token operatorToken = tokens.get(currentIndex - 1);
            SyntaxNode operand = parseUnaryExpression();
            SyntaxKind kind = switch (operatorToken.toSyntaxKind()) {
                case Symbol.PLUS -> SyntaxKind.UNARY_PLUS_EXPRESSION;
                case Symbol.MINUS -> SyntaxKind.UNARY_MINUS_EXPRESSION;
                case Symbol.EXCLAMATION -> SyntaxKind.LOGICAL_NOT_EXPRESSION;
                case Symbol.TILDE -> SyntaxKind.BITWISE_NOT_EXPRESSION;
                default -> SyntaxKind.BAD;
            };
            return createNode(kind, List.of(createTerminalNode(SyntaxKind.IDENTIFIER, operatorToken), operand));
        }

        return parsePrimary();
    }

    private int getOperatorPrecedence() {
        if (isNotEnd()) {
            Token currentToken = tokens.get(currentIndex);
            if (currentToken instanceof SymbolToken) {
                Symbol symbol = ((SymbolToken) currentToken).symbol;
                return switch (symbol) {
                    case PLUS, MINUS -> 3;
                    case ASTERISK, SLASH, PERCENT -> 2;
                    case AMPERSAND, BAR, CARET -> 5;
                    case AMPERSAND_AMPERSAND, BAR_BAR -> 10;
                    case EQUALS_EQUALS, EXCLAMATION_EQUALS, LESS_THAN, GREATER_THAN, LESS_THAN_EQUALS,
                         GREATER_THAN_EQUALS -> 8;
                    case LESS_THAN_LESS_THAN, GREATER_THAN_GREATER_THAN -> 4;
                    default -> -1;
                };
            } else if (currentToken.toSyntaxKind() == Keyword.IS) {
                return 8;
            }
        }
        return -1;
    }

    private boolean match(SyntaxKind kind) {
        if (isNotEnd() && tokens.get(currentIndex).toSyntaxKind() == kind) {
            currentIndex++;
            return true;
        }
        return false;
    }

    private boolean match(Keyword keyword) {
        if (isNotEnd()) {
            Token currentToken = tokens.get(currentIndex);
            if (currentToken instanceof KeywordToken keywordToken) {
                if (keywordToken.keyword == keyword) {
                    currentIndex++;
                    return true;
                }
            }
        }
        return false;
    }

    private boolean match(Symbol symbol) {
        if (isNotEnd()) {
            Token token = tokens.get(currentIndex);
            if (token instanceof SymbolToken symbolToken) {
                if (symbolToken.symbol == symbol) {
                    currentIndex++;
                    return true;
                } else if (symbolToken.symbol == Symbol.GREATER_THAN_GREATER_THAN && symbol == Symbol.GREATER_THAN) {
                    tokens.remove(currentIndex);
                    tokens.add(currentIndex, new SymbolToken(token.span().start() + 1, token.end, 0, token.trailingTriviaLength, Symbol.GREATER_THAN));
                    tokens.add(currentIndex, new SymbolToken(token.start, token.span().end() - 1, token.leadingTriviaLength, 0, Symbol.GREATER_THAN));
                    currentIndex++;
                    return true;
                }
            }
        }
        return false;
    }

    private boolean matchContextualKeyword(Keyword keyword) {
        if (isNotEnd()) {
            Token currentToken = tokens.get(currentIndex);
            if (currentToken instanceof IdentifierToken identifierToken) {
                if (identifierToken.contextualKeyword == keyword) {
                    currentIndex++;
                    return true;
                }
            }
        }
        return false;
    }

    private boolean matchIndent() {
        if (isNotEnd()) {
            Token currentToken = tokens.get(currentIndex);
            if (currentToken instanceof IndentationToken && ((IndentationToken) currentToken).isIndent()) {
                currentIndex++;
                return true;
            }
        }
        return false;
    }

    private boolean matchDedent() {
        if (isNotEnd()) {
            Token currentToken = tokens.get(currentIndex);
            if (currentToken instanceof IndentationToken && ((IndentationToken) currentToken).isDedent()) {
                currentIndex++;
                return true;
            }
        }
        return false;
    }


    private boolean isNotEnd() {
        return currentIndex < tokens.size();
    }

    private boolean isBinaryOperator() {
        if (isNotEnd()) {
            Token currentToken = tokens.get(currentIndex);
            return currentToken.toSyntaxKind() == Keyword.IS || (currentToken instanceof SymbolToken symbolToken && switch (symbolToken.symbol) {
                case PLUS, MINUS, ASTERISK, SLASH, PERCENT, AMPERSAND, BAR, CARET, AMPERSAND_AMPERSAND, BAR_BAR,
                     EQUALS_EQUALS, EXCLAMATION_EQUALS, LESS_THAN, GREATER_THAN, LESS_THAN_EQUALS, GREATER_THAN_EQUALS,
                     LESS_THAN_LESS_THAN, GREATER_THAN_GREATER_THAN -> true;
                default -> false;
            });
        }
        return false;
    }


    private SyntaxNode createTerminalNode(SyntaxKind kind, Token token) {
        return new SyntaxNode() {
            @Override
            public AnySyntaxKind kind() {
                return kind;
            }

            @Override
            public int slotCount() {
                return 0;
            }

            @Override
            public SyntaxNode slot(int index) {
                return null;
            }

            @Override
            public Token token() {
                return token;
            }
        };
    }

    private SyntaxNode createNode(SyntaxKind kind, List<SyntaxNode> nodes) {
        return new SyntaxNode() {
            @Override
            public AnySyntaxKind kind() {
                return kind;
            }

            @Override
            public int slotCount() {
                return nodes.size();
            }

            @Override
            public SyntaxNode slot(int index) {
                return index < tokens.size() ? nodes.get(index) : null;
            }

            @Override
            public Token token() {
                return null;
            }
        };
    }

    private SyntaxNode createBinaryExpressionNode(Token operatorToken, SyntaxNode left, SyntaxNode right) {
        SyntaxKind kind = switch ((Symbol) operatorToken.toSyntaxKind()) {
            case PLUS -> SyntaxKind.ADD_EXPRESSION;
            case MINUS -> SyntaxKind.SUBTRACT_EXPRESSION;
            case ASTERISK -> SyntaxKind.MULTIPLY_EXPRESSION;
            case SLASH -> SyntaxKind.DIVIDE_EXPRESSION;
            case PERCENT -> SyntaxKind.MODULO_EXPRESSION;
            case AMPERSAND -> SyntaxKind.BITWISE_AND_EXPRESSION;
            case BAR -> SyntaxKind.BITWISE_OR_EXPRESSION;
            case CARET -> SyntaxKind.BITWISE_EXCLUSIVE_OR_EXPRESSION;
            case AMPERSAND_AMPERSAND -> SyntaxKind.LOGICAL_AND_EXPRESSION;
            case BAR_BAR -> SyntaxKind.LOGICAL_OR_EXPRESSION;
            case EQUALS_EQUALS -> SyntaxKind.EQUALS_EXPRESSION;
            case EXCLAMATION_EQUALS -> SyntaxKind.NOT_EQUALS_EXPRESSION;
            case GREATER_THAN -> SyntaxKind.GREATER_THAN_EXPRESSION;
            case GREATER_THAN_EQUALS -> SyntaxKind.GREATER_THAN_OR_EQUAL_EXPRESSION;
            case LESS_THAN -> SyntaxKind.LESS_THAN_EXPRESSION;
            case LESS_THAN_EQUALS -> SyntaxKind.LESS_THAN_OR_EQUAL_EXPRESSION;
            case LESS_THAN_LESS_THAN -> SyntaxKind.BITWISE_LEFT_SHIFT_EXPRESSION;
            case GREATER_THAN_GREATER_THAN -> SyntaxKind.BITWISE_RIGHT_SHIFT_EXPRESSION;
            case DOT, COLON, COMMA, EXCLAMATION, TILDE, OPEN_BRACKET, CLOSE_BRACKET, OPEN_PAREN, CLOSE_PAREN, EQUALS, QUESTION, BOUND -> SyntaxKind.BAD;
        };
        return createNode(kind, Arrays.asList(left, createTerminalNode(SyntaxKind.IDENTIFIER, operatorToken), right));
    }

    private void addDiagnostic(String message, TextSpan span) {
        Diagnostic diagnostic = new Diagnostic(new DiagnosticInfo(() -> message, null), span, null);
        diagnostics.add(diagnostic);
    }
}