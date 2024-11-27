import syspro.tm.lexer.*;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;

import static syspro.tm.Tasks.Lexer.registerSolution;

public class Main {
    public static void main(String[] args) {
        registerSolution(new LexerImpl());
    }
}

class LexerImpl implements Lexer {
    private int currentIndentationLevel;
    private int currentIndentationLength;
    private int triviaLength;
    int length;

    @Override
    public List<Token> lex(String input) {
        //System.out.println(input);
        List<Token> tokens = new ArrayList<>();
        length = input.codePointCount(0, input.length());
        int pos = 0;
        int firstTriviaLength = 0;
        triviaLength = 0;
        currentIndentationLevel = 0;
        currentIndentationLength = 0;

        while (pos < length) {
            int currentCodePoint = getCodePointAt(input, pos);

            if (currentCodePoint == '\n' || currentCodePoint == '\r') {
                pos = handleIndentation(input, pos, tokens, length);
            } else if (Character.isWhitespace(currentCodePoint)) {
                pos++;
                triviaLength++;
            } else if (currentCodePoint == '#') {
                while (pos < length && getCodePointAt(input, pos) != '\n' && getCodePointAt(input, pos) != '\r') {
                    pos = pos + 1;
                    triviaLength++;
                }
            } else {
                if (!attachTrivia(tokens)) {
                    firstTriviaLength = triviaLength;
                    triviaLength = 0;
                }
                if (Character.isLetter(currentCodePoint) || currentCodePoint == '_') {
                    pos = parseIdentifierOrKeyword(input, pos, tokens);
                } else if (Character.isDigit(currentCodePoint)) {
                    pos = parseNumericLiteral(input, pos, tokens);
                } else if (isSymbol(currentCodePoint)) {
                    pos = parseSymbol(input, pos, tokens);
                } else if (currentCodePoint == '\'') {
                    pos = parseRuneLiteral(input, pos, tokens);
                } else if (currentCodePoint == '"') {
                    pos = parseStringLiteral(input, pos, tokens);
                } else {
                    tokens.add(new BadToken(pos, pos, 0, 0));
                    pos++;
                }
                if (firstTriviaLength > 0) {
                    Token token = tokens.removeLast();
                    tokens.add(token.withLeadingTriviaLength(firstTriviaLength).withStart(token.start - firstTriviaLength));
                    firstTriviaLength = 0;
                }
            }
        }

        attachTrivia(tokens);


        while (currentIndentationLevel > 0) {
            tokens.add(new IndentationToken(pos, pos, 0, 0, -1));
            currentIndentationLevel--;
        }

        //System.out.println(tokens);
        return tokens;
    }

    boolean attachTrivia(List<Token> tokens) {
        if (triviaLength > 0) {
            boolean found = false;
            ListIterator<Token> iterator = tokens.listIterator(tokens.size());
            while (iterator.hasPrevious()) {
                Token token = iterator.previous();
                if (!(token instanceof IndentationToken)) {
                    int index = tokens.indexOf(token);
                    tokens.set(index ,token.withTrailingTriviaLength(triviaLength).withEnd(token.end + triviaLength));
                    triviaLength = 0;
                    found = true;
                    break;
                }
            }
            return found;
        }
        return true;
    }

    private int handleIndentation(String input, int pos, List<Token> tokens, int length) {
        int startPos = pos;
        int countSpaces = 0;
        int newIndentationLevel = 0;
        triviaLength++;

        if (pos < length && getCodePointAt(input, pos) == '\r' && input.codePointAt(input.offsetByCodePoints(0, pos + 1)) == '\n') {
            pos++;
            triviaLength++;
        }

        int posSpaces = pos + 1;

        while (posSpaces < length && (getCodePointAt(input, posSpaces) == ' ' || getCodePointAt(input, posSpaces) == '\t')) {
            countSpaces += (getCodePointAt(input, posSpaces) == ' ') ? 1 : 2;
            posSpaces++;
        }

        if (posSpaces < length && (getCodePointAt(input, posSpaces) == '\n' || getCodePointAt(input, posSpaces) == '\r' || countSpaces % 2 != 0)) {
            return pos + 1;
        }

        if (posSpaces < length && countSpaces != 0) {
            if (currentIndentationLevel == 0) {
                currentIndentationLength = countSpaces;
                newIndentationLevel = 1;
            } else if (countSpaces % currentIndentationLength == 0) {
                newIndentationLevel = countSpaces / currentIndentationLength;
            } else {
                return pos + 1;
            }
        }

        while (newIndentationLevel != currentIndentationLevel) {
            if (newIndentationLevel > currentIndentationLevel) {
                tokens.add(new IndentationToken(startPos, pos, 0, 0, 1));
                currentIndentationLevel++;
            } else {
                tokens.add(new IndentationToken(startPos, pos, 0, 0, -1));
                currentIndentationLevel--;
            }
        }

        return pos + 1;
    }

    private int parseIdentifierOrKeyword(String input, int pos, List<Token> tokens) {
        int start = pos;
        do {
            pos++;
        } while (pos < length && (Character.isLetterOrDigit(getCodePointAt(input, pos)) ||
                Character.getType(getCodePointAt(input, pos)) == Character.CONNECTOR_PUNCTUATION || // \p{Pc}
                Character.getType(getCodePointAt(input, pos)) == Character.NON_SPACING_MARK || // \p{Mn}
                Character.getType(getCodePointAt(input, pos)) == Character.COMBINING_SPACING_MARK || // \p{Mc}
                Character.getType(getCodePointAt(input, pos)) == Character.FORMAT || // \p{Cf}
                (getCodePointAt(input, pos) >= 0x10330 && getCodePointAt(input, pos) <= 0x1037A))); // Gothic and other scripts
        String identifier = input.substring(input.offsetByCodePoints(0, start), input.offsetByCodePoints(0, pos));

        if (identifier.equals("true") || identifier.equals("false")) {
            boolean value = Boolean.parseBoolean(identifier);
            tokens.add(new BooleanLiteralToken(start, pos - 1, 0, 0, value));
        } else {
            Keyword keyword = getKeyword(identifier);
            if (keyword != null) {
                if (!keyword.isContextual) {
                    tokens.add(new KeywordToken(start, pos - 1, 0, 0, keyword));
                } else {
                    tokens.add(new IdentifierToken(start, pos - 1, 0, 0, identifier, keyword));
                }
            } else {
                tokens.add(new IdentifierToken(start, pos - 1, 0, 0, identifier, null));
            }
        }
        return pos;
    }

    private int parseRuneLiteral(String input, int pos, List<Token> tokens) {
        int start = pos;
        pos++;
        int codePoint;

        if (pos < length && getCodePointAt(input, pos) == '\\') {
            pos++;
            if (pos >= length) {
                tokens.add(new BadToken(start, pos, 0, 0));
                return pos;
            }
            StringBuilder sb = new StringBuilder();
            pos = handleEscapeSequence(input, pos, sb, start, tokens);
            codePoint = sb.codePointAt(0);
        } else if (pos < length && getCodePointAt(input, pos) != '\'') {
            codePoint = getCodePointAt(input, pos);
            pos++;
        } else {
            tokens.add(new BadToken(start, pos, 0, 0));
            return pos;
        }

        if (pos < length && getCodePointAt(input, pos) == '\'') {
            tokens.add(new RuneLiteralToken(start, pos, 0, 0, codePoint));
            pos++;
        } else {
            tokens.add(new BadToken(start, pos, 0, 0));
        }
        return pos;
    }

    private int parseStringLiteral(String input, int pos, List<Token> tokens) {
        int start = pos;
        pos++;
        StringBuilder sb = new StringBuilder();

        while (pos < length && getCodePointAt(input, pos) != '"') {
            if (getCodePointAt(input, pos) == '\\') {
                pos++;
                if (pos >= length) {
                    tokens.add(new BadToken(start, pos, 0, 0));
                    return pos;
                }
                pos = handleEscapeSequence(input, pos, sb, start, tokens);
            } else {
                sb.appendCodePoint(getCodePointAt(input, pos));
                pos++;
            }
        }

        if (pos < length && getCodePointAt(input, pos) == '"') {
            tokens.add(new StringLiteralToken(start, pos, 0, 0, sb.toString()));
            pos++;
        } else {
            tokens.add(new BadToken(start, pos, 0, 0));
        }
        return pos;
    }


    private int parseNumericLiteral(String input, int pos, List<Token> tokens) {
        int start = pos;
        while (pos < length && Character.isDigit(getCodePointAt(input, pos))) {
            pos++;
        }

        String numberStr = input.substring(input.offsetByCodePoints(0, start), input.offsetByCodePoints(0, pos));
        long value = Long.parseLong(numberStr);

        BuiltInType type = BuiltInType.INT32;
        boolean hasTypeSuffix = false;

        if (pos < length && (getCodePointAt(input, pos) == 'i' || getCodePointAt(input, pos) == 'u')) {
            hasTypeSuffix = true;
            char suffixType = (char) getCodePointAt(input, pos);
            pos++;

            if (pos < length && getCodePointAt(input, pos) == '3' && getCodePointAt(input, pos + 1) == '2') {
                type = (suffixType == 'i') ? BuiltInType.INT32 : BuiltInType.UINT32;
                pos = input.offsetByCodePoints(pos, 2);
            } else if (pos < length && getCodePointAt(input, pos) == '6' && getCodePointAt(input, pos + 1) == '4') {
                type = (suffixType == 'i') ? BuiltInType.INT64 : BuiltInType.UINT64;
                pos = input.offsetByCodePoints(pos, 2);
            } else {
                tokens.add(new BadToken(start, pos, 0, 0));
                return pos;
            }
        }

        tokens.add(new IntegerLiteralToken(start, input.offsetByCodePoints(pos, -1), 0, 0, type, hasTypeSuffix, value));

        return pos;
    }

    private int parseSymbol(String input, int pos, List<Token> tokens) {
        if (pos + 1 < length) {
            Symbol symbol = getSymbol(input.substring(input.offsetByCodePoints(0, pos), input.offsetByCodePoints(0, pos + 2)));

            if (symbol != null) {
                tokens.add(new SymbolToken(pos, pos + 1, 0, 0, symbol));
                return pos + 2;
            }
        }

        Symbol symbol = getSymbol(input.substring(input.offsetByCodePoints(0, pos), input.offsetByCodePoints(0, pos + 1)));

        if (symbol != null) {
            tokens.add(new SymbolToken(pos, pos, 0, 0, symbol));
        } else {
            tokens.add(new BadToken(pos, pos, 0, 0));
        }
        return pos + 1;
    }

    private int handleEscapeSequence(String input, int pos, StringBuilder sb, int start, List<Token> tokens) {
        switch (getCodePointAt(input, pos)) {
            case 'U':
                pos++;
                if (pos + 4 < length && getCodePointAt(input, pos) == '+') {
                    pos++;
                    StringBuilder unicodeHex = new StringBuilder();
                    while (pos < length && ((getCodePointAt(input, pos) >= '0' && getCodePointAt(input, pos) <= '9') || (getCodePointAt(input, pos) >= 'A' && getCodePointAt(input, pos) <= 'F'))) {
                        unicodeHex.append((char) getCodePointAt(input, pos));
                        pos++;
                    }
                    if (!unicodeHex.isEmpty()) {
                        int unicodeValue = Integer.parseInt(unicodeHex.toString(), 16);
                        sb.appendCodePoint(unicodeValue);
                        return pos;
                    }
                }
                tokens.add(new BadToken(start, pos, 0, 0));
                break;
            case '"': case '\\': case '/':
                sb.appendCodePoint(getCodePointAt(input, pos));
                break;
            case 'b':
                sb.append('\b');
                break;
            case 'f':
                sb.append('\f');
                break;
            case 'n':
                sb.append('\n');
                break;
            case 'r':
                sb.append('\r');
                break;
            case 't':
                sb.append('\t');
                break;
            default:
                tokens.add(new BadToken(start, pos, 0, 0));
                return pos;
        }
        return pos + 1;
    }

    private boolean isSymbol(int codePoint) {
        return "+-*/%&|^!~<>=()[]{}:".indexOf(codePoint) != -1;
    }

    private Symbol getSymbol(String text) {
        for (Symbol symbol : Symbol.values()) {
            if (symbol.text.equals(text)) {
                return symbol;
            }
        }
        return null;
    }

    private Keyword getKeyword(String identifier) {
        try {
            return Keyword.valueOf(identifier.toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException e) {
            return null;
        }
    }

    int getCodePointAt(String input, int codePointIndex) {
        return input.codePointAt(input.offsetByCodePoints(0, codePointIndex));
    }
}
