package myte.lexer

enum class TokenType {
    INTEGRAL_LITERAL {
        override fun toString(): String = "integral literal"
    },

    DECIMAL_LITERAL {
        override fun toString(): String = "decimal literal"
    },

    STRING_LITERAL {
        override fun toString(): String = "string literal"
    },

    IDENTIFIER {
        override fun toString(): String = "identifier"
    },

    TRUE {
        override fun toString(): String = "true"
    },

    FALSE {
        override fun toString(): String = "false"
    },

    PLUS {
        override fun toString(): String = "+"
    },

    MINUS {
        override fun toString(): String = "-"
    },

    ASTERISK {
        override fun toString(): String = "*"
    },

    PERCENT {
        override fun toString(): String = "%"
    },

    FORWARD_SLASH {
        override fun toString(): String = "/"
    },

    CARET {
        override fun toString(): String = "^"
    },

    EQUALS {
        override fun toString(): String = "="
    },

    DOUBLE_EQUALS {
        override fun toString(): String = "=="
    },

    LESS_THAN {
        override fun toString(): String = "<"
    },

    LESS_THAN_OR_EQUAL {
        override fun toString(): String = "<="
    },

    GREATER_THAN {
        override fun toString(): String = ">"
    },

    GREATER_THAN_OR_EQUAL {
        override fun toString(): String = ">="
    },

    NOT_EQUALS {
        override fun toString(): String = "!="
    },

    LOGICAL_NOT {
        override fun toString(): String = "!"
    },

    LOGICAL_AND {
        override fun toString(): String = "&&"
    },

    LOGICAL_OR {
        override fun toString(): String = "||"
    },

    LEFT_PAREN {
        override fun toString(): String = "("
    },

    RIGHT_PAREN {
        override fun toString(): String = ")"
    },

    LEFT_BRACE {
        override fun toString(): String = "{"
    },

    RIGHT_BRACE {
        override fun toString(): String = "}"
    },

    LEFT_BRACKET {
        override fun toString(): String = "["
    },

    RIGHT_BRACKET {
        override fun toString(): String = "]"
    },

    LEFT_MAP_LITERAL {
        override fun toString(): String = "[|"
    },

    RIGHT_MAP_LITERAL {
        override fun toString(): String = "|]"
    },

    LEFT_SET_LITERAL {
        override fun toString(): String = "{|"
    },

    RIGHT_SET_LITERAL {
        override fun toString(): String = "|}"
    },

    QUOTES {
        override fun toString(): String = "\""
    },

    PERIOD {
        override fun toString(): String = "."
    },

    COMMA {
        override fun toString(): String = ","
    },

    COLON {
        override fun toString(): String = ":"
    },

    ARROW {
        override fun toString(): String = "->"
    },

    PIPE {
        override fun toString(): String = "|"
    },

    SCOPE {
        override fun toString(): String = "::"
    },

    TYPE {
        override fun toString(): String = "type"
    },

    LET {
        override fun toString(): String = "let"
    },

    CONST {
        override fun toString(): String = "const"
    },

    DEF {
        override fun toString(): String = "def"
    },

    FUN {
        override fun toString(): String = "fun"
    },

    IF {
        override fun toString(): String = "if"
    },

    ELSE {
        override fun toString(): String = "else"
    },

    WHILE {
        override fun toString(): String = "while"
    },

    DO {
        override fun toString(): String = "do"
    },

    FOR {
        override fun toString(): String = "for"
    },

    FOR_EACH {
        override fun toString(): String = "forEach"
    },

    IN {
        override fun toString(): String = "in"
    },

    MATCH {
        override fun toString(): String = "match"
    },

    WHEN {
        override fun toString(): String = "when"
    },

    RETURN {
        override fun toString(): String = "return"
    },

    BREAK {
        override fun toString(): String = "break"
    },

    CONTINUE {
        override fun toString(): String = "continue"
    },

    UNIT {
        override fun toString(): String = "unit"
    },

    BOOL {
        override fun toString(): String = "bool"
    },

    STRING_TYPE {
        override fun toString(): String = "string"
    },

    BYTE {
        override fun toString(): String = "byte"
    },

    INT {
        override fun toString(): String = "int"
    },

    FLOAT {
        override fun toString(): String = "float"
    },

    DOUBLE {
        override fun toString(): String = "double"
    },

    VEC {
        override fun toString(): String = "vec"
    },

    MAP {
        override fun toString(): String = "map"
    },

    SET {
        override fun toString(): String = "set"
    },

    PACKAGE {
        override fun toString(): String = "package"
    },

    IMPORT {
        override fun toString(): String = "import"
    },

    AS {
        override fun toString(): String = "as"
    },

    IMPLEMENT {
        override fun toString(): String = "implement"
    },

    EXTENDS {
        override fun toString(): String = "extends"
    },

    TRAIT {
        override fun toString(): String = "trait"
    },

    SIG {
        override fun toString(): String = "sig"
    },

    STATIC {
        override fun toString(): String = "static"
    },

    MUT {
        override fun toString(): String = "mut"
    },

    BUILTIN {
        override fun toString(): String = "__builtin"
    },

    NEW_LINE {
        override fun toString(): String = "\\n"
    }
}
