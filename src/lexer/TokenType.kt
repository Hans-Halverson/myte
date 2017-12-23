package myte.lexer

enum class TokenType {
    INT_LITERAL {
        override fun toString(): String = "int literal"
    },

    FLOAT_LITERAL {
        override fun toString(): String = "float literal"
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

    QUOTES {
        override fun toString(): String = "\""
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

    LET {
        override fun toString(): String = "let"
    },

    CONST {
        override fun toString(): String = "const"
    },

    DEF {
        override fun toString(): String = "def"
    },

    NUM {
        override fun toString(): String = "num"
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

    INT {
        override fun toString(): String = "int"
    },

    FLOAT {
        override fun toString(): String = "float"
    },

    LIST {
        override fun toString(): String = "list"
    },

    NEW_LINE {
        override fun toString(): String = "\\n"
    }
}
