from main import Lexer, Lexer_tokenize, Token__repr__


def test_lexer():
    with open("test/simple.pi") as fp:
        source_code = "".join(fp.readlines())

    Lexer(source_code)
    tokens = Lexer_tokenize()
    for token in tokens:
        print(Token__repr__(token))

    converted_tokens = convert_tokens(tokens)
    print("-------------------CONVERTED TOKENS-------------------")
    print(converted_tokens)

    assert source_code == convert_tokens(tokens)


def convert_tokens(tokens):
    code = ""
    indent_level = 0

    for i in range(len(tokens)):
        token = tokens[i]
        match token["type"]:
            case "INDENT":
                indent_level += 1
                if i < len(tokens) and tokens[i + 1]["type"] != "INDENT":
                    for _ in range(indent_level):
                        code += "    "
            case "DEDENT":
                indent_level -= 1
                if i < len(tokens) and tokens[i + 1]["type"] != "DEDENT":
                    for _ in range(indent_level):
                        code += "    "
            case "DEF":
                code += "def "
            case "RETURN":
                code += "return "
            case "IF":
                code += "if "
            case "PLUS":
                code += " + "
            case "EQUAL":
                code += " = "
            case "EQEQUAL":
                code += " == "
            case "IDENTIFIER":
                code += token["value"]
            case "STRING":
                code += f"\"{token['value']}\""
            case "COMMA":
                code += ", "
            case "NEWLINE":
                code += "\n"
                if (
                    i < len(tokens)
                    and tokens[i + 1]["type"] != "NEWLINE"
                    and tokens[i + 1]["type"] != "DEDENT"
                    and tokens[i + 1]["type"] != "INDENT"
                ):
                    for _ in range(indent_level):
                        code += "    "
            case "EOF":
                break
            case _:
                code += str(token["value"])

    return code
