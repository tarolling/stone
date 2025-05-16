from src.compiler import compile_code


def main():
    source_code = """
def fibonacci(n):
    if n <= 1:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)

def main():
    result = fibonacci(10)
    return result
"""

    with open("test/test.pi", "r") as fp:
        source_code = "".join(fp.readlines())

    # Compile to ARM assembly
    arm_code = compile_code(source_code, "arm")
    print(arm_code)

    # To compile to another architecture (when implemented)
    # x86_code = compile_code(source_code, "x86")
    # print(x86_code)


if __name__ == "__main__":
    main()
