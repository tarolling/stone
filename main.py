from src.compiler import compile_code
import argparse


def main():
    parser = argparse.ArgumentParser(prog="newlang")
    parser.add_argument("--target", choices=["arm", "x86_64"], default="arm")

    args = vars(parser.parse_args())

    source_code = ""

    with open("test/simple.pi") as fp:
        source_code = "".join(fp.readlines())

    # Compile to ARM assembly
    arm_code = compile_code(source_code, args["target"])
    print(arm_code)

    # write to file
    with open("simple.s", "+w") as fp:
        fp.write(arm_code)


if __name__ == "__main__":
    main()
