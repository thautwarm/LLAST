from rbnf.easy import Language, build_language, build_parser, State
from Redy.Tools.PathLib import Path
import proc
ll = Language('ll')
ll.namespace.update(proc.__dict__)
build_language(Path('simple.rbnf').open('r').read(), ll, "simple.rbnf")
parse = build_parser(ll, use_parser='stmts')
x = ""
while x != "exit":
    x = input()
    print(parse(x).result.item[0].item[0])