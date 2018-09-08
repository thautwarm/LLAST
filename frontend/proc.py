from Redy.ADT.Core import data
from Redy.ADT.traits import ConsInd
from rbnf.core.State import State
from tco import scheduling, sugar
globals()['NamedTuple'] = object


@data
class Proc(ConsInd):
    Indent: lambda it:...
    NoIndent: lambda it:...
    Predef: lambda it:...
    Combine: lambda a, b:...
    Ordered: lambda it:...

    def __add__(self, other):
        return Combine(self, other)


Indent = Proc.Indent
NoIndent = Proc.NoIndent
Predef = Proc.Predef
Combine = Proc.Combine
Ordered = Proc.Ordered


class List:
    @staticmethod
    def rev(lst):
        it = ()
        while lst != ():
            head, lst = lst
            it = (head, it)
        return it

    @staticmethod
    def append(la, lb):
        la = List.rev(la)
        while la != ():
            head, la = la
            lb = (head, lb)
        return lb

    @staticmethod
    def reduce(f, lst, init):
        while lst != ():
            head, lst = lst
            init = f(init, head)
        return init


@sugar
def to_code(self, indent=0, tp=((), ())):
    predef, ordered = tp
    tag = self[0]
    if tag is Indent:
        return (yield to_code(self[1], indent + 1, tp))
    elif tag is NoIndent:
        return (yield to_code(self[1], 0, tp))
    elif tag is Predef:
        predef_, ordered_ = yield to_code(self[1], indent)
        predef = predef if predef_ == () else List.append(predef, predef_)
        return List.append(ordered_, predef), ordered
    elif tag is Combine:
        _, a, b = self
        predef, ordered = yield to_code(a, indent, (predef, ordered))
        return (yield to_code(b, indent, (predef, ordered)))
    elif tag is Ordered:
        return predef, ("  " * indent + self[1], ordered)
    else:
        raise TypeError


def locate(filename, it, string):
    return f'{string} @ {{' + f"filename = {filename!r}; lineno = {it.lineno}; colno = {it.colno}" + '}'


def inc(state: State) -> str:
    if hasattr(state, 'inc'):
        it = state.inc
        state.inc += 1
    else:
        it = state.inc = 0
    return 'if.%d' % it


def concat(*args):
    def mapp():
        head, *tail = args
        if isinstance(head, str):
            yield Ordered(head)
        else:
            yield head
        for each in tail:
            if isinstance(each, str):
                yield Indent(Ordered(each))
            else:
                yield Indent(each)
    head, *tail = mapp()
    return sum(tail, head)


def proc_to_str(proc: Proc):
    def line_add(a, b):
        return a + '\n' + b

    predef, ordered = scheduling(to_code(proc))

    return List.reduce(line_add, List.rev(predef), "") + '\n' + List.reduce(
        line_add, List.rev(ordered), "")
