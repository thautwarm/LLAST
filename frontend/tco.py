import bytecode
import types


def scheduling(application):
    gen_ty = types.GeneratorType
    coroutines = [application.send]
    append = coroutines.append
    pop = coroutines.pop
    last = None
    while coroutines:
        end = coroutines[-1]
        try:
            f, *args = end(last)
            result = f(*args)
            last = append(result.send) if isinstance(result,
                                                     gen_ty) else result
            # if true: tail call in recursive func
            # if false: not tail call in recursive func
        except StopIteration as e:
            # return of recursive func
            pop()
            last = e.value
    return last


def sugar(f):
    code = bytecode.Bytecode.from_code(f.__code__)
    n = len(code)
    for idx in range(n):
        each = code[idx]
        if hasattr(each, 'name') and each.name == 'YIELD_VALUE':
            idx = idx - 1
            each = code[idx]
            assert hasattr(each, 'name') and each.name == 'CALL_FUNCTION'
            arg_num = each.arg
            each.name = 'BUILD_TUPLE'
            each.arg = arg_num + 1

    return types.FunctionType(code.to_code(), f.__globals__, f.__name__,
                              f.__defaults__, f.__closure__)
