
from Redy.Tools.PathLib import Path
import os

for each in Path("ir-snippets").list_dir(lambda x: x.endswith('.ll')):
    print(f"run <{each}> :")
    os.system(f"llc-6.0 {str(each)!r} -o asm.s")
    os.system(f"gcc -C asm.s -o out")
    os.system(f"./out")
    os.system("echo status $?")
    Path("./out").delete()
Path("asm.s").delete()
