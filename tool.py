import re
import sys

regex = r": \[[\%|:|a-z|\ ]*\]. ="

test_str = ("  let _ = (true : [%v: int]) [@over] in\n"
	"  let (sizel : [%over: int]) = (v >= 0 : [%v: int]) [@over] in\n"
	"  let _ = (len v sizel : [%v: int leftisthp]) [@over] in\n"
	"  let (sizer : [%over: int]) = (v >= 0 : [%v: int]) [@over] in\n"
	"  let _ =\n"
	"    (fun (u : [%forall: int]) -> implies (0 <= u && u <= sizel) (len v u)\n"
	"      : [%v: int leftisthp])\n"
	"  in\n"
	"  let _ = (v == sizer : [%v: int]) [@over] in\n"
	"  (fun (u : [%forall: int]) -> implies (u == sizel + 1) (len v u)\n"
	"    : [%v: int leftisthp])\n"
	"    [@under]\n")

subst = ") ="

# You can manually specify the number of replacements by changing the 4th argument

if __name__ == '__main__':
    fname = sys.argv[1]
    with open(fname, "r") as f:
        res = f.read()
        res = re.sub(r": \[[\%|:|a-z|\ ]*\]. =", ") =", res, 0, re.MULTILINE)
        res = re.sub(r"dummy", "_", res, 0, re.MULTILINE)
        res = re.sub(r"\]\) in", "]) [@over] in", res, 0, re.MULTILINE)
    with open(fname, "w") as f:
        f.write(res)
