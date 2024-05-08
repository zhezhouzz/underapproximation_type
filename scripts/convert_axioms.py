# coding=utf8
# the above tag defines encoding for this document and is for Python 2.x compatibility

import sys
import re

regex0 = r"\."
subst0 = ""
regex1 = r"Admitted"
subst1 = ""
regex2_0 = "\),"
subst2_0 = ","
regex2 = r"\(([a-zA-Y0-9~,\_<>='\:\/\\\s\-\~\(+]*)\)\%Z"
subst2 = "\g<1>"
regex3 = r"Lemma"
subst3 = "let[@axiom]"
regex4 = r"IL"
subst4 = "int list"
regex5 = r"\/\\"
subst5 = "&&"
regex6 = r"\\\/"
subst6 = "||"
regex7 = r"->"
subst7 = "#==>"
regex8 = r"~"
subst8 = "not"
regex9 = r"exists\s\(([a-z]+)\:([a-z\s]+),"
subst9 = "((\\g<1> [@exists]) : \\g<2>)"
regex10 = r"\):"
subst10 = ") ="
regex11 = r","
subst11 = " "

with open(sys.argv[1], 'r') as reader:
    result = reader.read()
    result = re.sub(regex0, subst0, result, 0, re.MULTILINE)
    result = re.sub(regex1, subst1, result, 0, re.MULTILINE)
    result = re.sub(regex2_0, subst2_0, result, 0, re.MULTILINE)
    result = re.sub(regex2, subst2, result, 0, re.MULTILINE)
    result = re.sub(regex3, subst3, result, 0, re.MULTILINE)
    result = re.sub(regex4, subst4, result, 0, re.MULTILINE)
    result = re.sub(regex5, subst5, result, 0, re.MULTILINE)
    result = re.sub(regex6, subst6, result, 0, re.MULTILINE)
    result = re.sub(regex7, subst7, result, 0, re.MULTILINE)
    result = re.sub(regex8, subst8, result, 0, re.MULTILINE)
    result = re.sub(regex9, subst9, result, 0, re.MULTILINE)
    result = re.sub(regex10, subst10, result, 0, re.MULTILINE)
    result = re.sub(regex11, subst11, result, 0, re.MULTILINE)

print (result)
