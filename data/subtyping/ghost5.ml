let[@assert] rty1 = (v == 5 : [%v: int]) [@over]
let[@assert] rty2 = (v == 5 || v == 6 : [%v: int]) [@under]
