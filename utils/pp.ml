open Sugar

let mk_colored color str = spf " @{<%s>%s@}" color str
let mk_red = mk_colored "red"
let mk_blue = mk_colored "blue"
let printf = Ocolor_format.printf
