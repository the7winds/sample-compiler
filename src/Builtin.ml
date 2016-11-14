
let read (x::input') output = (x, input', output)

let write x input output = (0, input, output@[x])
