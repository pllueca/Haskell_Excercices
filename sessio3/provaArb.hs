;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

let n4 = fulla 4
let n5 = fulla 5
let n3 = Node 3 Empty n5
let n2 = Node 2 n4 Empty
let n1 = Node 1 n2 n3

preOrder n1 -> [1,2,4,3,5]
inOrder n1  -> [4,2,1,3,5]