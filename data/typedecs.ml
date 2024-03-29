(* The datatype constructor should use the lower case instead of the first char *)
type 'a pairinghp = Phpleaf | Phpnode of 'a * 'a pairinghp list
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
type 'a heap = Hempty | Hnode of 'a * 'a heap * 'a heap
type 'a set = Sempty | Snode of 'a * 'a set * 'a set
type 'a binomialhp = Bhpleaf | Bhpnode of int * 'a * 'a binomialhp list
type 'a rbtree = Rbtleaf | Rbtnode of bool * 'a rbtree * 'a * 'a rbtree
type 'a skewhp = Shpnode of int * 'a * 'a list * 'a skewhp list
type 'a splayhp = Sphpleaf | Sphpnode of 'a splayhp * 'a * 'a splayhp
type 'a unbset = Usleaf | Usnode of 'a * 'a unbset * 'a unbset
type 'a batchedq = Batchedq of 'a list * 'a list
type 'a lazyty = Lazyty of 'a
type 'a stream = Streamnil | Streamlazycons of 'a * 'a stream lazyty
type 'a bankersq = Bankersq of int * 'a stream * int * 'a stream

type 'a leftisthp =
  | Lhpleaf
  | Lhpnode of 'a * 'a leftisthp * 'a leftisthp * int
