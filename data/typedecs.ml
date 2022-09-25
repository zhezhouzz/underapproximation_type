(* The datatype constructor should use the lower case instead of the first char *)
type 'a leftisthp =
  | Lhpleaf
  | Lhpnode of int * 'a * 'a leftisthp * 'a leftisthp

type 'a pairinghp = Phpleaf | Phpnode of 'a * 'a pairinghp list
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
type 'a binomialhp = Bhpleaf | Bhpnode of int * 'a * 'a binomialhp list
type 'a rbset = Rbsleaf | Rbsnode of bool * 'a rbset * 'a * 'a rbset
type 'a skewhp = Shpnode of int * 'a * 'a list * 'a skewhp list
type 'a splayhp = Sphpleaf | Sphpnode of 'a splayhp * 'a * 'a splayhp
type 'a unbset = Usleaf | Usnode of 'a unbset * 'a * 'a unbset
