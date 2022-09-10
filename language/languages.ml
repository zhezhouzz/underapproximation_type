include Ast

module Lemma = struct
  include Frontend.Lemma
  include Lemma
end

module UnderTypectx = struct
  include Frontend.Utypectx
  include UnderTypectx
end

module UT = struct
  include Frontend.Underty
  include UT
end

module Typedec = struct
  include Frontend.Typedec
  include Typedec
end

module Struc = struct
  include Frontend.Structure
  include Struc

  let prog_of_ocamlstruct = Frontend.Structure.client_of_ocamlstruct
end

module StrucNA = struct
  include StrucNA

  let prog_of_ocamlstruct = Frontend.Structure.client_of_ocamlstruct
  let layout code = Struc.layout @@ Trans.struc_nan_to_term code
end

module OT = struct
  include Frontend.Overty
  include OT
end
