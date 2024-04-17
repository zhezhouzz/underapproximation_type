Atom.vo Atom.glob Atom.v.beautified Atom.required_vo: Atom.v 
Atom.vio: Atom.v 
Atom.vos Atom.vok Atom.required_vos: Atom.v 
Tactics.vo Tactics.glob Tactics.v.beautified Tactics.required_vo: Tactics.v Atom.vo
Tactics.vio: Tactics.v Atom.vio
Tactics.vos Tactics.vok Tactics.required_vos: Tactics.v Atom.vos
CoreLang.vo CoreLang.glob CoreLang.v.beautified CoreLang.required_vo: CoreLang.v Atom.vo
CoreLang.vio: CoreLang.v Atom.vio
CoreLang.vos CoreLang.vok CoreLang.required_vos: CoreLang.v Atom.vos
NamelessTactics.vo NamelessTactics.glob NamelessTactics.v.beautified NamelessTactics.required_vo: NamelessTactics.v Atom.vo Tactics.vo CoreLang.vo
NamelessTactics.vio: NamelessTactics.v Atom.vio Tactics.vio CoreLang.vio
NamelessTactics.vos NamelessTactics.vok NamelessTactics.required_vos: NamelessTactics.v Atom.vos Tactics.vos CoreLang.vos
CoreLangProp.vo CoreLangProp.glob CoreLangProp.v.beautified CoreLangProp.required_vo: CoreLangProp.v Atom.vo Tactics.vo CoreLang.vo NamelessTactics.vo
CoreLangProp.vio: CoreLangProp.v Atom.vio Tactics.vio CoreLang.vio NamelessTactics.vio
CoreLangProp.vos CoreLangProp.vok CoreLangProp.required_vos: CoreLangProp.v Atom.vos Tactics.vos CoreLang.vos NamelessTactics.vos
