'use strict';

exports.Mod = {
  Lazy: () => ({type: 'Mod.Lazy'}),
  Case: () => ({type: 'Mod.Case'}),
  Implicit: () => ({type: 'Mod.Implicit'}),
  Final: null, // nope
  Sealed: null // nope
};


exports.Lit = {
  Int: (value /* integer */) => ({
    type: 'Lit.Int',
    value: value
  }),
  Long: (value /* number */) => ({
    type: 'Lit.Long',
    value: value
  }),
  Boolean: (value /* boolean */) => ({
    type: 'Lit.Boolean',
    value: value
  })
};


const TypeName = (value /*: string */) => ({
  type: 'Type.Name',
  value: value
});

exports.Type = {
  Name: TypeName,
  Tuple: null, // nope
  Function: null // nope
};


const NameIndeterminate = (value /*: string */) => ({
  type: 'Name.Indeterminate',
  value: value
});

const NameAnonymous = () => ({
  type: 'Name.Anonymous',
  value: ''
});

exports.Name = {
  Indeterminate: NameIndeterminate,
  Anonymous: NameAnonymous
};


const TermName = (value /*: string */) => ({
  type: 'Term.Name',
  value: value
});

exports.Term = {
  Name: TermName,
  ApplyInfix: (lhs /*: {} */, name /*: string */, args /* Array<{}> */) => ({
    type: 'Term.ApplyInfix',
    lhs: lhs,
    op: TermName(name),
    args: args
  }),
  Select: (qual /*: {} */, name /*: {} */) => ({
    type: 'Term.Select',
    qual: qual,
    name: name
  }),
  Apply: (fun /*: {} */, args /*: Array<{}> */) => ({
    type: 'Term.Apply',
    fun: TermName(fun),
    args: args
  }),
  Match: null, // nope
  Tuple: (args /*: Array<{}> */) => ({
    type: 'Term.Tuple',
    args: args
  }),
  Block: (stats /*: Array<{}> */) => ({
    type: 'Term.Block',
    stats: stats
  }),
  For: null, // nope
  While: null, // nope
  If: null // nope
};


exports.Defn = {
  Val: (
    pats /*: Array<string> */,
    type /*: string */,
    rhs /*: {} */
  ) => ({
    type: 'Defn.Val',
    pats: pats.map(e => ({
      type: 'Pat.Var',
      name: TermName(e)
    })),
    decltpe: TypeName(type),
    rhs: rhs
  }),
  Type: null, // nope
  Def: (
    mods /*: Array<{}> */,
    name /*: string */,
    paramss /*: Array<Array<{}>>*/,
    decltpe /*: {} */,
    body /*: {} */
  ) => ({
    type: 'Defn.Def',
    mods: mods,
    name: TermName(name),
    paramss: paramss,
    decltpe: TermName(decltpe),
    body: body
  }),
  Class: (mods /* Array<{}> */, type /* string */, ctor) => ({
    type: 'Defn.Class',
    mods: mods,
    name: TypeName(type),
    tparams: [],
    ctor: ctor
  }),
  Trait: null, // nope
  Object: (mods, name /*: string */, stats /*: Array<{}> */) => ({
    type: 'Defn.Object',
    mods: mods,
    name: TermName(name),
    templ: {
      type: 'Template',
      self: {
        type: 'Self',
        name: NameAnonymous()
      },
      stats: stats
    }
  })
};


exports.Decl = {
  Type: null, // nope
  Val: null, // nope
  Def: null
};

exports.Ctor = {
  Primary: (mods /* Array<{}> */, name, paramss) => ({
    type: 'Ctor.Primary',
    mods: mods,
    name: {
      type: 'Name.Anonymous',
      value: name
    },
    paramss: paramss
  })
};

exports.Import = (importers /* Array<{}> */) => ({
  type: 'Import',
  importers: importers
});

exports.Importer = (ref /* string */, importees /* Array<{}> */) => ({
  type: 'Importer',
  ref: TermName(ref),
  importees: importees
});

exports.Importee = {
  Name: (value /*: string */) => ({
    type: 'Importee.Name',
    name: NameIndeterminate(value)
  }),
  Wildcard: () => ({
    type: 'Importee.Wildcard'
  })
};
