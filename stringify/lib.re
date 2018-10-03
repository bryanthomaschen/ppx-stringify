open Location;

open Parsetree;

open Asttypes;

open Ast_helper;
let dlp: Lexing.position = {
  pos_fname: "dummyPosition",
  pos_lnum: 0,
  pos_cnum: 0,
  pos_bol: 0,
};

let stringConstant = const => Ast_helper.Exp.constant(Const_string(const, Some("大家好")));
let dummyLoc = {loc_ghost: false, loc_start: dlp, loc_end: dlp};
let lid = (s) => Location.mkloc(s, dummyLoc);
let makeLid = str => {
  let x = Longident.parse(str);
  let z = Location.mkloc(x, dummyLoc);
  z;
};
let fail = (loc, txt) => raise(Location.Error(Location.error(~loc, txt)));
let process_let = (contents, loc) => {
  open Parsetree;
  let bindings =
    switch (contents) {
    | PStr([{pstr_desc: Pstr_value(Nonrecursive, bindings), pstr_loc}]) => bindings
    | _ => fail(loc, "guard must contain a nonrecursive let binding")
    };

  let binding =
    switch (bindings) {
    | [binding] => binding
    | _ => fail(loc, "only one binding supported atm")
    };
  (binding.pvb_pat, binding.pvb_expr);
};

let getExpr = (contents, loc) =>
  Parsetree.(
    switch (contents) {
    | PStr([{pstr_desc: Pstr_eval(expr, _)}]) => expr
    | _ => fail(loc, "@else must contain an expression")
    }
  );

let mapper = _args =>
  Parsetree.{
    ...Ast_mapper.default_mapper,
    expr: (mapper, expr) =>
      switch (expr.pexp_desc) {
      | Pexp_sequence(
          {
            pexp_desc: Pexp_extension(({txt: "guard"}, contents)),
            pexp_loc,
            pexp_attributes,
          },
          next,
        ) =>
        let (pat, expr) = process_let(contents, pexp_loc);
        switch (pexp_attributes) {
        | [({txt: "else"}, else_contents)] =>
          switch%expr ([%e expr]) {
          | [%p pat] => %e
                        mapper.expr(mapper, next)
          | _ => %e
                 mapper.expr(mapper, [%expr Some([%e (stringConstant("hello"))])])
          }
        | _ => fail(pexp_loc, "No @else attribute given")
        };

      | _ => Ast_mapper.default_mapper.expr(mapper, expr)
      },
  };
