"use strict"

// module Lovefield.Query

function curry(fx) {
  // http://blog.carbonfive.com/2015/01/14/gettin-freaky-functional-wcurried-javascript/
  var arity = fx.length;

  return function f1() {
    var args = Array.prototype.slice.call(arguments, 0);
    if (args.length >= arity) {
      return fx.apply(null, args);
    } else {
      return function f2() {
        var args2 = Array.prototype.slice.call(arguments, 0);
        return f1.apply(null, args.concat(args2));
      }
    }
  };
}

exports.runQueryNative = function (db, froms, wheres, matchOnPrimExpr, error, success) {
  return function () {
    var schema = db.getSchema();

    var aliases = [];
    for (var i = 0; i < froms.length; i++) {
      var f = froms[i];
      aliases[f.alias] = schema.table(f.name).as('t' + f.alias);
    }

    var extractAttr = matchOnPrimExpr
        (curry(function (alias, name) { // AttrExpr, got it
          return aliases[alias][name];
        }))
        (function (_) { throw new Error("TernExpr is not AttrExpr") })
        (function (_) { throw new Error("BinExpr is not AttrExpr") })
        (function (_) { throw new Error("UnExpr is not AttrExpr") })
        (function (_) { throw new Error("ConstExpr is not AttrExpr") });

    var extractConst = matchOnPrimExpr
        (function (_) { throw new Error("AttrExpr is not ConstExpr") })
        (function (_) { throw new Error("TernExpr is not ConstExpr") })
        (function (_) { throw new Error("BinExpr is not ConstExpr") })
        (function (_) { throw new Error("UnExpr is not ConstExpr") })
        (function (literal) { return lf.bind(literal); });

    var extractConstAndExpr = matchOnPrimExpr
        (curry(function (alias, name) { // AttrExpr, got it
          return aliases[alias][name];
        }))
        (function (_) { throw new Error("TernExpr is not ConstExpr") })
        (function (_) { throw new Error("BinExpr is not ConstExpr") })
        (function (_) { throw new Error("UnExpr is not ConstExpr") })
        (function (literal) { return lf.bind(literal); });

    function whereToLF(w) {
      return matchOnPrimExpr
        (function (_) { throw new Error("AttrExpr is not of type Expr Bool") })
        (curry(function (op, a, b, c) { // TernExpr, there's only between
          var ps = PS["Lovefield.Internal.PrimExpr"] || {};
          if (op instanceof ps.OpBetween) {
            return extractAttr(a).between(extractConst(b), extractConst(c));
          } else {
            throw new Error("Unknown TernOp " + op);
          }
        }))
        (curry(function (op, a, b) { // BinExpr
          var ps = PS["Lovefield.Internal.PrimExpr"] || {};
          if (op instanceof ps.OpEq) {
            return extractAttr(a).eq(extractConstAndExpr(b));
          } else if (op instanceof ps.OpNotEq) {
            return extractAttr(a).neq(extractConstAndExpr(b));
          } else if (op instanceof ps.OpLt) {
            return extractAttr(a).lt(extractConstAndExpr(b));
          } else if (op instanceof ps.OpLtEq) {
            return extractAttr(a).lte(extractConstAndExpr(b));
          } else if (op instanceof ps.OpGt) {
            return extractAttr(a).gt(extractConstAndExpr(b));
          } else if (op instanceof ps.OpGtEq) {
            return extractAttr(a).gte(extractConstAndExpr(b));
          } else if (op instanceof ps.OpMatch) {
            return extractAttr(a).match(extractConstAndExpr(b));
          } else if (op instanceof ps.OpIn) {
            return extractAttr(a).in(extractConstAndExpr(b));
          } else {
            throw new Error("Unknown BinOp " + op);
          }
        }))
        (curry(function (op, a) { // UnExpr
          var ps = PS["Lovefield.Internal.PrimExpr"] || {};
          if (op instanceof ps.OpIsNull) {
            return extractAttr(a).isNull();
          } else if (op instanceof ps.OpIsNotNull) {
            return extractAttr(a).isNotNull();
          } else {
            throw new Error("Unknown UnOp " + op);
          }
        }))
        (function (_) { throw new Error("ConstExpr is not of type Expr Bool") })
        (w.condition);
    };


    var q = db.select();
    q = q.from.apply(q, aliases);
    if (wheres.length > 0) {
        var clauses = wheres.map(whereToLF);
        q = q.where(clauses.length > 1 ? lf.op.and(clauses) : clauses[0]);
    }
    return q.exec()
      .then(function (rows) { return success(rows)(); })
      .catch(function (e) { return error(e)(); });
  };
};
