"use strict"

// module Lovefield.Query

function curry(fx) {
  // http://blog.carbonfive.com/2015/01/14/gettin-freaky-functional-wcurried-javascript/
  var arity = fx.length;

  return function f1() {
    var args = Array.prototype.slice.call(arguments, 0);
    if (args.length >= arity) {
      return fx.apply(null, args);
    }
    else {
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
        (function (_) { throw new Error("ConstExpr is not AttrExpr") })

    var extractConst = matchOnPrimExpr
        (function (_) { throw new Error("AttrExpr is not AttrExpr") })
        (function (_) { throw new Error("TernExpr is not AttrExpr") })
        (function (_) { throw new Error("BinExpr is not AttrExpr") })
        (function (_) { throw new Error("UnExpr is not AttrExpr") })
        (function (literal) { return lf.bind(literal); })

    function whereToLF(w) {
      return matchOnPrimExpr
        (function (_) { throw new Error("AttrExpr is not of type Expr Bool") })
        (curry(function (op, a, b, c) { // TernExpr, there's only between
          if (op instanceof OpBetween) {
            return extractAttr(a).between(extractConst(b), extractConst(c));
          } else {
            throw new Error("Unknown TernOp " + op);
          }
        }))
        (curry(function (op, a, b) { // BinExpr
          if (op instanceof OpEq) {
            return extractAttr(a).eq(extractConst(b));
          } else if (op instanceof OpNotEq) {
            return extractAttr(a).neq(extractConst(b));
          } else if (op instanceof OpLt) {
            return extractAttr(a).lt(extractConst(b));
          } else if (op instanceof OpLtEq) {
            return extractAttr(a).lte(extractConst(b));
          } else if (op instanceof OpGt) {
            return extractAttr(a).gt(extractConst(b));
          } else if (op instanceof OpGtEq) {
            return extractAttr(a).gte(extractConst(b));
          } else if (op instanceof OpMatch) {
            return extractAttr(a).match(extractConst(b));
          } else if (op instanceof OpIn) {
            return extractAttr(a).in(extractConst(b));
          } else {
            throw new Error("Unknown BinOp " + op);
          }
        }))
        (curry(function (op, a) { // UnExpr
          if (op instanceof OpIsNull) {
            return extractAttr(a).isNull();
          } else if (op instanceof OpIsNotNull) {
            return extractAttr(a).isNotNull();
          } else {
            throw new Error("Unknown UnOp " + op);
          }
        }))
        (function (_) { throw new Error("ConstExpr is not of type Expr Bool") })
        (w.condition);
    };

    var whereClause = lf.op.and(wheres.map(whereToLF));

    var q = db.selectAll();
    var q = q.from(q, aliases);
    var q = q.where(whereClause);
    return q.exec();
  };
};

exports.fromNative = function (db, name, q) {
  var tbl = db.getSchema().table(name);
  return q.from(tbl)
      .then(function (rows) { return success(rows)(); })
      .catch(function (e) { return error(e)(); });
};

exports.execNative = function (q, error, success) {
  return function () {
    q.exec()
      .then(function (rows) { return success(rows)(); })
      .catch(function (e) { return error(e)(); });
  };
};
