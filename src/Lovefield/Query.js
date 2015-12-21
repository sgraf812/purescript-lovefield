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

exports.runQueryNative = function (db, selected, queryState, matchOnPrimExpr, error, success) {
  return function () {

    // This is needed for repeated switching over ADTs
    var ps = PS["Lovefield.Internal.PrimExpr"] || {};
    var isCase = function (value, adtCase) {
      return adtCase // adtCase might be undefined if it was not emitted
        && value instanceof adtCase;
    };

    // 1. Get the aliases for from() in place.
    //    We also need them for attribute access.

    var schema = db.getSchema();

    var aliases = [];
    for (var i = 0; i < queryState.references.length; i++) {
      var r = queryState.references[i];
      aliases[r.alias] = schema.table(r.name).as('t' + r.alias);
    }

    // Some auxiliary PrimExpr accessors

    var extractAttr = matchOnPrimExpr
        (curry(function (alias, name) { // AttrExpr, got it
          return aliases[alias][name];
        }))
        (function (_) { throw new Error("TernExpr is not AttrExpr") })
        (function (_) { throw new Error("BinExpr is not AttrExpr") })
        (function (_) { throw new Error("UnExpr is not AttrExpr") })
        (function (_) { throw new Error("AggrExpr is not AttrExpr") })
        (function (_) { throw new Error("ConstExpr is not AttrExpr") });

    var extractConst = matchOnPrimExpr
        (function (_) { throw new Error("AttrExpr is not ConstExpr") })
        (function (_) { throw new Error("TernExpr is not ConstExpr") })
        (function (_) { throw new Error("BinExpr is not ConstExpr") })
        (function (_) { throw new Error("UnExpr is not ConstExpr") })
        (function (_) { throw new Error("AggrExpr is not ConstExpr") })
        (function (literal) { return literal; });

    var extractConstAndExpr = matchOnPrimExpr
        (curry(function (alias, name) { // AttrExpr, got it
          return aliases[alias][name];
        }))
        (function (_) { throw new Error("TernExpr is not ConstExpr") })
        (function (_) { throw new Error("BinExpr is not ConstExpr") })
        (function (_) { throw new Error("UnExpr is not ConstExpr") })
        (function (_) { throw new Error("AggrExpr is not ConstExpr") })
        (function (literal) { return literal; });

    // 2. Prepare the filtering/selection of the resulting fields.
    //    LF documents this as having overhead, but I don't see a way
    //    without resorting to copying right now.
    //    Although a lazy map over the result could work.

    // selected is a record of PrimExprs (always). We can construct the mapping
    // by iterating over the keys.

    var extractAttrOrAggr = matchOnPrimExpr
        (curry(function (alias, name) { // AttrExpr, got it
          return aliases[alias][name];
        }))
        (function (_) { throw new Error("TernExpr is not AttrExpr") })
        (function (_) { throw new Error("BinExpr is not AttrExpr") })
        (function (_) { throw new Error("UnExpr is not AttrExpr") })
        (curry(function (op, expr) {
          if (isCase(op, ps.AggrCount)) {
            return lf.fn.count(extractAttrOrAggr(expr));
          } else if (isCase(op, ps.AggrSum)) {
            return lf.fn.sum(extractAttrOrAggr(expr));
          } else if (isCase(op, ps.AggrAvg)) {
            return lf.fn.avg(extractAttrOrAggr(expr));
          } else if (isCase(op, ps.AggrGeomMean)) {
            return lf.fn.geommean(extractAttrOrAggr(expr));
          } else if (isCase(op, ps.AggrMin)) {
            return lf.fn.min(extractAttrOrAggr(expr));
          } else if (isCase(op, ps.AggrMax)) {
            return lf.fn.max(extractAttrOrAggr(expr));
          } else if (isCase(op, ps.AggrStdDev)) {
            return lf.fn.stddev(extractAttrOrAggr(expr));
          } else if (isCase(op, ps.AggrDistinct)) {
            return lf.fn.distinct(extractAttrOrAggr(expr));
          }
        }))
        (function (_) { throw new Error("ConstExpr is not AttrExpr") });

    var selection = [];
    for (var fieldName in selected) {
      if (selected.hasOwnProperty(fieldName)) {
        var expr = selected[fieldName];
        selection.push(extractAttrOrAggr(expr).as(fieldName));
      }
    }

    // 3. Transform all where conditions into a single clause.
    //    This also has to inspect PrimExprs, so it gets a little messy.
    //    Would love to also do this in PS, but this is the most comfy way.

    var clauseForRestriction = matchOnPrimExpr
      (function (_) { throw new Error("AttrExpr is not of type Expr Bool") })
      (curry(function (op, a, b, c) { // TernExpr, there's only between
        if (isCase(op, ps.OpBetween)) {
          return extractAttr(a).between(extractConst(b), extractConst(c));
        } else {
          throw new Error("Unknown TernOp " + op);
        }
      }))
      (curry(function (op, a, b) { // BinExpr
        if (isCase(op, ps.OpEq)) {
          return extractAttr(a).eq(extractConstAndExpr(b));
        } else if (isCase(op, ps.OpNotEq)) {
          return extractAttr(a).neq(extractConstAndExpr(b));
        } else if (isCase(op, ps.OpLt)) {
          return extractAttr(a).lt(extractConstAndExpr(b));
        } else if (isCase(op, ps.OpLtEq)) {
          return extractAttr(a).lte(extractConstAndExpr(b));
        } else if (isCase(op, ps.OpGt)) {
          return extractAttr(a).gt(extractConstAndExpr(b));
        } else if (isCase(op, ps.OpGtEq)) {
          return extractAttr(a).gte(extractConstAndExpr(b));
        } else if (isCase(op, ps.OpMatch)) {
          return extractAttr(a).match(extractConstAndExpr(b));
        } else if (isCase(op, ps.OpIn)) {
          return extractAttr(a).in(extractConstAndExpr(b));
        } else {
          throw new Error("Unknown BinOp " + op);
        }
      }))
      (curry(function (op, a) { // UnExpr
        if (isCase(op, ps.OpIsNull)) {
          return extractAttr(a).isNull();
        } else if (isCase(op, ps.OpIsNotNull)) {
          return extractAttr(a).isNotNull();
        } else {
          throw new Error("Unknown UnOp " + op);
        }
      }))
      (function (_) { throw new Error("AggrExpr is not of type Expr Bool") })
      (function (_) { throw new Error("ConstExpr is not of type Expr Bool") });


    // 4. Prepare grouping attributes. This is the easy part,
    //    The hard part is handled in the selection section.
    var groupBys = queryState.groupings.map(function (grouping) {
      return aliases[grouping.alias][grouping.name];
    });


    // Finally build and execute the query
    var q = db.select.apply(db, selection);
    q = q.from.apply(q, aliases);
    if (queryState.restrictions.length > 0) {
      var clauses = queryState.restrictions.map(clauseForRestriction);
      q = q.where(clauses.length > 1 ? lf.op.and(clauses) : clauses[0]);
    }
    if (groupBys.length > 0) {
      q = q.groupBy.apply(q, groupBys);
    }
    for (var i = 0; i < queryState.orderings.length; ++i) {
      var ordering = queryState.orderings[i];

      var op = ordering.value0; // This is either OpAsc or OpDesc
      var order = isCase(op, ps.OpAsc) ? lf.Order.ASC : lf.Order.DESC;

      var expr = ordering.value1; // This might potentially not be an AttrExpr, I hope we get lucky.

      q = q.orderBy(extractAttr(expr), order);
    }
    if (queryState.limit != null) {
      q = q.limit(queryState.limit);
    }
    if (queryState.offset != null) {
      q = q.skip(queryState.offset);
    }
    console.log(q.explain());
    return q.exec()
      .then(function (rows) { return success(rows)(); })
      .catch(function (e) { return error(e)(); });
  };
};
