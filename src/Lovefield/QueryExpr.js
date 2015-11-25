"use strict"

// module Lovefield.QueryExpr

exports.selectAll = function (db) { return db.select(); };

exports.fromNative = function (db, name, q) {
  var tbl = db.getSchema().table(name);
  return q.from(tbl);
};

exports.execNative = function (q, error, success) {
  return function () {
    q.exec()
      .then(function (rows) { return success(rows)(); })
      .catch(function (e) { return error(e)(); });
  };
};
