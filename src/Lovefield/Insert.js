"use strict"

// module Lovefield.Insert

exports.insertOrReplaceNative = function (db, tableName, values, error, success) {
  return function () {
    var table = db.getSchema().table(tableName);
    var rows = values.map(function (v) { return table.createRow(v); });
    return db.insertOrReplace().into(table).values(rows).exec()
      .then(function () { success(PS["Prelude"].unit)(); })
      .catch(function (e) { error(e)(); });
  }
}
