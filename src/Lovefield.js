"use strict"

// module Lovefield

exports.mkExistentialTable = function (value) {
  return value;
};

exports.runExistentialTable = function (f) {
  return f;
};

exports.createNative = function (name, version) {
  return function () {
    return lf.schema.create(name, version);
  }
}

exports.createTableNative = function (name, sb) {
  return function () {
    return sb.createTable(name);
  }
}

function stringToLFType(type) {
  switch (type) {
    case "int":
      return lf.Type.INTEGER;
    case "number":
      return lf.Type.NUMBER;
    case "string":
      return lf.Type.STRING;
    case "boolean":
      return lf.Type.BOOLEAN;
    case "datetime":
      return lf.Type.DATE_TIME;
    case "arrayobject":
      return lf.Type.ARRAY_BUFFER;
    case "object":
      return lf.Type.OBJECT;
    default:
      throw Error("Invalid marshaled lf type " + type);
  }
}

exports.addColumnNative = function (name, type, tb) {
  return function () {
    return tb.addColumn(name, stringToLFType(type));
  }
}

exports.addPrimaryKey = function (columns, autoIncrement, tb) {
  return function () {
    return tb.addPrimaryKey(columns, autoIncrement);
  }
}

exports.addUnique = function (name, columns, tb) {
  return function () {
    return tb.addUnique(name, columns);
  }
}

exports.addForeignKey = function (fkname, tb) {
  return function () {
    return tb.addForeignKey(fkname);
  }
}

exports.addNullable = function (columns, tb) {
  return function () {
    return tb.addNullable(columns);
  }
}

exports.connectNative = function (sb, error, success) {
  return function () {
    return sb.connect()
             .then(function (db) { return success(db)(); })
             .catch(function (e) { return error(e)(); });
  }
}

exports.insertOrReplaceNative = function (db, tableName, values, error, success) {
  return function () {
    var table = db.getSchema().table(tableName);
    var rows = values.map(function (v) { return table.createRow(v); });
    return db.insertOrReplace().into(table).values(rows).exec()
      .then(function () { success(PS["Prelude"].unit)(); })
      .catch(function (e) { error(e)(); });
  }
}
