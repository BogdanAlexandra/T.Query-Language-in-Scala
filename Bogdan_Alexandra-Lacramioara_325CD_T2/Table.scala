import util.Util.Row
import util.Util.Line
import TestTables.tableImperative
import TestTables.tableFunctional
import TestTables.tableObjectOriented

trait FilterCond {
  def &&(other: FilterCond): FilterCond = And(this, other)
  def ||(other: FilterCond): FilterCond = Or(this, other)
  // fails if the column name is not present in the row
  def eval(r: Row): Option[Boolean]
}
case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  // 2.2.
  override def eval(r: Row): Option[Boolean] = {
    if (r.contains(colName)) {
      Some(predicate(r(colName)))
    } else {
      None}
  }
}

case class And(f1: FilterCond, f2: FilterCond) extends FilterCond {
  // 2.2.
  override def eval(r: Row): Option[Boolean] = {
    f1.eval(r) match {
      case Some(b1) =>
        f2.eval(r) match {
          case Some(b2) => Some(b1 && b2)
          case None => None
        }
      case None => None
    }
  }
}

case class Or(f1: FilterCond, f2: FilterCond) extends FilterCond {
  // 2.2.
  override def eval(r: Row): Option[Boolean] = {
    f1.eval(r) match {
      case Some(b1) =>
        f2.eval(r) match {
          case Some(b2) => Some(b1 || b2)
          case None => None
        }
      case None => None
    }
  }
}

trait Query {
  def eval: Option[Table]
}

/*
  Atom query which evaluates to the input table
  Always succeeds
 */
case class Value(t: Table) extends Query {
  override def eval: Option[Table] = Some(t)
}
/*
  Selects certain columns from the result of a target query
  Fails with None if some rows are not present in the resulting table
 */

case class Select(columns: Line, target: Query) extends Query {
  override def eval: Option[Table] = {
    if (target.eval.isEmpty) {
      None
    } else {
      val a = target.eval.get
      val columnNames = a.getColumnNames
      val selectedColumns = columns.map(col => columnNames.contains(col))
      if (selectedColumns.forall(identity)) {
        Some(Table.apply(a.select(columns).get.toString))
      } else {
        None
      }
    }
  }
}



/*
  Filters rows from the result of the target query
  Success depends only on the success of the target
 */
case class Filter(condition: FilterCond, target: Query) extends Query {
  override def eval: Option[Table] = target.eval.flatMap(_.filter(condition))
}

/*
  Creates a new column with default values
  Success depends only on the success of the target
 */
case class NewCol(name: String, defaultVal: String, target: Query) extends Query {
  override def eval: Option[Table] = {
    if (target.eval.isEmpty) {
      None
    } else {
      val a = target.eval.get
      Some(Table.apply(a.newCol(name, defaultVal).toString))
    }
  }
}

/*
  Combines two tables based on a common key
  Success depends on whether the key exists in both tables or not AND on the success of the target
 */
case class Merge(key: String, t1: Query, t2: Query) extends Query {
  override def eval: Option[Table] = {
    val t1Table = t1.eval.getOrElse(return None)
    val t2Table = t2.eval.getOrElse(return None)
    val mergedTable = t1Table.merge(key, t2Table).getOrElse(return None)
    Some(mergedTable)
  }
}


class Table (columnNames: Line, tabular: List[List[String]]) {
  def getColumnNames: Line = columnNames

  def getTabular: List[List[String]] = tabular

  // 1.1
  override def toString: String = {
    val columnHeader = columnNames.mkString(",")
    val rowStrings = tabular.map(row => row.mkString(","))
    (columnHeader :: rowStrings).mkString("\n")

  }

  // 2.1

  def select(columns: Line): Option[Table] = {
    def indices(columns: Line, columnNames: Line): List[Int] = columns match {
      case Nil => Nil
      case col :: rest => columnNames.indexOf(col) :: indices(rest, columnNames)
    }

    val colIndices = indices(columns, columnNames)
    if (colIndices.contains(-1)) {
      None
    } else {
      val newTabularData = tabular.map(row => colIndices.map(row(_)))
      Some(new Table(columns, newTabularData))
    }
  }

  private def rowToMap(row: List[String]): Map[String, String] = {
    columnNames.zip(row).toMap
  }

  // 2.2

  def filter(cond: FilterCond): Option[Table] = {
    val filteredTabularData = tabular.filter(row => {
      val rowMap = rowToMap(row)
      cond.eval(rowMap) match {
        case Some(bool) => bool
        case None => false
      }
    })

    if (filteredTabularData.isEmpty) None
    else Some(new Table(columnNames, filteredTabularData))
  }

  // 2.3.

  def newCol(name: String, defaultVal: String): Table = {
    val newRows = getTabular.map(row => row ++ List(defaultVal))
    val newColumnNames = getColumnNames ++ List(name)
    new Table(newColumnNames, newRows)
  }

  //2.4.

  def merge(key: String, other: Table): Option[Table] = {

    if (getColumnNames.contains(key) && other.getColumnNames.contains(key)) {

      val (thistablerow, othertablerow, thistablecol, othertablecol) =
        (this.getTabular, other.getTabular, this.getColumnNames, other.getColumnNames)

      val diffcol = othertablecol.map(col =>
        if (!thistablecol.contains(col)) Some(col) else None).filter(_.isDefined).map(_.get)

      val columns_different = thistablecol ++ diffcol

      val rowListTb1 = thistablerow.map(row => thistablecol.zip(row).toMap)
      val rowListTb2 = othertablerow.map(row => othertablecol.zip(row).toMap)

      val t1Keys = thistablerow.map(e => e(thistablecol.indexOf(key)))
      val t2Keys = othertablerow.map(e => e(othertablecol.indexOf(key)))


      val bothTables = t1Keys.intersect(t2Keys)

      val onlyT1 = t1Keys.filterNot(t2Keys.contains)
      val onlyT2 = t2Keys.filterNot(t1Keys.contains)


      val bothTablesRowT1 = bothTables.flatMap(name => rowListTb1.map(row =>
        if (row(key) == (name)) row else Nil)).filter(_ != Nil)

      val bothTablesRowT2 = bothTables.flatMap(name => rowListTb2.map(row =>
        if (row(key) == (name)) row else Nil)).filter(_ != Nil)

      val onlyT1Row = onlyT1.flatMap(name => rowListTb1.collectFirst
      { case row if row(key).equals(name) => row }).distinct

      val onlyT2Row = onlyT2.flatMap(name => rowListTb2.collectFirst
      { case row if row(key).equals(name) => row }).distinct


      def mergeRows(row1: Map[String, String], row2: Map[String, String], cols: List[String]): List[String] = cols match {
        case Nil => Nil
        case col :: rest => {
          val value1 = row1.getOrElse(col, "")
          val value2 = row2.getOrElse(col, "")
          if (value1 == value2) value1 :: mergeRows(row1, row2, rest)
          else if (value1.isEmpty) value2 :: mergeRows(row1, row2, rest)
          else if (value2.isEmpty) value1 :: mergeRows(row1, row2, rest)
          else s"$value1;$value2" :: mergeRows(row1, row2, rest)
        }
      }

      val merged_rows = for {
        row <- bothTablesRowT1
        idx = bothTablesRowT1.indexOf(row)
        rowTb1 = row.toMap
        rowTb2 = bothTablesRowT2(idx).toMap
        if rowTb1.get(key) == rowTb2.get(key)
      } yield mergeRows(rowTb1, rowTb2, columns_different)

      def build_this_rows(rows: List[Map[String, String]], colNames: List[String]): List[List[String]] = {
        rows match {
          case Nil => Nil
          case r :: rs =>
            val row = r
            val f = colNames.map(name => {
              if (row.contains(name)) {
                row.get(name).mkString
              } else {
                ""
              }
            })
            f :: build_this_rows(rs, colNames)
        }
      }

      val only_this_rows = build_this_rows(onlyT1Row, columns_different)

      def build_only_other(rows: List[Map[String, String]], columns: List[String]): List[List[String]] = {
        rows match {
          case Nil => Nil
          case r :: rs =>
            val rowMap = r
            val newRow = columns.map(name => {
              if (rowMap.contains(name)) {
                rowMap.get(name).mkString
              } else {
                ""
              }
            })
            newRow :: build_only_other(rs, columns)
        }
      }

      val only_other_rows = build_only_other(onlyT2Row, columns_different)

      val merged_tabular = merged_rows ++ only_this_rows ++ only_other_rows
      Some(new Table(columns_different, merged_tabular))

    }
    else None
  }

}

object Table {

  // 1.2

  def apply(s: String): Table = {
    val lines = s.split("\n").toList
    val columns = lines.head.split(",").toList
    val rows = lines.tail.map(line => line.split(",", -1).toList)
    new Table(columns, rows)
  }

}
