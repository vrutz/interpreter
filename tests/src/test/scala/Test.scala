import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211

class TestSuite extends FunSuite {
	test("foo") {
		def findGet(tree: Tree): Unit = tree match {
			case q"..$mods object $name extends $template" => findGet(template)
			case template"{ ..$stats0 } with ..$ctorcalls { $param => ..$stats1 }" => stats1.map(findGet)
			case q"..$mods def $name[..$tparams](..$paramss): $tpeopt = $expr" => findGet(expr)
			case q"..$mods val ..$pats: $tpeopt = $expr" => findGet(expr)

			// Literals: Nothing to do here
			case q"$bool" => 
			case q"$byte" =>
			case q"$short" =>
			case q"$int" =>
			case q"$long" =>
			case q"$float" =>
			case q"$double" =>
			case q"$char" =>
			case q"$str" =>
			case q"$symbol" =>
			case q"null" =>
			case q"()" =>

			//Expressions (meta.Term)
			case q"this" =>
			case q"$qname.this" =>
			case q"$expr.$name" if name.toString == "get" => println(s"Found one get in expression: $tree")
			case q"$expr.$name" =>
			// case q""" $name"$${..$exprs}" """ => exprs.map(findGet)
			case q"$expr(..$aexprs)" => findGet(expr); aexprs.map(findGet)
			// case q"$expr[..$tpes]" => findGet(expr)
			case q"$expr $name[..$tpes] (..$aexprs)" => findGet(expr); aexprs.map(findGet)
			case q"!$expr" => findGet(expr)
			case q"~$expr" => findGet(expr)
			case q"-$expr" => findGet(expr)
			case q"+$expr" => findGet(expr)
			case q"$ref = $expr" => findGet(expr)
			case q"$expr0(..$aexprs) = $expr1" => findGet(expr0); aexprs.map(findGet); findGet(expr1)
			case q"return $expr" => findGet(expr)
			case q"throw $expr" => findGet(expr)
			case q"$expr: $tpe" => findGet(expr)
			case q"$expr: ..@$annots" => findGet(expr)
			case q"(..$exprs)" => exprs.map(findGet)
			case q"{ ..$stats }" => stats.map(findGet)
			case q"if ($cond) $thn else $elz" => findGet(cond); findGet(thn); findGet(elz)
			case q"$expr match { ..case $cases }" => findGet(expr); cases.map(findGet)
			case q"try $expr catch { ..case $cases } finally $expropt" => findGet(expr); cases.map(findGet); findGet(expropt)
			case q"try $expr0 catch $expr1 finally $expropt" => findGet(expr0); findGet(expr1); findGet(expropt)
			case q"(..$params) => $expr" => findGet(expr)
			case q"{ ..case $cases }" => cases.map(findGet)
			case q"while ($cond) $expr" => findGet(cond); findGet(expr)
			case q"do $expr while($cond)" => findGet(expr); findGet(cond)
			case q"for (..$enumerators) $expr" => enumerators.map(findGet); findGet(expr)
			case q"for (..$enumerators) yield $expr" => enumerators.map(findGet); findGet(expr)
			case q"new { ..$stat } with ..$exprs { $param => ..$stats }" => stat.map(findGet); exprs.map(findGet); stats.map(findGet)
			case q"_" =>
			case q"$expr _" => findGet(expr)
			case q"$lit" => findGet(lit)

			// Arguments (meta.Term.Arg)
			case qarg"$name = $expr" => findGet(expr)
			case qarg"$expr: _*" => findGet(expr)
			case qarg"$expr" => findGet(expr)

			// Types (meta.Type)
			// Arguments Types (meta.Type.Arg)

			// Patterns (meta.Pat) and Cases (meta.Case)
			case p"_" =>
			case p"$name" =>
			case p"$pname @ $apat" =>
			case p"$pat0 | $pat1" =>
			case p"(..$pats)" =>
			case p"$ref[..$tpes](..$apats)" =>
			case p"case $pat if $expropt => $expr" => findGet(expropt); findGet(expr)

			// Default: Just print so we know there is a case missing
			case _ => println("Default case")
				      println(tree)
		}


		findGet("""
			object O {
				def main(args: Array[String]) {
					val x = Some(42)
					x.get
				}
			}
			""".parse[Stat])
	}
}