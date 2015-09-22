import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211

class TestSuite extends FunSuite {
	test("foo") {
		def findGet(tree: Tree): Unit = {
			// println(tree)
			tree match {
				// Members (meta.Member)
				case q"..$mods val ..$pats: $tpeopt = $expr" => findGet(expr)
				case q"..$mods var ..$pats: $tpeopt = $expropt" => findGet(expropt)
				case q"..$mods def $name[..$tparams](..$paramss): $tpe = macro $expr" => findGet(expr)
				case q"..$mods def $name[..$tparams](..$paramss): $tpeopt = $expr" => findGet(expr)
				case q"..$mods class $tname[..$tparams] $mod (..$paramss) extends $template" =>  findGet(template)
				case q"..$mods trait $tname[..$tparams] extends $template" => findGet(template)
				case q"..$mods object $name extends $template" => findGet(template)
				case q"package object $name extends $template" => findGet(template)
				case q"package $ref { ..$stats }" => stats.map(findGet)
				// TODO: What is this EOF error with this line??
				// case q"..$mods def this(..$params)" =>
				case q"..$mods def this(..$paramss) = $expr" => findGet(expr)

				// Template (meta.Template)
				case template"{ ..$stats0 } with ..$ctorcalls { $param => ..$stats1 }" => stats1.map(findGet)

				// Literals: Nothing to do here
				// case q"$bool" => 
				// case q"$byte" =>
				// case q"$short" =>
				// case q"$int" =>
				// case q"$long" =>
				// case q"$float" =>
				// case q"$double" =>
				// case q"$char" =>
				// case q"$str" =>
				// case q"$symbol" =>
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
				// case q"$lit" => findGet(lit)

				// Arguments (meta.Term.Arg)
				// Types (meta.Type)
				// Arguments Types (meta.Type.Arg)

				// Patterns (meta.Pat) and Cases (meta.Case)
				case p"_" =>
				case p"$name" =>
				case p"$pname @ $apat" =>
				case p"$pat0 | $pat1" =>
				case p"(..$pats)" =>
				case p"$ref[..$tpes](..$apats)" =>
				case p"$pat $name (..$apats)" =>
				// case p""" $name"$${..$pats}" """ => 
				case p"$pat: $ptpe" =>
				case p"`name`" =>
				case p"$expr.$name" => findGet(expr)
				case p"$lit" =>
				case p"case $pat if $expropt => $expr" => findGet(expropt); findGet(expr)

				//Argument Patterns (meta.Pat.Arg)
				// Type Patterns (meta.Pat.Type)
				// Statements (meta.Stat)

				// Value Parameters (meta.Term.Param)
				// Type Parameters (meta.Type.Param)
				// Constructor References (meta.Ctor.Ref and meta.Term)

				// Enumerator (meta.Enum)
				case enumerator"$pat <- $expr" => findGet(expr)
				case enumerator"$pat = $expr" => findGet(expr)
				case enumerator"if $expr" => findGet(expr)

				// Sources (meta.Source)
				case source"..$stats" => stats.map(findGet)

				// Default: Just print so we know there is a case missing
				case _ => println("Default case")
					      println(tree)
			}
		}


		findGet("""
			|object O {
			|	def main(args: Array[String]) {
			|		val x = Some(42)
			|		x.get
			|	}
			|}
			""".stripMargin.parse[Stat])
	}
}