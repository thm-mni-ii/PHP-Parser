package de.thm.ii.phpparser.parser.literals

import de.thm.ii.phpparser.ast.{Expressions => EAst, Statements => SAst}
import fastparse.all._
import de.thm.ii.phpparser.parser.literals.Keywords._

/**
  * Created by tobias on 27.05.17.
  */
object KeywordConversions {

  // NamespaceUseTypes
  val FunctionUseType = P(FUNCTION).map(_ => SAst.NamespaceUseType.FUNCTION)
  val ConstUseType = P(CONST).map(_ => SAst.NamespaceUseType.CONST)

  // Modifier
  val AbstractMod = P(ABSTRACT).map(_ => SAst.AbstractMod)
  val FinalMod = P(FINAL).map(_ => SAst.FinalMod)
  val PublicMod = P(PUBLIC).map(_ => SAst.PublicMod)
  val PrivateMod = P(PRIVATE).map(_ => SAst.PrivateMod)
  val ProtectedMod = P(PROTECTED).map(_ => SAst.ProtectedMod)
  val StaticMod = P(STATIC).map(_ => SAst.StaticMod)
  val NoMod = P(VAR).map(_ => SAst.NoMod)

  // CastTypes
  val ArrayCastType = P(ARRAY).map(_ => EAst.CastType.ARRAY)
  val BinaryCastType = P(BINARY).map(_ => EAst.CastType.BINARY)
  val BooleanCastType = P(BOOLEAN).map(_ => EAst.CastType.BOOLEAN)
  val BoolCastType = P(BOOL).map(_ => EAst.CastType.BOOL)
  val DoubleCastType = P(DOUBLE).map(_ => EAst.CastType.DOUBLE)
  val IntegerCastType = P(INTEGER).map(_ => EAst.CastType.INTEGER)
  val IntCastType = P(INT).map(_ => EAst.CastType.INT)
  val FloatCastType = P(FLOAT).map(_ => EAst.CastType.FLOAT)
  val ObjectCastType = P(OBJECT).map(_ => EAst.CastType.OBJECT)
  val RealCastType = P(REAL).map(_ => EAst.CastType.REAL)
  val StringCastType = P(STRING).map(_ => EAst.CastType.STRING)
  val UnsetCastType = P(UNSET).map(_ => EAst.CastType.UNSET)

  // DeclareDeclarative
  val TicksDeclarative = P(TICKS).map(_ => SAst.DeclareDeclarative.TICKS )
  val EncodingDeclarative = P(ENCODING).map(_ => SAst.DeclareDeclarative.ENCODING)
  val StrictTypesDeclarative = P(STRICT_TYPES).map(_ => SAst.DeclareDeclarative.STRICT_TYPES)

  // TypeDecl
  val ArrayType = P(ARRAY).map(_ => SAst.ArrayType)
  val CallableType = P(CALLABLE).map(_ => SAst.CallableType)
  val IterableType = P(ITERABLE).map(_ => SAst.IterableType)
  val BoolType = P(BOOL).map(_ => SAst.BoolType)
  val FloatType = P(FLOAT).map(_ => SAst.FloatType)
  val IntType = P(INT).map(_ => SAst.IntType)
  val StringType = P(STRING).map(_ => SAst.StringType)
  val VoidType = P(VOID).map(_ => SAst.VoidType)

  // ScopeTypes
  val SelfScope = P(SELF).map(_ => EAst.ScopeType.SELF)
  val ParentScope = P(PARENT).map(_ => EAst.ScopeType.PARENT)
  val StaticScope = P(STATIC).map(_ => EAst.ScopeType.STATIC)

}
