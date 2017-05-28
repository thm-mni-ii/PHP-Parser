package parser.literals

import ast.Expressions._
import ast.Statements._
import fastparse.all._
import parser.literals.Keywords._

/**
  * Created by tobias on 27.05.17.
  */
object KeywordConversions {

  // NamespaceUseTypes
  val functionUseType = P(FUNCTION).map(_ => NamespaceUseType.FUNCTION)
  val constUseType = P(CONST).map(_ => NamespaceUseType.CONST)

  // Modifier
  val abstractMod = P(ABSTRACT).map(_ => AbstractMod)
  val finalMod = P(FINAL).map(_ => FinalMod)
  val publicMod = P(PUBLIC).map(_ => PublicMod)
  val privateMod = P(PRIVATE).map(_ => PrivateMod)
  val protectedMod = P(PROTECTED).map(_ => ProtectedMod)
  val staticMod = P(STATIC).map(_ => StaticMod)
  val noMod = P(VAR).map(_ => NoMod)

  // CastTypes
  val arrayCastType = P(ARRAY).map(_ => CastType.ARRAY)
  val binaryCastType = P(BINARY).map(_ => CastType.BINARY)
  val booleanCastType = P(BOOLEAN).map(_ => CastType.BOOLEAN)
  val boolCastType = P(BOOL).map(_ => CastType.BOOL)
  val doubleCastType = P(DOUBLE).map(_ => CastType.DOUBLE)
  val integerCastType = P(INTEGER).map(_ => CastType.INTEGER)
  val intCastType = P(INT).map(_ => CastType.INT)
  val floatCastType = P(FLOAT).map(_ => CastType.FLOAT)
  val objectCastType = P(OBJECT).map(_ => CastType.OBJECT)
  val realCastType = P(REAL).map(_ => CastType.REAL)
  val stringCastType = P(STRING).map(_ => CastType.STRING)
  val unsetCastType = P(UNSET).map(_ => CastType.UNSET)

  // DeclareDeclarative
  val ticksDeclarative = P(TICKS).map(_ => DeclareDeclarative.TICKS )
  val encodingDeclarative = P(ENCODING).map(_ => DeclareDeclarative.ENCODING)
  val strictTypesDeclarative = P(STRICT_TYPES).map(_ => DeclareDeclarative.STRICT_TYPES)

  // TypeDecl
  val arrayType = P(ARRAY).map(_ => ArrayType)
  val callableType = P(CALLABLE).map(_ => CallableType)
  val iterableType = P(ITERABLE).map(_ => IterableType)
  val boolType = P(BOOL).map(_ => BoolType)
  val floatType = P(FLOAT).map(_ => FloatType)
  val intType = P(INT).map(_ => IntType)
  val stringType = P(STRING).map(_ => StringType)
  val voidType = P(VOID).map(_ => VoidType)

  // ScopeTypes
  val selfScope = P(SELF).map(_ => ScopeType.SELF)
  val parentScope = P(PARENT).map(_ => ScopeType.PARENT)
  val staticScope = P(STATIC).map(_ => ScopeType.STATIC)

}
