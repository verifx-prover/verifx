package org.verifx.Compiler

import Types.{Classes, Proof, TypeWithEqualsCheck, TypeWithSubtypeCheck}
import CompilerUtil.{isAnnotation, isOverrideModifier, isPrivateModifier, isProtectedModifier, isRecursiveAnnotation}
import Implicits._
import org.jgrapht.graph.{DefaultEdge, DirectedAcyclicGraph}
import org.verifx.{ClassNotFound, FieldDoesNotExist, IllegalArgumentException, IllegalOperationError, MatchError, MethodNotFound, ParseError, TypeError}

import scala.annotation.tailrec
import scala.meta.Mod.{Annot, Sealed}
import scala.meta._
import scala.meta.contrib._

object IRConversions {
  // Returns all the names of the (nested) type parameters
  def getAllTParamNames(tparam: Type.Param): Set[String] = {
    Set(tparam.name.value) ++ tparam.tparams.map(getAllTParamNames).foldLeft(Set.empty[String])(_ ++ _)
  }

  def typeParam2IR(tparam: Type.Param)(implicit clazz: Type, thisMethod: Option[String], classTable: Classes): Term = {
    tparam match {
      case Type.Param(_, name, tparams, tbounds, _, _) => {
        tbounds.hi match {
          case Some(upperTypeBound) => {
            val tparamsTypes = getAllTParamNames(tparam) - name.value // get the names of all types introduced by this type parameter
            val upperTypeIR = Types.transformer(upperTypeBound, tparamsTypes)
            q"Type.TypeParameter(${name.toString()}, List(..${tparams.map(typeParam2IR)}), Some($upperTypeIR))"
          }
          case None => {
            q"Type.TypeParameter(${name.toString()}, List(..${tparams.map(typeParam2IR)}), None)"
          }
        }
      }
    }
  }

  def termParam2IR(tparam: Term.Param)(implicit clazz: Type, thisMethod: Option[String], classTable: Classes): Term =
    tparam match {
      case Term.Param(_, name, Some(declType), _) =>
        q"Defn.Arg(${name.value}, ${Types.transformer(declType)})"
      case param => throw ParseError(s"Illegal parameter $param in $clazz", param.pos)
    }

  def termParam2IRField(tparam: Term.Param)(implicit clazz: Type, thisMethod: Option[String], classTable: Classes): Term =
    tparam match {
      case Term.Param(mods, name, Some(declType), _) =>
        q"Defn.Field(List(..${mods2IR(mods, tparam)}), ${name.value}, ${Types.transformer(declType)})"
      case param => throw ParseError(s"Illegal parameter $param in $clazz", param.pos)
    }

  def ascribe2IR(asc: Term.Ascribe)(implicit clazz: Type, thisMethod: Option[String], classTable: Classes): Term =
    asc match {
      case Term.Ascribe(Term.Name(name), argType) =>
        q"Defn.Arg($name, ${Types.transformer(argType)})"
    }

  def mods2IR(mods: List[Mod], exp: Any)(implicit clazz: Type): List[Term] = mods map {
    case mod => mod match {
      case Mod.Private(_) => q"""Mod.Private"""
      case Mod.Protected(_) => q"""Mod.Protected"""
      case Mod.Override() => q"Mod.Override"
      case Mod.Annot(Init(Type.Name(name), _, _)) => q"""Mod.Annot($name)"""
      case _ => throw ParseError(s"Illegal modifier $mod on $exp in $clazz", mod.pos)
    }
  }
}

object Implicits {
  implicit class ExtendedListOfStats(l: List[Stat]) {
    def methodsAndPredsOnly() = l.filter(stat => MethodCompiler.isMethod(stat) || MethodPredicate.isMethodPred(stat))

    /**
     * Takes a list containing method definitions and method predicates, possibly with duplicates,
     * and returns a list containing no duplicates and keeping the first of each method/predicate.
     */
    def distinctMethodsAndPreds(): List[Stat] = {
      l.distinctBy {
        case m: Defn.Def => (m.name, "METHOD")
        case p if MethodPredicate.isMethodCtx(p) => (MethodPredicate.getMethodName(p), "CTX")
        case p if MethodPredicate.isMethodPre(p) => (MethodPredicate.getMethodName(p), "PRE")
        case p if MethodPredicate.isMethodInv(p) => (MethodPredicate.getMethodName(p), "INV")
        case x => throw new InternalError(s"Expected a method definition or a predicate but got something else: $x")
      }
    }
  }

  implicit class ExtendedStat(s: Stat) {
    def methodName(): String = s match {
      case m: Defn.Def => m.name.value
      case m: Decl.Def => m.name.value
      case p if MethodPredicate.isMethodPred(p) => MethodPredicate.getMethodName(p)
      case x => throw new InternalError(s"Expected a method definition or a method predicate but got something else: $x")
    }
  }
}

object CompilerUtil {
  /**
   * Takes a list of parameters and returns a map containing the type of each parameter.
   */
  def termParamsToTypeMap(lst: List[Term.Param]): Map[String, Type] = {
    lst.foldLeft(Map.empty[String, Type]) {
      case (typeMap, param) => {
        param.decltpe match {
          case Some(tpe) => {
            val paramName = param.name.value
            typeMap + (paramName -> tpe)
          }
          case None => throw ParseError(s"Missing type for $param", param.pos)
        }
      }
    }
  }

  def termParamToFieldMap(lst: List[Term.Param]): Map[String, Field] = {
    lst.foldLeft(Map.empty[String, Field]) {
      case (fieldMap, param) => {
        param.decltpe match {
          case Some(tpe) => {
            val paramName = param.name.value
            fieldMap + (paramName -> Field(param.mods, paramName, tpe, param))
          }
          case None => throw ParseError(s"Missing type for $param", param.pos)
        }
      }
    }
  }

  def ascribeToParam(a: Term.Ascribe): Term.Param = a match {
    case Term.Ascribe(Term.Name(name), argType) => Term.Param(List(), Name(name), Some(argType), None)
  }

  def ascribesToTypeMap(lst: List[Term.Ascribe]) = {
    val termParams = lst.map(ascribeToParam)
    termParamsToTypeMap(termParams)
  }

  def varsToTypeMap(vars: List[Term], env: Map[String, Type]): Map[String, Type] = {
    // extend the environment with the variables defined by the quantifier
    val varTypes = CompilerUtil.ascribesToTypeMap(vars.asInstanceOf[List[Term.Ascribe]])
    env ++ varTypes
  }

  /**
   * Extends the provided `args` with the default arguments `defaults` where needed.
   * @param args Arguments passed by the programmer.
   * @param defaults Default values for the arguments.
   * @param exp Call expression (used for providing meaningful error messages)
   * @return A list of extended arguments.
   */
  def extendArgsWithDefaults(args: List[Term], defaults: List[Option[Term]], exp: Term): List[Term] = {
    if (args.length > defaults.length)
      throw ParseError(s"Expected ${defaults.length} arguments but got ${args.length}", exp.pos)

    val someArgs = args.map(Some(_))
    val delta = defaults.length - someArgs.length
    val extension = List.fill[Option[Term]](delta)(None)

    val extendedArgs = someArgs ++ extension
    // Now `extendedArgs` and `defaults` have the same length so we can zip them
    (extendedArgs zip defaults) map {
      case (Some(providedArg), _) => providedArg
      case (None, Some(default)) => default
      case _ => throw ParseError(s"Too few arguments provided in $exp", exp.pos)
    }
  }

  /**
   * Checks that an argument list is well-formed,
   * i.e. that once a default value has been provided all subsequent arguments also have default values.
   * Also checks that the types of the default values are right, and that the type of each argument is concrete.
   * @param exp Expression that is being compiled (used to provide meaningful error messages)
   */
  def checkArgumentListCorrectness(args: List[Term.Param], argTypes: Map[String, Type], tparams: List[Type.Param], exp: Tree)(implicit thisClass: Type, classTable: Classes): Unit = {
    args.map(arg => (arg.name.value, argTypes.get(arg.name.value).get)).foreach { // don't loop over `argTypes` because it contains `this` which may be a trait type
      case (arg, tpe) => {
        if (classTable.isTrait(tpe, tparams) && !classTable.isSealedTrait(tpe, tparams))
          throw TypeError(s"Illegal argument type $tpe for argument $arg\nArgument types must be concrete but $tpe trait is not sealed.", tpe.pos)
      }
    }

    val idx = args.indexWhere(_.default.isDefined)

    if (idx >= 0) {
      val defaultArgs = args.splitAt(idx)._2
      if (!defaultArgs.forall(_.default.isDefined))
        throw ParseError(s"Illegal argument list, once a default value is provided all subsequent arguments must have default values", exp.pos)

      // Also check that the values are of the right type
      defaultArgs.foreach {
        case Term.Param(_, name, _, maybeDefault) => {
          val default = maybeDefault.get
          val defaultType = Types.inferType(default, argTypes.filter(kv => kv._1 == "this"))
          val expectedType = argTypes.get(name.value).get
          if (!(defaultType <:< expectedType))
            throw TypeError(s"Argument $name is of type $expectedType but default value $default has type $defaultType", name.pos)
        }
      }
    }
  }

  /**
   * Checks that there are no duplicate type parameters.
   */
  def checkTParams(tparams: List[Type.Param], allowUpperTypeBounds: Boolean = false)(implicit classTable: Classes): Unit = {
    if (tparams.nonEmpty) {
      // Check that there are no duplicate type parameters
      val typeParamOccurences = tparams.groupBy(_.name.value).toList
      typeParamOccurences.find(_._2.length > 1) match {
        case Some((typeParam, occurences)) => throw ParseError(s"Type parameters must be unique but $typeParam occurs $occurences times", occurences.tail.head.pos)
        case None => ()
      }

      // Check that there are only upper type bounds (lower type bounds are not supported)
      // and also check that those upper type bounds are traits
      // (it is not allowed to have an upper type bound which is a primitive, class, or type parameter because those cannot be subtyped in VeriFx)
      tparams.foldLeft(List.empty[Type.Param]) {
        case (previousTParams, tparam) => {
          tparam.tbounds.hi.foreach {
            case upperTypeBound => {
              // The upper type bound should be a trait
              if (!classTable.isTrait(upperTypeBound, previousTParams))
                throw TypeError(s"Type parameter $tparam has illegal upper type bound $upperTypeBound. The upper type bound must be a trait (i.e. cannot be a primitive, class, or type parameter).", tparam.pos)
            }
          }
          previousTParams.appended(tparam)
        }
      }

      // Do not allow lower type bounds
      tparams.foreach(tparam => {
        if (tparam.tbounds.lo.nonEmpty)
          throw TypeError(s"Illegal type parameter $tparam. Lower type bounds are not allowed.", tparam.pos)
        if (tparam.tbounds.hi.nonEmpty && !allowUpperTypeBounds)
          throw TypeError(s"Illegal type parameter $tparam. Upper type bound is not allowed here (only on type parameters of traits).", tparam.pos)
      })
    }
  }

  /**
   * @param mod A modifier.
   * @return `true` if the modifier is `@recursive`, false` otherwise.
   */
  def isRecursiveAnnotation(mod: Mod) = mod match {
    case Mod.Annot(Init(Type.Name("recursive"), Name(""), Nil)) => true
    case _ => false
  }

  def isProtectedModifier(mod: Mod) = mod match {
    case Mod.Protected(_) => true
    case _ => false
  }

  def isPrivateModifier(mod: Mod) = mod match {
    case Mod.Private(_) => true
    case _ => false
  }

  def isOverrideModifier(mod: Mod) = mod match {
    case Mod.Override() => true
    case _ => false
  }

  def isAnnotation(mod: Mod) = mod match {
    case Mod.Annot(Init(_, Name(""), Nil)) => true
    case _ => false
  }

  def isSealedModifier(mod: Mod) = mod match {
    case Mod.Sealed() => true
    case _ => false
  }

  val _ENUM_KEYWORD_ = "enum" // keyword that is used for defining enumerations

  def isEnum(stat: Stat): Boolean = stat match {
    case Term.ApplyInfix(Term.Name(`_ENUM_KEYWORD_`), _, _, _) => true
    case _ => false
  }

  def getEnumName(obj: Term.ApplyInfix): String = obj match {
    case Term.ApplyInfix(Term.Name(`_ENUM_KEYWORD_`), Term.Name(enumName), _, _) => enumName
  }

  def isComparisonOperator(op: String) = isArithmeticComparisonOperator(op) || op == "==" || op == "!="
  def isArithmeticComparisonOperator(op: String) = op == "<" || op == "<=" || op == ">" || op == ">="
  def isBooleanOperator(op: String) = op == "||" || op == "&&"
  def isArithmeticOperator(op: String) = op == "+" || op == "-" || op == "*" || op == "/"
  def isLogicOperator(op: String) = op == "=>:"

  def isValueOrMethodOrPredicate(stat: Stat): Boolean = {
    MethodCompiler.isValueDeclaration(stat) || MethodCompiler.isMethod(stat) ||
    MethodCompiler.isMethodDeclaration(stat) || MethodPredicate.isMethodPred(stat)
  }

  def isValueOrMethodOrPredicateOrProof(stat: Stat): Boolean = {
    isValueOrMethodOrPredicate(stat) || ProofCompiler.isProof(stat)
  }
}

case class MethodBody(var env: Map[String, Type])(implicit val thisClass: Type, val thisMethod: Option[String], val classTable: Classes) {
  private val (methodTParams, classTParams) = {
    val clazz = classTable.get(thisClass).get
    val methodTparams = thisMethod.flatMap(clazz.getMethod(_)).map(_.tparams).getOrElse(List.empty[Type.Param])
    (methodTparams, clazz.tparams)
  }

  private def transformSelect(qual: Term, field: Term.Name): Term = {
    val obj = transformer(qual)
    val objTpe = Types.inferType(qual, env)
    val transformedObjTpe = Types.transformer(objTpe)

    Types.getClassM(objTpe, classTable, thisClass) match {
      case None => {
        if (Types.isPrimitiveType(objTpe))
          throw ParseError(s"Missing argument(s) for operator $field on class $objTpe", field.pos)
        else
          throw ClassNotFound(objTpe.toString, qual.pos)
      }
      case Some(clazz) => {
        clazz.getField(field.value) match {
          case Some(f) => {
            val transformedFieldType = Types.transformer(f.tpe)
            q"Term.FieldAccess($transformedObjTpe, $obj, ${field.value}, $transformedFieldType)"
          }
          case None => throw FieldDoesNotExist(clazz.name.value, field.value, field.pos)
        }
      }
    }
  }

  private def makeMethodRef(qual: Term, field: Term.Name): Term = {
    val obj = transformer(qual)
    val objTpe = Types.inferType(qual, env)
    val transformedObjTpe = Types.transformer(objTpe)

    Types.getClassM(objTpe, classTable, thisClass) match {
      case None => {
        if (Types.isPrimitiveType(objTpe))
          throw ParseError(s"Missing argument(s) for operator $field on class $objTpe", field.pos)
        else
          throw ClassNotFound(objTpe.toString, qual.pos)
      }
      case Some(clazz) => {
        clazz.getMethod(field.value) match {
          case Some(m) => {
            // Reject method reference if the method has type parameters
            // because we don't know what the actual type arguments are for the method
            if (m.tparams.nonEmpty) throw ParseError("Cannot create a reference to a parametric method.", field.pos)
            // Reject method reference if the method does not take arguments
            // because we do not allow nullary functions
            if (m.argList.isEmpty) throw TypeError("Cannot turn nullary methods into functions because they are constants.", field.pos)

            val transformedMethodType = Types.transformer(m.tpe)
            q"Term.MethodReference($transformedObjTpe, $obj, ${field.value}, $transformedMethodType)"
          }
          case None => throw FieldDoesNotExist(clazz.name.value, field.value, field.pos)
        }
      }
    }
  }

  // Transforms a list of terms and keeps accumulating the class table
  // Returns a tuple containing the list of transformed arguments and the new class table
  private def transformSeq(terms: List[Term]): List[Term] =
    terms.map(transformer(_))

  private def makeMethodCall(obj: Term, method: Method, maybeTypeArgs: Option[List[Type]], args: List[Term], objTpe: Type, clazz: ClassOrTrait, exp: Term) = {
    val tparams = method.tparams
    // If no type arguments are provided, then infer the method's type parameters from the provided arguments
    val inferredTypeArgs = Types.inferTypeParams(method, args, env, exp)
    val typeArgs = if (maybeTypeArgs.nonEmpty) maybeTypeArgs.get else inferredTypeArgs
    // Check that the provided type args match the inferred type args
    if (maybeTypeArgs.nonEmpty && !Types.matchSubtypes(typeArgs, inferredTypeArgs))
      throw TypeError(s"Provided type arguments (${typeArgs.mkString(", ")}) do not match inferred type arguments (${inferredTypeArgs.mkString(", ")}) in method call $objTpe.${method.name}", exp.pos)
    // Check that the inferred type parameters match the structure of the method's type parameters
    Types.matchTypeParamsForStructure(tparams, typeArgs, exp)

    // Fill the type arguments in the method
    val filledMethod = method.fillTypeParameters(method.name, Types.tparamsToTypes(tparams), typeArgs)

    // Check that the programmer passed the correct number of arguments and that the types are correct
    filledMethod match {
      case method @ Method(_, _, _, argList, argTypes, _, _) => {
        val defaults = method.defaults.map(_.map(_.transform { case Term.This(_) => obj}.asInstanceOf[Term])) // replace `this` by the object on which we call the method because the `this` in the default value refers to that object
        val extendedArgs = CompilerUtil.extendArgsWithDefaults(args, defaults, exp)
        val expectedArgTypes = argList.map(arg => argTypes.get(arg.name.value).get)
        val providedArgTypes = Types.inferArgTypes(extendedArgs, env)

        if (!Types.matchSubtypes(expectedArgTypes, providedArgTypes))
          throw TypeError(s"Wrong arguments for method $objTpe.${method.name}\n${Types.makeExpectedVsActualTypeStr(argList, providedArgTypes)}", exp.pos)

        val argumentTypes = argList.map(_.decltpe.get)

        // Transform the call
        val transformedObjTpe = Types.transformer(clazz.getType)
        val transformedObj = transformer(obj)
        val transformedArgs = transformSeq(extendedArgs)
        val transformedTypeArgs = typeArgs.map(Types.transformer(_))
        val transformedArgTypes = argumentTypes.map(Types.transformer(_))

        q"Term.MethodCall($transformedObjTpe, $transformedObj, ${method.name.value}, List(..$transformedTypeArgs), List(..$transformedArgs), List(..$transformedArgTypes))"
      }
    }
  }

  private def makeMethodCallWithTypeArgs(obj: Term, methodName: Term.Name, typeArgs: List[Type], args: List[Term]) = {
    val objTpe = Types.inferType(obj, env)

    Types.getClassM(objTpe, classTable, thisClass) match {
      case Some(clazz) => {
        clazz.getMethod(methodName.value) match {
          case Some(method) => makeMethodCall(obj, method, Some(typeArgs), args, objTpe, clazz, methodName)
          case None => throw MethodNotFound(methodName.value, clazz.name.value, methodName.pos)
        }
      }
      case None => throw ClassNotFound(Types.getClassName(objTpe), obj.pos)
    }
  }

  // Compiles the type cast to a regular method call of the `asInstanceOf` method
  private def makeTypeCast(obj: Term, tpe: Type): Term = {
    val objTpe = Types.inferType(obj, env)

    // Only allow typecasting to a subtype or a supertype
    // because this may be needed by enumerations
    // for example, if we have an enumeration `enum Op { Ins(pos: Int, char: String) | Del(pos: Int) }`
    // we may need sometimes to typecast an `Op` to an `Ins`.
    // Since types are covariant in VeriFx, we can also typecast a `Set[Op]` to a `Set[Ins]` because `Set[Op] <:< Set[Ins]`.
    if (!(tpe <:< objTpe || objTpe <:< tpe))
      throw TypeError(s"Illegal typecast. Can only cast to a subtype or super type.\n`$obj` is of type `$objTpe` which is not a subtype or supertype of `$tpe`", tpe.pos)

    val transformedObjTpe = Types.transformer(objTpe)
    val transformedObj = transformer(obj)
    val transformedTypeArg = Types.transformer(tpe)

    q"""Term.MethodCall($transformedObjTpe, $transformedObj, "asInstanceOf", List($transformedTypeArg), List(), List())"""
  }

  private def makeMethodCallOrFunctionCall(obj: Term, methodName: Term.Name, args: List[Term], exp: Term) = {
    // `obj.methodName` can be a method or a field that is a function
    // if it is a method, delegate to `makeMethodCall`, otherwise delegate to `makeFunctionCall`
    val objTpe = Types.inferType(obj, env)

    Types.getClassM(objTpe, classTable, thisClass) match {
      case Some(clazz) => {
        clazz.getMethod(methodName.value) match {
          case Some(method) => makeMethodCall(obj, method, None, args, objTpe, clazz, methodName)
          case None => {
            // check if it is a field
            clazz.getField(methodName.value) match {
              case Some(_) => makeFunctionCall(Term.Select(obj, methodName), args, exp)
              case None => throw MethodNotFound(methodName.value, clazz.name.value, exp.pos)
            }
          }
        }
      }
      case None => throw ClassNotFound(Types.getClassName(objTpe), exp.pos)
    }
  }

  private def makeNew(tpe: Type, args: List[Term], exp: Term): Term = {
    def getExtendedAndTransformedArgs(clazz: Clazz) = {
      // Extend the arguments with the default args
      val extendedArgs = CompilerUtil.extendArgsWithDefaults(args, clazz.defaults, exp)

      // Transform the arguments
      val transformedArgs = transformSeq(extendedArgs)

      (extendedArgs, transformedArgs)
    }

    def checkArgumentTypes(clazz: Clazz, args: List[Term]) = {
      // Check that the arguments match the argument's types
      val expectedArgTypes = clazz.fieldNames.map(arg => clazz.fields.get(arg).get.tpe)
      val actualArgTypes = Types.inferArgTypes(args, env)
      if (!Types.matchSubtypes(expectedArgTypes, actualArgTypes))
        throw TypeError(s"Wrong arguments passed to constructor $tpe\n${Types.makeExpectedVsActualTypeStr(expectedArgTypes, actualArgTypes)}", tpe.pos)
    }

    tpe match {
      case t: Type.Name => {
        // Check if class exists and infer the type parameters
        classTable.get(t.value) match {
          case None => throw ClassNotFound(t.value, tpe.pos)
          case Some(classOrTrait) => {
            classOrTrait match {
              case _: Trait => throw TypeError(s"Cannot instantiate a trait", exp.pos)
              case clazz: Clazz => {
                // Infer the type parameters from the provided arguments
                val typeArgs = Types.inferTypeParams(clazz, args, env, exp)

                if (typeArgs.isEmpty) {
                  // Add the default arguments
                  val (extendedArgs, transformedArgs) = getExtendedAndTransformedArgs(clazz)
                  checkArgumentTypes(clazz, extendedArgs)
                  q"Term.InstantiateClass(${t.value}, Nil, List(..$transformedArgs))"
                }
                else {
                  makeNew(Type.Apply(t, typeArgs), args, exp)
                }
              }
            }
          }
        }
      }
      case classType @ Type.Apply(Type.Name(className), typeArgs) => {
        classTable.getConcrete(classType) match {
          case None => throw ClassNotFound(className, exp.pos)
          case Some(classOrTrait) => {
            classOrTrait match {
              case _: Trait => throw TypeError(s"Cannot instantiate a trait", exp.pos)
              case clazz: Clazz => {
                // Check that the provided type parameters match the kind's type parameters
                val expectedTypeParams = clazz.tparams
                Types.matchTypeParamsForStructure(expectedTypeParams, typeArgs, exp)

                // Transform the type parameters to IR types
                val irTypeArgs = typeArgs.map(Types.transformer(_))

                // Transform the arguments
                val (extendedArgs, transformedArgs) = getExtendedAndTransformedArgs(clazz)

                checkArgumentTypes(clazz, extendedArgs)

                // Instantiate this concrete class with the given arguments
                q"Term.InstantiateClass($className, List(..$irTypeArgs), List(..$transformedArgs))"
              }
            }
          }
        }
      }
      case _ => throw TypeError(s"Illegal type $tpe", tpe.pos)
    }
  }

  /**
   * A block is a sequence of statements that evaluates to the last statement.
   * e.g.
   * {{{
   *   {
   *      val x = 2
   *      val y = 3
   *      println("x is: " + x)
   *      val z = 1
   *      x + y + z
   *   }
   * }}}
   *
   * `makeBlock` transforms a block into its intermediate representation `IR.Term.Block`
   */
  private def makeBlock(block: Term.Block): Term = {
    def makeValDef(valDef: Defn.Val): Term = {
      valDef match {
        case Defn.Val(Nil, List(Pat.Var(Term.Name(valName))), maybeValTpe, value) => {
          val inferredValueType = Types.inferType(value, env)

          if (maybeValTpe.isDefined && !(inferredValueType <:< maybeValTpe.get))
            throw TypeError(s"Inferred type $inferredValueType does not match declared type ${maybeValTpe.get}", valDef.pos)

          // Transform value definition into a let whose body is the transformation of the rest of the block
          val irValueType = Types.transformer(inferredValueType)
          val irVal = transformer(value)

          env = env + (valName -> inferredValueType) // This is the only place where we mutate the environment! Otherwise we need to return it from every function and pass it to every function call...
          q"Defn.ValDef($valName, $irValueType, $irVal)"
        }
        case Defn.Val(_ :: _, _, _, _) => throw ParseError(s"Modifiers are not supported for value definitions.", valDef.pos)
        case _ => throw ParseError(s"Malformed value definition: $valDef", valDef.pos)
      }
    }

    if (block.stats.isEmpty)
      throw ParseError(s"Empty block in $block", block.pos)

    if (block.stats.last.isInstanceOf[Defn.Val])
      throw ParseError(s"Last expression of a block cannot be a value definition", block.pos)

    val oldEnv = env

    val transformedStats = block.stats.map {
      case valDef: Defn.Val => makeValDef(valDef) // `makeValDef` will extend (i.e. mutate) the environment
      case term: Term => transformer(term)
      case exp => throw ParseError(s"Statements in a block must be terms or value definitions, but found $exp", exp.pos)
    }

    env = oldEnv // we're at the end of the block (i.e. exiting this scope) so we reset the environment to the old one

    q"Term.Block(List(..$transformedStats))"
  }

  private def makeIf(cond: Term, thenB: Term, elseB: Term, fi: Term): Term = {
    val condTpe = Types.inferType(cond, env)
    if (!condTpe.isEqual(Type.Name("Boolean"))) throw TypeError(s"If condition must be of type Boolean but is of type $condTpe", fi.pos)

    val thenTpe = Types.inferType(thenB, env)
    val elseTpe = Types.inferType(elseB, env)

    Types.getLeastCommonType(List(thenTpe, elseTpe)) match {
      case Some(retTpe) => {
        // This common type should not be a trait because traits don't exist in Z3
        // so in Z3 they would actually be different types (because only the subtypes exist)
        val allTParams = methodTParams ++ classTParams
        if (classTable.isTrait(retTpe, allTParams) && !classTable.isSealedTrait(retTpe, allTParams))
          throw TypeError(s"If expression has type `$retTpe` which is a trait and is not allowed.", fi.pos)

        val List(tCond, tThen, tElse) = transformSeq(List(cond, thenB, elseB))
        q"Term.If($tCond, $tThen, $tElse)"
      }
      case None => {
        // Then and else branch have different types
        throw TypeError(s"Then branch and else branch have no common type (`$thenTpe` and `$elseTpe`)", fi.pos)
      }
    }
  }

  private def makeLiteral(lit: Lit) = lit match {
    case Lit.Unit() | Lit.Null() | Lit.Double(_) | Lit.Float(_) | Lit.Long(_) | Lit.Byte(_) | Lit.Short(_) | Lit.Char(_) | Lit.Symbol(_) =>
      throw ParseError(s"Illegal literal $lit. Only booleans, integers, and strings are supported.", lit.pos)
    case Lit.Boolean(bool) => q"Term.BoolL($bool)"
    case Lit.Int(value)    => q"Term.IntL($value)"
    case Lit.String(value) => q"Term.StringL($value)"
  }

  def getArgumentTypeAndTransform(identifier: String): Option[Term] =
    env.get(identifier).map(Types.transformer(_))

  private def makeImplication(antecedent: Term, args: List[Term]): Term = {
    if (args.length != 1) throw ParseError(s"Implication expects 1 antecedent and 1 consequent but got ${args.length} consequents.", antecedent.pos)
    val consequent = args.head
    val antecedentTpe = Types.inferType(antecedent, env)
    val consequentTpe = Types.inferType(antecedent, env)

    if (!(antecedentTpe <:< Type.Name("Boolean"))) throw TypeError(s"Antecedent of a logical implication must evaluate to a boolean, but $antecedent is of type $antecedentTpe", antecedent.pos)
    if (!(consequentTpe <:< Type.Name("Boolean"))) throw TypeError(s"Consequent of a logical implication must evaluate to a boolean, but $consequent is of type $consequentTpe", consequent.pos)

    val tAntecedent = transformer(antecedent)
    val tConsequent = transformer(consequent)

    q"Logic.Implication($tAntecedent, $tConsequent)"
  }

  private def makeInfixCall(lhs: Term, op: Term.Name, args: List[Term]): Term = {
    if (args.length != 1) throw TypeError(s"Operation $op expects 1 right-hand side argument but got ${args.length}.", op.pos)

    val rhs = args.head
    val lhsType = Types.inferType(lhs, env)
    val rhsType = Types.inferType(rhs, env)

    def expectType(expectedType: Type) = {
      if (!(lhsType <:< expectedType)) throw TypeError(s"Operator $op expects $expectedType but $lhs is of type $lhsType", op.pos)
      if (!(rhsType <:< expectedType)) throw TypeError(s"Operator $op expects $expectedType but $rhs is of type $rhsType", op.pos)
    }

    if (CompilerUtil.isArithmeticOperator(op.value) || CompilerUtil.isArithmeticComparisonOperator(op.value)) {
      expectType(Type.Name("Int"))
    }
    else if (CompilerUtil.isBooleanOperator(op.value)) {
      expectType(Type.Name("Boolean"))
    }
    else if (op.value == "==" || op.value == "!=") {
      // force lhs and rhs to have the same type,
      // otherwise Z3 will complain
      // if lhsType <:< rhsType or rhsType <:< lhsType
      // then we are comparing enumerations (which have the same enumeration type in Z3)
      // it cannot be something else than an enumeration because classes cannot extend other classes
      if (!(lhsType <:< rhsType || rhsType <:< lhsType))
        throw TypeError(s"Arguments passed to operator $op must have the same type but lhs has type $lhsType and rhs has type $rhsType", op.pos)
    }

    val tLhs = transformer(lhs)
    val tRhs = transformer(rhs)

    val operators = Map(
      "||" -> q"Op.Or", "&&" -> q"Op.And",
      "==" -> q"Op.Equals", "!=" -> q"Op.NotEquals",
      "+"  -> q"Op.Plus", "-"  -> q"Op.Minus", "*"  -> q"Op.Times", "/"  -> q"Op.Divide",
      "<"  -> q"Op.SmallerThan", "<=" -> q"Op.SmallerOrEqual", ">"  -> q"Op.BiggerThan", ">=" -> q"Op.BiggerOrEqual")

    operators.get(op.value) match {
      case Some(operator) => q"Term.BinaryOperation($operator, $tLhs, $tRhs)"
      case None =>
        lhsType match {
          case Type.Name("Boolean") | Type.Name("Int") | Type.Name("String") =>
            throw IllegalOperationError(s"Operator $op does not exist on value $lhs which is of type $lhsType", op.pos)
          case _ => transformer(Term.Apply(Term.Select(lhs, op), args)) // user-defined class, transform infix notation to a regular method call
        }
    }
  }

  private def makePrefixCall(op: Term.Name, arg: Term): Term = {
    val operator = op.value
    if (operator != "!" && operator != "-")
      throw ParseError(s"Unknown unary operator $op", op.pos)

    val tArg = transformer(arg)
    if (operator == "!")
      q"Term.UnaryOperation(Op.Not, $tArg)"
    else
      q"Term.UnaryOperation(Op.Negative, $tArg)"
  }

  private def makeVar(term: Term) = term match {
    case Term.Ascribe(Term.Name(name), tpe) => {
      val irType = Types.transformer(tpe)
      q"Logic.Variable($name, $irType)"
    }
    case _ => throw ParseError(s"Malformed logical variable: $term", term.pos)
  }

  private def makeQuantifiedFormula(vars: List[Term], body: Term, quantifierStr: String)(exp: Term): Term = {
    val irVars = vars.map(makeVar)
    val extendedEnv = CompilerUtil.varsToTypeMap(vars, env)
    // parse the body using the extended environment
    val irBody = MethodBody(extendedEnv).transformer(body)
    val quantifier = Term.Name(quantifierStr)
    q"Logic.$quantifier(Set(..$irVars), $irBody)"
  }

  private def makeFunctionCall(fun: Term, args: List[Term], exp: Term): Term = {
    Types.inferType(fun, env) match {
      case Type.Function(params, _) => {
        // Check that the right number of arguments is provided and that they have the right type
        val argTypes = Types.inferArgTypes(args, env)
        Types.matchSubtypes(params, argTypes)
        // Transform the function call to IR
        val irArgs = transformSeq(args)
        val irFun = transformer(fun)
        q"Term.FunctionCall($irFun, List(..$irArgs))"
      }
      case tpe => throw TypeError(s"Cannot apply $fun because it is of type $tpe, expected a function.", fun.pos)
    }
  }

  // Compiles lambdas to IR
  private def makeFunction(params: List[Term.Param], body: Term): Term = {
    if (params.isEmpty) throw TypeError(s"Function should take at least one argument. Otherwise, make it a constant.", body.pos)
    val irParams = params map IRConversions.termParam2IR
    // Extend the environment with the function parameters
    val extendedEnv = env ++ CompilerUtil.termParamsToTypeMap(params)
    val irBody = MethodBody(extendedEnv).transformer(body)
    q"Term.Function(List(..$irParams), $irBody)"
  }

  /**
   * Fetches the name of the data constructor from a pattern.
   */
  private def getCtorName(pat: Pat): String = pat match {
    case Pat.Typed(_, tpe) => Types.getClassName(tpe)
    case Pat.Extract(Term.Name(className), _) => className
    case _ => throw ParseError(s"Unsupported pattern $pat", pat.pos)
  }

  /**
   * Checks if the pattern match is exhaustive,
   * i.e. the cases must cover all possible data constructors.
   * @param cases Cases of the pattern match expression
   * @param ctorNames Name of all data constructors for this enumeration type
   * @return Returns a set of data constructors that are not covered.
   */
  @tailrec
  private def patternMatchIsExhaustive(cases: List[Case], ctorNames: Set[String]): Set[String] = cases match {
    case Nil => ctorNames
    case Case(Pat.Wildcard(), None, _) :: Nil => Set() // wildcard pattern matches everything but should be the last case
    case Case(w @ Pat.Wildcard(), None, _) :: _ => throw ParseError(s"Wildcard pattern should be the last one, otherwise the remaining cases are unreachable.", w.pos) // wildcard pattern is not the last one
    case Case(pat, None, _) :: xs => {
      val ctorName = getCtorName(pat)
      patternMatchIsExhaustive(xs, ctorNames - ctorName)
    }
    case Case(_, Some(guard), _) :: _ => throw ParseError(s"Guards are not supported on cases.", guard.pos)
    case x :: _ => throw ParseError(s"Expected a case but got something else.", x.pos)
  }

  // Transforms the pattern and introduces the new constants in the environment
  private def transformPattern(p: Pat, enumType: Type): (Term, Map[String, Type]) = {
    def transformVarOrWildcard(p: Pat): Term = p match {
      case Pat.Var(Term.Name(name)) => q"Pat.Var($name)"
      case Pat.Wildcard() => q"Pat.Wildcard"
      case _ => throw new InternalError(s"Expected a variable or wildcard pattern but got something else: $p")
    }

    val newEnv = Types.extendEnvWithPattern(p, env, enumType)
    val irPattern = p match {
      case Pat.Typed(lhs, tpe) => {
        if (tpe <:< enumType) {
          val irType = Types.transformer(tpe)
          val varOrWildcardIrPat = transformVarOrWildcard(lhs)
          q"Pat.Typed($varOrWildcardIrPat, $irType)"
        }
        else
          throw TypeError(s"Pattern with type `$tpe` will never match expression of type `$enumType`", tpe.pos)
      }
      case Pat.Wildcard() => q"Pat.Wildcard"
      case Pat.Extract(ctorTerm @ Term.Name(className), vars) => {
        // This extraction pattern contains a constructor name `className`
        // and a number of variables `vars` which can be either `Pat.Var` or `Pat.Wildcard`
        // e.g.: case Insert(pos, _) => ...
        // in the above example "Insert" is the `className`, `pos: Int` is a `Pat.Var` and `_` is a `Pat.Wildcard`
        classTable.get(className) match {
          case Some(Clazz(_, tparams, _, _, _, _, _, _, _)) => {
            val enumTParams = Types.getTypeArgs(enumType)
            if (tparams.length != enumTParams.length) throw TypeError(s"$className will never match expression of type $enumType", ctorTerm.pos)
            // add the type parameters inferred from the expression to this class
            val clazzType = if (tparams.nonEmpty) Type.Apply(Type.Name(className), enumTParams) else Type.Name(className)

            if (clazzType <:< enumType) {
              val irType = Types.transformer(clazzType)
              val irVars = vars map transformVarOrWildcard
              q"Pat.Extract($irType, List(..$irVars))"
            }
            else
              throw TypeError(s"Pattern with type `$clazzType` will never match expression of type `$enumType`", ctorTerm.pos)
          }
          case Some(_) => throw MatchError("Pattern matching is allowed on algebraic data types only.", ctorTerm.pos)
          case None => throw ClassNotFound(className, ctorTerm.pos)
        }
      }
      case _ => throw ParseError(s"Pattern not supported.", p.pos)
    }

    (irPattern, newEnv)
  }

  private def transformCase(c: Case, enumType: Type) = c match {
    case Case(pat, None, body) => {
      val (irPat, newEnv) = transformPattern(pat, enumType)
      val irBody = MethodBody(newEnv).transformer(body) // transform the body of the case using the extended environment that contains the constants introduced by the pattern
      q"Term.Case($irPat, $irBody)"
    }
    case _ => throw ParseError(s"Expected a case but got something else.", c.pos)
  }

  private def makePatternMatch(exp: Term, cases: List[Case], matchExpression: Term): Term = {
    val expTpe = Types.inferType(exp, env)
    if (!expTpe.isInstanceOf[Type.Name] && !expTpe.isInstanceOf[Type.Apply])
      throw TypeError(s"Pattern matching is allowed on enumerations only. $exp is of type $expTpe", exp.pos)

    val patMatchTpe = Types.inferType(matchExpression, env) // will throw an error if the cases have no common type

    // Make sure the type of the pattern match is not a trait (because those types do not exist in Z3)
    // sealed traits are fine because they are enumerations and those types exist in Z3
    val allTParams = methodTParams ++ classTParams
    if (classTable.isTrait(patMatchTpe, allTParams) && !classTable.isSealedTrait(patMatchTpe, allTParams))
      throw TypeError(s"Pattern match expression evaluates to a trait type (`$patMatchTpe`) which is not allowed.", matchExpression.pos)

    val enumName = Types.getClassName(expTpe)
    classTable.enums.get(enumName) match {
      case Some(ctorNames) => {
        // Enforce pattern matching to be exhaustive
        val notCoveredCtors = patternMatchIsExhaustive(cases, ctorNames)
        if (notCoveredCtors.isEmpty) {
          // pattern match is exhaustive
          // transform to IR
          val irExp = transformer(exp)
          val irCases = cases.map(transformCase(_, expTpe))
          q"Term.Match($irExp, List(..$irCases))"
        }
        else {
          // pattern match is not exhaustive, throw an error
          val (notCoveredCtorsPart1, lastNotCoveredCtor) = notCoveredCtors.splitAt(notCoveredCtors.size - 1)
          val ctorsList =
            if (notCoveredCtorsPart1.isEmpty)
              lastNotCoveredCtor.head
            else {
              val p1 = notCoveredCtorsPart1.mkString(", ")
              s"$p1 and ${lastNotCoveredCtor.head}"
            }

          throw MatchError(s"Pattern match is not exhaustive. Missing case(s) for $ctorsList", exp.pos)
        }
      }
      case None => throw TypeError(s"Pattern matching is allowed on enumerations only, `$exp` is of type `$enumName` which is not an enumeration.", exp.pos)
    }
  }

  def transformer(term: Term): Term = term match {
    case Term.Name(name) => q"Term.Reference($name)"
    case Term.This(_) => q"""Term.Reference("this")"""
    case Term.Select(qual, field) => transformSelect(qual, field)
    case Term.Eta(Term.Select(qual, field)) => makeMethodRef(qual, field)
    case Term.ApplyType(Term.Select(obj, Term.Name("asInstanceOf")), List(tpe)) => makeTypeCast(obj, tpe)
    case Term.Apply(Term.Select(obj, method), args) => makeMethodCallOrFunctionCall(obj, method, args, term) // regular method call `obj.method(args)`
    case Term.Apply(Term.ApplyType(Term.Select(obj, method), typeArgs), args) => makeMethodCallWithTypeArgs(obj, method, typeArgs, args) // method call with explicit type arguments `obj.method[T](args)`
    case Term.Apply(Term.Apply(Term.Name(quantifier), vars), List(body @ Term.Block(_))) if quantifier == "forall" || quantifier == "exists" =>
      makeQuantifiedFormula(vars, body, quantifier.capitalize)(term)
    case Term.Apply(fun, args) => makeFunctionCall(fun, args, term)
    case Term.ApplyInfix(antecedent, Term.Name("=>:"), _, args) => makeImplication(antecedent, args)
    case Term.ApplyInfix(lhs, op, _, args) => makeInfixCall(lhs, op, args)
    case Term.ApplyUnary(op, arg) => makePrefixCall(op, arg) // prefix operators like !a, -a, ~a
    case Term.New(Init(tpe, _, Nil)) => makeNew(tpe, List(), term) // `new Foo` is parsed as `Term.New(Init(Type.Name("foo"), Name(""), Nil))`
    case Term.New(Init(tpe, _, args :: Nil)) => makeNew(tpe, args, term) // `new Foo(1, 2)` is parsed as `Term.New(Init(Type.Name("Foo"), Name(""), List(List(Lit.Int(1), Lit.Int(2)))))`
    case Term.New(Init(_, _, _ :: _)) => throw ParseError(s"Multiple argument lists are not supported", term.pos)
    case block: Term.Block => makeBlock(block)
    case fi @ Term.If(cond, thenB, elseB) => makeIf(cond, thenB, elseB, fi)
    case lit: Lit => makeLiteral(lit)
    case Term.Function(params, body) => makeFunction(params, body)
    case m @ Term.Match(exp, cases) => makePatternMatch(exp, cases, m)
    case Term.ApplyType(Term.Name("Dummy"), targs) => {
      // `Dummy` is a special function that is used by built-in classes
      // built-in classes are not compiled to the target language
      // so we can return a dummy value
      if (targs.size == 1) q"Term.BoolL(true)"
      else throw ParseError(s"Dummy expects exactly one type argument but got ${targs.size}", term.pos)
    }
    case node => throw ParseError(s"Illegal expression: $node", node.pos)
  }
}

object MethodPredicate {
  def isMethodPred(stat: Stat): Boolean = stat match {
    case Term.ApplyInfix(Term.Name(pred), _, _, _) =>
      pred == "ctx" || pred == "pre" || pred == "inv"
    case _ => false
  }

  def isMethodCtx(stat: Stat): Boolean = stat match {
    case Term.ApplyInfix(Term.Name("ctx"), _, _, _) => true
    case _ => false
  }

  def isMethodPre(stat: Stat): Boolean = stat match {
    case Term.ApplyInfix(Term.Name("pre"), _, _, _) => true
    case _ => false
  }

  def isMethodInv(stat: Stat): Boolean = stat match {
    case Term.ApplyInfix(Term.Name("inv"), _, _, _) => true
    case _ => false
  }

  def getMethodName(stat: Stat): String = stat match {
    case Term.ApplyInfix(_, Term.Name(method), _, _) => method
    case _ => throw IllegalArgumentException(s"Expected a method predicate but got something else: $stat", stat.pos)
  }

  def getPredArgsAndBody(infixArgs: List[Term]): (List[Term], Term) = {
    // Depending on the number of provided arguments the pattern is different
    // because ScalaMeta parses our predicate definitions as infix operations
    // so the right arguments passed to the infix operation will be different
    infixArgs match {
      // omitted argument list leads to this pattern
      case List(body @ Term.Block(_)) => (List(), body)
      // empty argument list leads to this pattern
      case List(Term.Apply(Lit.Unit(), List(body))) => (List(), body)
      // argument list with only 1 argument leads to this pattern
      case List(Term.Apply(ascribe @ Term.Ascribe(_, _), List(body))) => (List(ascribe), body)
      // argument list with several arguments leads to this pattern
      case List(Term.Apply(Term.Tuple(args), List(body))) => (args, body)
    }
  }

  def computePredDependencies(stat: Stat): Set[String] = {
    assert(isMethodPred(stat), s"Expected a predicate but got something else: $stat")
    stat match {
      case Term.ApplyInfix(_, Term.Name(method), _, infixArgs) => {
        val body = getPredArgsAndBody(infixArgs)._2
        MethodCompiler.computeDependenciesFromBody(method, body)
      }
    }
  }
}

case class MethodPredicate()(implicit thisClass: Type, classTable: Classes) {
  /**
   * Transforms predicates that are associated to methods to their intermediate representation.
   * e.g. `ctx someMethod[A, B, N](arg1: A, arg2: B, ..., argN: N) { <body> }`
   * is transformed into `IR.Defn.CtxDef("someMethod", List["A", "B", "N"], List(arg1, arg2, ..., argN), body)`
   */
  def transformer(predDef: Term): Term = predDef match {
    case Term.ApplyInfix(pred, method, typeParams, infixArgs) => {
      val methodName = method.value
      val maybeMethod = classTable.get(thisClass).get.getMethod(methodName)

      // Check that the method exists
      if (maybeMethod.isEmpty)
        throw ParseError(s"Cannot associate predicate to inexistent method ${Types.getClassName(thisClass)}.$methodName", predDef.pos)

      val methodTParams = maybeMethod.get.tparams
      // Check that the predicate has the right number of type parameters
      if (methodTParams.length != typeParams.length)
        throw TypeError(s"Method $methodName has ${methodTParams.length} type parameters but predicate has ${typeParams.length} type parameters", predDef.pos)
      // Check that the predicate's type parameters are the same as those of the associated method
      val methodTypeParams = Types.tparamsToTypes(methodTParams)
      val paramsAreTheSame = (methodTypeParams zip typeParams).forall { case (p1, p2) => p1 =:= p2 }
      if (!paramsAreTheSame)
        throw TypeError(s"Predicate's type parameters are different from the associated method's ($methodName) type parameters", predDef.pos)

      // Check that there are no duplicate type parameters
      val tparams = typeParams.map(Types.typeToTParam)
      CompilerUtil.checkTParams(tparams)

      // Depending on the number of provided arguments the pattern is different
      // because ScalaMeta parses our predicate definitions as infix operations
      // so the right arguments passed to the infix operation will be different
      val (args, body) = infixArgs match {
        // omitted argument list leads to this pattern
        case List(body @ Term.Block(_)) => (List(), body)
        // empty argument list leads to this pattern
        case List(Term.Apply(Lit.Unit(), List(body))) => (List(), body)
        // argument list with only 1 argument leads to this pattern
        case List(Term.Apply(ascribe @ Term.Ascribe(_, _), List(body))) => (List(ascribe), body)
        // argument list with several arguments leads to this pattern
        case List(Term.Apply(Term.Tuple(args), List(body))) => (args, body)
      }

      if (!args.forall(t => t.isInstanceOf[Term.Ascribe] && t.asInstanceOf[Term.Ascribe].expr.isInstanceOf[Term.Name]))
        throw ParseError(s"Malformed $pred for method $method, invalid arguments.", predDef.pos)

      // Check that the number of arguments corresponds with how many arguments the method expects
      val classOrTrait = classTable.get(thisClass).get
      val internalMethod = classOrTrait.getMethod(methodName).get
      if (args.length != internalMethod.argList.length)
        throw TypeError(s"$pred on method $method has ${args.length} arguments, expected ${internalMethod.argList.length} arguments.", predDef.pos)

      val argDeclaredTypes = args.asInstanceOf[List[Term.Ascribe]].map(ascribe => ascribe.tpe)
      val methodArgDeclaredTypes = internalMethod.argList.map(param => param.decltpe.get)
      if (!Types.matchSubtypes(methodArgDeclaredTypes, argDeclaredTypes))
        throw TypeError(s"Type mismatch for arguments in $pred on method $method\n${Types.makeExpectedVsActualTypeStr(internalMethod.argList, args)}", predDef.pos)

      // Transform `args` which is a list of Term.Ascribe into a list of ESL_Identifier
      val arguments = args.asInstanceOf[List[Term.Ascribe]].map(IRConversions.ascribe2IR(_)(thisClass, Some(methodName), classTable))

      val argTypes = CompilerUtil.ascribesToTypeMap(args.asInstanceOf[List[Term.Ascribe]])
      val argumentTypes = pred match {
        case Term.Name("pre") => argTypes + ("this" -> thisClass)
        case _ => argTypes + ("this" -> thisClass) + ("old" -> thisClass)
      }

      implicit val thisMethod = Some(methodName)
      val b = MethodBody(argumentTypes).transformer(body)
      val irTypeParams = tparams.map(IRConversions.typeParam2IR)

      pred match {
        case Term.Name("ctx") => q"Defn.CtxDef($methodName, List(..$irTypeParams), List(..$arguments), $b)"
        case Term.Name("pre") => q"Defn.PreDef($methodName, List(..$irTypeParams), List(..$arguments), $b)"
        case Term.Name("inv") => q"Defn.InvDef($methodName, List(..$irTypeParams), List(..$arguments), $b)"
      }
    }
    case node => {
      throw ParseError(s"Illegal expression: $node", node.pos)
    }
  }
}

object MethodCompiler {
  def isValue(stat: Tree): Boolean = stat.isInstanceOf[Defn.Val]
  def isValueDeclaration(stat: Tree): Boolean = stat.isInstanceOf[Decl.Val]
  def isMethod(stat: Tree): Boolean = stat.isInstanceOf[Defn.Def]
  def isMethodDeclaration(stat: Tree): Boolean = stat.isInstanceOf[Decl.Def]
  def getMethodName(stat: Defn.Def): String = stat.name.value

  def computeDependenciesFromBody(methodName: String, body: Term): Set[String] = {
    // Compute all dependencies based on method calls of the form `this.method()`
    // except for recursive calls
    body.collect {
      case Term.Apply(Term.Select(Term.This(_), Term.Name(methodName)), _) => methodName // regular method call
      case Term.Eta(Term.Select(Term.This(_), Term.Name(methodName))) => methodName // method reference
    }.toSet - methodName
  }

  def computeDependencies(method: Stat): Set[String] = {
    method match {
      case Defn.Def(_, Term.Name(methodName), _, _, _, body) =>
        computeDependenciesFromBody(methodName, body)
      case _: Decl.Def => Set()
      case stat => throw new InternalError(s"Expected a method definition or declaration but got something else:\n$stat")
    }
  }
}

case class MethodCompiler()(implicit thisClass: Type, classTable: Classes) {

  /**
   * Transforms a method declaration into an IR [[IR.Decl.Method]].
   */
  def transformer(method: Decl.Def): (Term, Classes) = {
    // Method declarations may not be private or have override modifier
    method.mods.find(mod => !isAnnotation(mod) && !isProtectedModifier(mod) && !isPrivateModifier(mod)) match {
      case Some(mod) => throw ParseError(s"Illegal modifier $mod on method:\n$thisClass.$method", mod.pos)
      case None => ()
    }

    val modsIR = getModsIR(method.mods)

    // Check that there are no duplicate type parameters
    val tparams = method.tparams
    CompilerUtil.checkTParams(tparams)

    val name = method.name.value
    val retType = method.decltpe

    // Already add the method to the class in the class table because we may need the method's type parameters to transform the argument's types
    val extendedClassTable = classTable.get(thisClass).get match {
      case traitt: Trait => {
        val newMethods = traitt.abstractMethods + (name -> Method(method.mods, method.name, tparams, List(), Map(), retType, method))
        val newTraitt = traitt.copy(abstractMethods = newMethods)
        classTable + (Types.getClassName(thisClass), newTraitt)
      }
      case _ => throw new InternalError(s"Method declarations are only allowed on traits.")
    }

    val (args, irArgs, argTypes) = getArgInfo(method.paramss)(thisClass, Some(name), extendedClassTable)

    // ensure that the argument types and the return type is concrete
    (retType :: args.map(_.decltpe.get)).foreach(Types.isConcrete(_)(thisClass, Some(name), extendedClassTable))

    val internalMethod = Method(method.mods, method.name, tparams, args, argTypes, retType, method)

    // Update this trait in the class table
    val newClassTable = extendedClassTable.get(thisClass).get match {
      case traitt: Trait => {
        val newMethods = traitt.abstractMethods + (name -> internalMethod)
        val newTraitt = traitt.copy(abstractMethods = newMethods)
        extendedClassTable + (Types.getClassName(thisClass), newTraitt)
      }
      case _ => throw new InternalError(s"Method declarations are only allowed on traits.")
    }

    val retTypeIR = Types.transformer(retType)(thisClass, Some(name), newClassTable)
    val irTypeParams = tparams.map(tparam => IRConversions.typeParam2IR(tparam)(thisClass, Some(name), newClassTable))

    val methodDeclIR = q"Decl.Method($name, List(..$modsIR), List(..$irTypeParams), List(..$irArgs), $retTypeIR)"

    (methodDeclIR, newClassTable)
  }

  /**
   * Transforms a method definition into an IR [[IR.Defn.MethodDef]].
   */
  def transformer(method: Defn.Def): (Term, Classes) = {
    val recursive = method.mods.exists(isRecursiveAnnotation)

    method.mods.find(mod => !isAnnotation(mod) && !isProtectedModifier(mod) && !isPrivateModifier(mod) && !isOverrideModifier(mod)) match {
      case Some(mod) => throw ParseError(s"Illegal modifier $mod on method: \n$thisClass.${method.name}", mod.pos)
      case None => ()
    }

    val mods = getModsIR(method.mods)

    // Check that there are no duplicate type parameters
    val tparams = method.tparams
    CompilerUtil.checkTParams(tparams)

    val name = method.name.value
    val declaredRetType = method.decltpe

    val extendedClassTable = {
      val clazz = classTable.get(thisClass).get
      // the return type may not be declared, in that case we fill in `null`
      // if the return type is declared then we use it because it is needed to infer the return type of recursive methods
      val returnType = declaredRetType.getOrElse(Type.Name("NOT DECLARED AND NOT YET INFERRED"))
      val newMethods = clazz.methods + (name -> Method(method.mods, method.name, tparams, List(), Map(), returnType, method))
      val newClazz = clazz.cpy(methods = newMethods)
      classTable + (Types.getClassName(thisClass), newClazz)
    }

    val (args, irArgs, argTypes) = getArgInfo(method.paramss)(thisClass, Some(name), extendedClassTable)

    // Update the method in the class table with the arguments and their types
    val newClassTable = {
      val clazz = extendedClassTable.get(thisClass).get
      val method = clazz.getMethod(name).get
      val newMethods = clazz.methods + (name -> method.copy(argList = args, args = argTypes))
      val newClazz = clazz.cpy(methods = newMethods)
      extendedClassTable + (Types.getClassName(thisClass), newClazz)
    }

    // ensure that the argument types and return type are concrete
    // no need to check the return type because we check the inferred return type
    // which will also be the return type of the method
    args.map(_.decltpe.get).foreach(Types.isConcrete(_)(thisClass, Some(name), newClassTable))
    val clazzTParams = newClassTable.get(thisClass).get.tparams

    val argumentTypes = argTypes + ("this" -> thisClass) + ("old" -> thisClass)

    val body = method.body

    if (recursive && declaredRetType.isEmpty)
      throw TypeError(s"Recursive method ${method.name} needs return type.", method.pos)

    val inferredRetType = Types.inferType(body, argumentTypes)(newClassTable)

    // Ensure the return type is concrete
    Types.isConcrete(inferredRetType)(thisClass, Some(name), newClassTable)
    if (newClassTable.isClass(thisClass) && newClassTable.isTrait(inferredRetType, tparams ++ clazzTParams) && !newClassTable.isSealedTrait(inferredRetType, tparams ++ clazzTParams)) // return type may not be a trait, unless it is a sealed trait (because sealed traits are a regular type in Z3) or if this class is a trait it is fine because the return type can still be concretized by the subclass
      throw TypeError(s"Return type must be concrete thus cannot be a trait (`$inferredRetType`).", inferredRetType.pos)

    if (declaredRetType.isDefined && !(inferredRetType <:< declaredRetType.get))
      throw TypeError(s"Inferred return type $inferredRetType for method $thisClass.$name does not match declared return type ${declaredRetType.get}", declaredRetType.get.pos)

    val irTypeParams = tparams.map(tparam => IRConversions.typeParam2IR(tparam)(thisClass, Some(name), newClassTable))
    val b = MethodBody(argumentTypes)(thisClass, Some(name), newClassTable).transformer(body)
    val retType = Types.transformer(inferredRetType)(thisClass, Some(name), newClassTable)

    (q"Defn.MethodDef($name, List(..$mods), List(..$irTypeParams), List(..$irArgs), $b, $retType, $recursive)", newClassTable)
  }

  private def getModsIR(mods: List[Mod]): List[Term] = {
    mods.filter(!isRecursiveAnnotation(_)).map {
      case Mod.Private(_) => q"Mod.Private"
      case Mod.Protected(_) => q"Mod.Protected"
      case Mod.Override() => q"Mod.Override"
      case Mod.Annot(Init(Type.Name(name), _, _)) => q"Mod.Annot($name)"
    }
  }

  /**
   * Takes the method's list of parameters and returns a tuple containing:
   * the first argument list, the transformed arguments, and the argument types.
   */
  private def getArgInfo(paramss: List[List[Term.Param]])(implicit thisClass: Type, methodName: Option[String], classTable: Classes): (List[Term.Param], List[Term], Map[String, Type]) = {
    val args = paramss.headOption.getOrElse(List.empty[Term.Param])
    val irArgs = args.map(IRConversions.termParam2IR(_))
    val argTypes = CompilerUtil.termParamsToTypeMap(args)

    (args, irArgs, argTypes)
  }
}

final case class TraitCompiler()(implicit thisTrait: Type, classTable: Classes) {
  /**
   * IMPORTANT: Only call this method if `thisTrait` is a trait!
   * Because value declarations are only allowed in the body of traits!
   */
  def compileTraitFieldDecl(decl: Decl.Val)(implicit thisTrait: Type, classTable: Classes): (Term, Classes) = decl match {
    case Decl.Val(Nil, List(Pat.Var(Term.Name(name))), declTpe) => {
      implicit val thisMethod: Option[String] = None
      Types.isConcrete(declTpe) // ensure the declared type is a concrete type
      val irType = Types.transformer(declTpe)
      val irDecl = q"Decl.Val($name, $irType)"
      val internalField = Field(Nil, name, declTpe, decl)

      classTable.get(thisTrait).get match {
        case traitt: Trait => {
          if (traitt.fields.get(name).nonEmpty || traitt.abstractFields.get(name).nonEmpty)
            throw ParseError(s"$decl already exists in ${traitt.name}", decl.pos)

          val extendedTrait = traitt.copy(abstractFields = traitt.abstractFields + (name -> internalField))
          val extendedClassTable = classTable + (Types.getClassName(thisTrait) -> extendedTrait)
          (irDecl, extendedClassTable)
        }
        case _ => throw new InternalError(s"compileValueDeclaration can only be called on traits!")
      }
    }
    case _ => throw ParseError(s"Malformed value declaration: $decl", decl.pos)
  }
}

final case class EnumCompiler(classTable: Classes) {
  implicit val thisMethod: Option[String] = None

  case class DataCtor(name: String, tparams: List[Type], fields: List[Field])

  private def containsType(it: Iterable[Type], tpe: Type): Boolean = it.exists(_.isEqual(tpe))

  private def makeConstructor(stat: Term.Apply, enumTParams: List[Type]): DataCtor = stat match {
    case Term.Apply(Term.Name(ctorName), fs) => {
      // `fields` must be a list of `Term.Ascribe`
      val (tparams, fields) = fs.reverse.foldLeft((Set.empty[Type], List.empty[Field])) {
        case ((tparams, fields), term) => term match {
          case a @ Term.Ascribe(Term.Name(fieldName), tpe) => {
            val newFields = Field(Nil, fieldName, tpe, a) :: fields

            // if `tpe` is a type parameter then remember that this ctor needs that type parameter
            // by adding it to the `tparams` set
            val newTparams = if (containsType(tparams, tpe)) tparams else tparams + tpe

            (newTparams, newFields)
          }
          case x => throw ParseError(s"Expected a field `name: Type`", x.pos)
        }
      }

      val orderedTparams = enumTParams.filter(containsType(tparams, _))
      DataCtor(ctorName, orderedTparams, fields)
    }
    case _ => throw ParseError(s"Malformed constructor definition", stat.pos)
  }

  private def makeConstructors(defs: Stat, enumTParams: List[Type]): List[DataCtor] = {
    // `defs` may contain 1 constructor or several constructors separated by `|`
    defs match {
      case ctor: Term.Apply => List(makeConstructor(ctor, enumTParams))
      case Term.ApplyInfix(ctors, Term.Name("|"), Nil, List(c @ Term.Apply(_, _))) => {
        val ctor = makeConstructor(c, enumTParams)
        ctor :: makeConstructors(ctors, enumTParams)
      }
      case _ => throw ParseError("Expected a constructor.", defs.pos)
    }
  }

  private def compileDataCtor(ctor: DataCtor)(implicit thisClass: Type, classTable: Classes): Term = ctor match {
    case DataCtor(name, tparams, fields) => {
      val tparamsIR = tparams.map(Types.typeToTParam).map(IRConversions.typeParam2IR)
      val fieldsIR = fields map {
        case Field(_, name, tpe, _) => {
          val irType = Types.transformer(tpe)
          q"Defn.Field(Nil, $name, $irType)"
        }
      }

      q"Defn.DataCtor($name, List(..$tparamsIR), List(..$fieldsIR))"
    }
  }

  // Transforms the constructor to a class that extends `enumType`
  private def transformConstructorToClass(ctor: DataCtor, enumType: Type): Defn.Class = ctor match {
    case DataCtor(name, _, fields) => {
      val ctorTpe = Type.Name(name)
      val params: List[Term.Param] = fields.map {
        case Field(_, name, tpe, _) =>
          Term.Param(Nil, Term.Name(name), Some(tpe), None)
      }

      val superType = Init(enumType, Name.Anonymous(), Nil)

      enumType match {
        case _: Type.Name => q"class $ctorTpe(..$params) extends $superType {}"
        case Type.Apply(_, tparams) => {
          val tparamss = tparams.map(Types.typeToTParam)
          q"class $ctorTpe[..$tparamss](..$params) extends $superType {}"
        }
      }
    }
  }

  import CompilerUtil._ENUM_KEYWORD_

  def transformer(enum: Term.ApplyInfix): Classes = enum match {
    case Term.ApplyInfix(Term.Name(`_ENUM_KEYWORD_`), Term.Name(enumName), tparams, List(Term.Block(List(ctorDefs)))) => {
      val ctors = makeConstructors(ctorDefs, tparams).reverse

      // Make a sealed trait for the Algebraic Data Type
      val enumType: Type = if (tparams.isEmpty) Type.Name(enumName) else Type.Apply(Type.Name(enumName), tparams)
      val enumTrait =
        if (tparams.isEmpty) {
          q"sealed trait ${Type.Name(enumName)}"
        }
        else {
          val tparamss = tparams.map(Types.typeToTParam)
          q"sealed trait ${Type.Name(enumName)}[..$tparamss]"
        }

      // Transform all constructors to classes that extend the Algebraic Data Type
      val ctorClasses = ctors.map(transformConstructorToClass(_, enumType))
      val traitsAndClasses = enumTrait :: ctorClasses

      // Announce the enumeration such that the classes and traits are not added to the `classDefinitionOrder`
      val classTable2 = classTable.announceEnum(enumTrait.name.value, ctorClasses.map(_.name.value).toSet)

      // Compile the enumeration trait and its subclasses
      implicit val classTable3 = traitsAndClasses.foldLeft(classTable2) {
        case (classTable, classOrTraitDef) => {
          val classOrTrait = ClassCompiler(classTable).transformer(classOrTraitDef)
          classTable + (classOrTrait.name.value -> classOrTrait)
        }
      }

      // Make an IR representation for the enum
      implicit val thisClass = Types.classNameAndTArgsToType(Type.Name(enumName), tparams)
      val tparamsIR = tparams.map(Types.typeToTParam).map(IRConversions.typeParam2IR)
      val ctorsIR = ctors.map(compileDataCtor)

      // Add the compiled enumeration to the class table
      val irEnum = q"Defn.Enum($enumName, List(..$tparamsIR), List(..$ctorsIR))"
      val compiledEnum = Enum(Type.Name(enumName), irEnum)

      classTable3 + compiledEnum
    }
    case _ => throw ParseError("Malformed enumeration", enum.pos)
  }
}

object ProofCompiler {
  def isProof(stat: Stat) = stat match {
    case Term.ApplyInfix(Term.Name("proof"), Term.Name(_), _, List(Term.Block(_))) => true
    case _ => false
  }

  def getProofName(stat: Stat) = stat match {
    case Term.ApplyInfix(Term.Name("proof"), Term.Name(proofName), _, List(Term.Block(_))) => proofName
    case _ => throw new Error(s"Expected a proof but got: $stat")
  }
}

case class ProofCompiler(proofs: Set[Proof])(implicit classTable: Classes) {
  import ProofCompiler._

  private def getName(tpe: Type, exp: Stat): Type.Param = tpe match {
    case Type.Name(name) => Type.Param(Nil, Name(name), Nil, Type.Bounds(None, None), Nil, Nil)
    case Type.Apply(Type.Name(name), args) => Type.Param(Nil, Name(name), args.map(Types.typeToTParam), Type.Bounds(None, None), Nil, Nil)
    case _ => throw ParseError(s"Expected a simple type parameter but got $tpe", exp.pos)
  }

  def transformer(proofObj: Defn.Object): (Classes, Set[Proof]) = {
    val objectName = proofObj.name
    val objInits = proofObj.templ.inits
    val superTrait = ClassCompiler.getSuperTrait(objInits, Type.Name(objectName.value))(classTable)
    val proofsFromSuperTrait: List[Stat] = superTrait.map(_.getAllProofs).getOrElse(List())
    val objStats = proofObj.templ.stats ++ proofsFromSuperTrait // also "inherit" the proofs from the super trait

    objStats.foreach {
      case stat =>
        if (!isProof(stat) && !stat.isInstanceOf[Defn.Def])
          throw ParseError(s"Illegal statement in object, only proofs and methods are allowed in objects", stat.pos)
    }

    // Split the object's stats into methods and proofs
    val (methods, proofStats) = objStats.reverse.foldLeft((List.empty[Defn.Def], Set.empty[Stat])) {
      case ((regularStats, proofStats), stat) => stat match {
        case proof if isProof(proof) => (regularStats, proofStats + proof)
        case method: Defn.Def => (method :: regularStats, proofStats)
        case stat => throw new InternalError(s"Unexpected statement in proof object: $stat")
      }
    }

    proofStats.foldLeft((classTable, proofs)) {
      case ((classTable, compiledProofs), proofStat) => {
        proofStat match {
          case proofDef @ Term.ApplyInfix(Term.Name("proof"), proofTermName @ Term.Name(proofName), tparams, List(proofBody)) => {
            // Check that this proof name is unique
            val objName = objectName.asInstanceOf[Term.Name].value
            if (proofs.exists(p => p.name == proofName && p.containerObject == objName))
              throw ParseError(s"Cannot redefine proof $proofName in object $objName", proofDef.pos)

            // Make a dummy class with type parameters `tparams`
            // and add this proof as a method of that class
            val ctr = compiledProofs.size + 1
            val tparamNames = tparams.map(getName(_, proofStat))
            val dummyClassName = Type.Name(s"__VFx_Proof${ctr}__")

            /*
             * If the proof body starts with a universal quantifier
             * then eliminate the quantifier as follows:
             * proof test {
             *   forall(v1: V1, ..., vn: Vn) {
             *     p
             *   }
             * }
             *
             * Becomes:
             * def test(v1: V1, ..., vn: Vn) {
             *   p
             * }
             *
             * This is correct because proofs are checked by negating them:
             *   (assert (not test))
             * which is equivalent to:
             *   (declare-const v1 V1)
             *   ...
             *   (declare-const vn Vn)
             *   (assert (not (test v1, ..., vn)))
             *
             * The reason they are equivalent is because `(not test)`
             * checks `(not (forall ((v1 V1) ... (vn Vn)) p))` which is equivalent to
             * `(exists ((v1 V1) ... (vn Vn)) p)` which in turn is equivalent to:
             *
             *   (declare-const v1 V1)
             *   ...
             *   (declare-const vn Vn)
             *   p
             *
             * We eliminate the top level negated forall because sometimes
             * Z3 aborts the proof whereas if we eliminate the negated forall
             * Z3 is able to prove it.
             */
            val regularProofMethod = q"def $proofTermName() = $proofBody"
            val proofMethods = proofBody match {
              case Term.Block(List(Term.Apply(Term.Apply(Term.Name("forall"), vars), List(body @ Term.Block(_))))) => {
                vars.foreach {
                  case term => if (!term.isInstanceOf[Term.Ascribe]) throw ParseError(s"Invalid variable $term in forall quantifier", term.pos)
                }

                val params = vars.asInstanceOf[List[Term.Ascribe]].map(CompilerUtil.ascribeToParam)
                // Z3 is non-deterministic and sometimes it can only prove a certain goal with the top-level negated forall
                // and sometimes it can only prove it without the top-level negated forall
                // So we make 2 proof methods, one with the forall and one without the forall
                val proofNameWithForallEliminated = Term.Name(s"${proofTermName}_eliminated")
                List(
                  regularProofMethod,
                  q"def $proofNameWithForallEliminated(..$params) = $body")
              }
              case _ => List(regularProofMethod)
            }

            val methodsAndProof = methods ++ proofMethods

            /*
             * Corresponds to:
             *   q"""class $dummyClassName[..$tparamNames] extends $superTrait {
             *         ..$methodsAndProof
             *       }"""
             * but we don't have to handle all corner cases where type parameters are empty or there is no super trait.
             */

            val dummyClassDef =
              Defn.Class(
                List(), dummyClassName, tparamNames,
                Ctor.Primary(List(), Name.Anonymous(), List()),
                Template(List(), objInits, Self(Name.Anonymous(), None), methodsAndProof))

            val dummyClazz = ClassCompiler(classTable).transformer(dummyClassDef)
            val classTableWithDummy = classTable.copy(classTable = classTable.classTable + (dummyClazz.name.value -> dummyClazz))

            val irProof = dummyClazz.compiled

            // Check that the goal (i.e. body of the proof) is of type boolean
            // The dummy class needs to be known for this because the proof body may use the `this` pointer
            val dummyClassType = if (tparamNames.isEmpty) dummyClassName else Type.Apply(dummyClassName, tparams)
            Types.inferType(proofBody, Map("this" -> dummyClassType))(classTableWithDummy) match {
              case Type.Name("Boolean") => ()
              case proofBodyType => throw TypeError(s"The goal of a proof must be of type boolean but the goal of proof $proofName has type $proofBodyType", proofDef.pos)
            }

            (classTableWithDummy, compiledProofs + Proof(proofName, objName, irProof, dummyClassName.value))
          }
          case _ => throw ParseError(s"Unexpected statement in ProofCompiler", proofStat.pos)
        }
      }
    }
  }
}

object ClassCompiler {
  def isClass(node: Tree) = node.isInstanceOf[Defn.Class]

  /**
   * Returns a directed acyclic graph of dependencies between the provided methods.
   */
  def computeMethodDependencies(methods: List[Stat], methodPredicates: Map[String, List[Stat]]): DirectedAcyclicGraph[String, DefaultEdge] = {
    val dependencyGraph: DirectedAcyclicGraph[String, DefaultEdge] = new DirectedAcyclicGraph(classOf[DefaultEdge])

    methods.foreach {
      case method => {
        val methodName = method.methodName()
        dependencyGraph.addVertex(methodName)

        val deps = MethodCompiler.computeDependencies(method) // dependencies of the method
        val preds = methodPredicates.getOrElse(methodName, List())
        val predDeps =
          preds
            .map(MethodPredicate.computePredDependencies) // dependencies of the predicates
            .foldLeft(Set.empty[String])(_ ++ _) // merge all dependencies in 1 set

        val allDeps = deps ++ predDeps

        allDeps.foreach {
          case dep => {
            // Method depends on `dep`
            // so add an edge from `dep` to `methodName` because the dependencies must occur before the dependent methods
            dependencyGraph.addVertex(dep)
            try {
              // The edge may cause a cycle
              dependencyGraph.addEdge(dep, methodName)
            } catch {
              case _: IllegalArgumentException => throw ParseError(s"Could not compile method $methodName due to a circular dependency with method $dep", method.pos)
            }
          }
        }
      }
    }

    dependencyGraph
  }

  /**
   * Computes an ordering of the methods based on their dependencies.
   * Also takes into account the dependencies of the method's predicates (i.e. its context, preconditions, and invariants).
   */
  def computeMethodOrder(methods: List[Stat], methodPredicates: Map[String, List[Stat]]): List[String] = {
    import scala.jdk.CollectionConverters._
    val dependencyGraph = computeMethodDependencies(methods, methodPredicates)
    // Topologically sort the dependency graph
    dependencyGraph.iterator().asScala.toList
  }

  // Fetches the trait in the class table and concretizes it
  def getSuperTrait(inits: List[Init], classOrTraitOrObjectName: Type.Name)(implicit classTable: Classes) = {
    if (inits.length > 1) throw ParseError(s"$classOrTraitOrObjectName inherits from $inits but multiple inheritance is not supported.", classOrTraitOrObjectName.pos)
    inits.headOption.map {
      case Init(tpe, Name.Anonymous(), Nil) => classTable.getConcrete(tpe) match {
        case None => throw ClassNotFound(tpe.toString(), tpe.pos)
        case Some(classOrTrait) => classOrTrait match {
          case t: Trait => t
          case _: Clazz => throw TypeError(s"$classOrTraitOrObjectName extends class ${classOrTrait.name} but we may only extend traits.", classOrTraitOrObjectName.pos)
        }
      }
    }
  }
}

/**
 * Compiles classes and traits to IR.
 * @param classTable Maps class names to their internal class representation.
 */
case class ClassCompiler(classTable: Classes) {
  private def defToMethod(method: Defn.Def)(implicit thisClass: Type, classTable: Classes): Method = {
    val args = method.paramss.headOption.getOrElse(List.empty[Term.Param])
    val argTypes = CompilerUtil.termParamsToTypeMap(args)
    val argumentTypes = argTypes + ("this" -> thisClass) + ("old" -> thisClass)
    val tparams = (method.tparams ++ classTable.get(thisClass).get.tparams).distinctBy(_.name.value) // merge the method type parameters and the class type parameters
    CompilerUtil.checkArgumentListCorrectness(args, argumentTypes, tparams, method) // check that the argument list is well-formed

    val retTpe = Types.inferType(method.body, argumentTypes)

    method.decltpe match {
      case None => ()
      case Some(Type.Name(_)) | Some(Type.Apply(Type.Name(_), _)) => {
        if (!(retTpe <:< method.decltpe.get)) throw TypeError(s"Method ${method.name} has declared return type ${method.decltpe.get} but body has inferred type $retTpe", method.decltpe.get.pos)
      }
      case Some(t) => throw TypeError(s"Illegal return type $t for method $thisClass.${method.name}", t.pos)
    }

    Method(method.mods, method.name, method.tparams, args, argumentTypes, retTpe, method)
  }

  private def compileMethodOrPred(stat: Stat)(implicit thisClass: Type, classTable: Classes): (Term, Classes) = {
    if (MethodCompiler.isMethod(stat))
      compileMethod(stat.asInstanceOf[Defn.Def])
    else if (MethodPredicate.isMethodPred(stat))
      (compilePredicate(stat.asInstanceOf[Term]), classTable)
    else
      throw new InternalError("Expected a method or a predicate but got something else.")
  }

  private def compileMethod(m: Defn.Def)(implicit thisClass: Type, classTable: Classes): (Term, Classes) = {
    // Compile the method to IR
    val (methodIR, newClassTable) = MethodCompiler().transformer(m)
    val internalMethod = defToMethod(m)(thisClass, newClassTable)

    // Add the method to the method table of this class
    val clazz = newClassTable.get(thisClass).get
    val extendedClazz = clazz.cpy(methods = clazz.methods + (internalMethod.name.value -> internalMethod))
    val extendedClassTable = newClassTable + (Types.getClassName(thisClass) -> extendedClazz)

    (methodIR, extendedClassTable)
  }

  private def compilePredicate(pred: Term)(implicit thisClass: Type, classTable: Classes): Term = {
    MethodPredicate().transformer(pred)
  }

  private def compileStat(stat: Stat)(implicit thisClass: Type, classTable: Classes): (Term, Classes) = {
    if (MethodCompiler.isMethod(stat) || MethodPredicate.isMethodPred(stat))
      compileMethodOrPred(stat)
    else if (MethodCompiler.isValueDeclaration(stat))
      TraitCompiler().compileTraitFieldDecl(stat.asInstanceOf[Decl.Val])
    else if (MethodCompiler.isValue(stat))
      throw ParseError(s"Value definitions are not allowed in traits.", stat.pos) // because we can't handle those in Z3
      /* Value definitions are not allowed in traits because we can't handle those in Z3.
          e.g.
            trait Foo {
              val a = 1
              val b = this.a + 1
            }

          In Z3 those values will need to be fields of the `Foo` type.
          But, the `b` parameter of the type constructor cannot refer to the `a` parameter of the type constructor
          because `this.a` is translated to `(this a)` but the `this` object does not yet exist!
       */
    else if (MethodCompiler.isMethodDeclaration(stat))
      MethodCompiler().transformer(stat.asInstanceOf[Decl.Def])
    else
      throw new InternalError(s"Expected a value or method declaration/definition or a predicate but got something else: $stat")
  }

  /**
   * Orders methods before predicates.
   * @return True if `x` is a method and `y` is a predicate, false otherwise.
   */
  private def methodBeforePredicate(x: Stat, y: Stat): Boolean =
    (MethodCompiler.isMethod(x) || MethodCompiler.isMethodDeclaration(x)) && MethodPredicate.isMethodPred(y)

  def transformer(classOrTrait: Defn): ClassOrTrait = {
    classOrTrait match {
      case clazz: Defn.Class => transformClass(clazz)
      case t: Defn.Trait => transformTrait(t)
      case x => throw TypeError(s"ClassCompiler can't transform $x", x.pos)
    }
  }

  private def checkAndGetAnnotations(mods: List[Mod], allowSealed: Boolean = false)(implicit thisClass: Type): List[Term] = {
    mods.map {
      case Annot(init) => {
        val annot = Types.getClassName(init.tpe)
        q"Mod.Annot($annot)"
      }
      case Sealed() if allowSealed => q"Mod.Sealed"
      case mod if allowSealed => throw ParseError(s"Illegal modifier $mod. Only sealed modifier and annotations are allowed on traits", mod.pos)
      case mod => throw ParseError(s"Illegal modifier $mod. Only annotations are allowed on classes", mod.pos)
    }
  }

  /**
   * Ensures that only 1 trait is extended.
   * Looks up the trait that is extended.
   * @return The trait and its IR type
   */
  private def getFullSuperTrait(inits: List[Init], classOrTraitName: Type.Name)(implicit thisClass: Type, thisMethod: Option[String], classTable: Classes): (Option[Trait], List[Type.Param], List[Type], Term) = {
    val superTrait = ClassCompiler.getSuperTrait(inits, classOrTraitName)
    val superType = inits.headOption.map(_.tpe)
    val superTypeIR = superType.map(Types.transformer(_)) match {
      case None => q"None"
      case Some(supert) => q"Some($supert)"
    }

    val superTParams = superTrait.map(_.tparams).getOrElse(List())
    val superTypeArgs = inits.headOption.map(t => Types.getTypeArgs(t.tpe)).getOrElse(List())

    // Check that the type arguments are subtypes of the type parameters' upper type bounds
    superType foreach {
      case superTpe =>
        (superTypeArgs zip superTParams)
          .foldLeft(Map.empty[String, Type]) {
            case (inferredTypeParams, tup) => {
              typeArgMatchesTParam(tup._1, tup._2, superTpe, inferredTypeParams)
            }
          }
    }

    (superTrait, superTParams, superTypeArgs, superTypeIR)
  }

  // Checks that the type argument implement the type parameter's upper type bound (if it has one)
  private def typeArgMatchesTParam(typeArg: Type, typeParam: Type.Param, superTrait: Type, previouslyInferredTypeParams: Map[String, Type])(implicit thisClass: Type, thisMethod: Option[String], classTable: Classes): Map[String, Type] = {
    // Fetch the upper type bound of the type parameter (if it has one) and check that `typeArg` is a subtype of that upper type bound.
    // The upper type bound is not allowed to be another type parameter, e.g. trait Foo[A <: CRDT, B <: A],
    // because in the end, they both need to be filled in with concrete types (i.e. primitives or classes) and those cannot have subtypes.
    // So they would then need to be the same type, hence, there is no need to have 2 type params like this and hence it is not allowed.

    // lookup the type argument in the environment of the class/traits that extends the super trait
    Types.getClassM(typeArg, classTable, thisClass) match {
      case Some(typeArgClazz) => {

        // Check that the type argument matches the type parameter
        // and infer the type of each type parameter
        // if `typeParam` expects a type constructor, e.g. `trait Foo[A[B] <: CRDT[A[B]]]`
        // then type arg must be a type constructor, e.g. `ORSet`
        // So fetch the generic type of the type argument `Set` from its class definition, e.g. `ORSet[V]` from its class: `class ORSet[V] extends CRDT[ORSet[V]]`
        // and match that generic type `ORSet[V]` against the type parameter `A[B]` in order to infer the type of A and B, i.e. A -> ORSet and B -> V
        val paramNames = IRConversions.getAllTParamNames(typeParam)
        val typeArgType = if (Types.typeParamExpectsTypeCtor(typeParam)) typeArgClazz.getType() else typeArg
        val typeParamType = Types.typeParamToType(typeParam)
        val inferredTypeParams: Map[String, Type] = Types.inferTypeParam(typeParamType, typeArgType, paramNames, typeArg, previouslyInferredTypeParams)

        typeParam.tbounds.hi.foreach {
          case upperTypeBound => {
            // In the upper type bound of each type parameter: replace the type parameters by their inferred types.
            // Then check that the type argument is indeed a subtype of the filled upper type bound
            // This is needed because we may have a type parameter: T <: CRDT[T]
            // So in that case, when passing a type argument, e.g. GCounter, we need to check that GCounter <: CRDT[GCounter]
            // so it is important to replace the `T` in the upper type bound by the type argument `GCounter`
            // We could also have: T[A] <: CRDT[T[A]] , so we need to infer the type of both `T` and `A` and fill them in the upper type bound
            val filledUpperTypeBound = Types.fillTypeParameters(superTrait, inferredTypeParams.keys.toList.map(Type.Name(_)), inferredTypeParams.values.toList, upperTypeBound)

            // Get the class of this upper type bound
            Types.getClassM(filledUpperTypeBound, classTable, superTrait).foreach { // lookup the upper type bound in the environment of the super trait!
              case upperTypeBoundClazz => {
                val upperType = upperTypeBoundClazz.getType()
                if (!(typeArgClazz <:< upperType))
                  throw TypeError(s"Type argument $typeArg does not match type parameter $typeParam", typeArg.pos)
              }
            }
          }
        }

        inferredTypeParams
      }
      case None => {
        // No class found for `typeArg`
        // -> it could be a primitive type or a type parameter (without concrete upper type bound)
        if (typeParam.tbounds.hi.nonEmpty) {
          // The `typeArg` is not a subtype of the concrete upper type bound (because the `typeArg` is a primitive or a type parameter (without concrete upper type bound))
          throw TypeError(s"Type argument $typeArg does not match type parameter $typeParam", typeArg.pos)
        }

        previouslyInferredTypeParams + (typeParam.name.value -> typeArg)
      }
    }
  }

  def getPredicates(methodsAndPreds: List[Stat]) =
    methodsAndPreds
      .filter(MethodPredicate.isMethodPred) // keep only predicates
      .map(pred => (pred.methodName(), pred)) // make tuples of method name and the predicate

  def getPredicateMap(preds: List[(String, Stat)]) =
    preds
      .groupBy(_._1)
      .mapValues(_.map(_._2))
      .toMap // Map of method name to list of its predicates

  def transformTrait(traitt: Defn.Trait): Trait = traitt match {
    case Defn.Trait(mods, traitName, tparams, _, template) => {
      implicit val thisTrait = Types.classNameAndTParamsToType(traitName, tparams)

      // Check that all class modifiers are annotations or the sealed modifier
      // And transform those annotations to IR annotations
      val annotsIR = checkAndGetAnnotations(mods, true)

      // make sure that this sealed trait does not contain anything in its body
      // i.e. it must simply be a marker trait
      val isSealed = mods.find(CompilerUtil.isSealedModifier).nonEmpty
      if (isSealed && template.stats.nonEmpty) {
        throw ParseError(s"Sealed trait must have empty body.", traitt.pos)
      }

      // Already store the trait we are defining in the class table
      // and extend it later with the methods, proofs, etc.
      val trait1 = Trait(isSealed, traitName, tparams, Map(), Map(), List(), Map(), Map(), None, traitt, null)
      val classTable2 = classTable + (traitName.value -> trait1)

      // Check that there are no duplicate type parameters and allow upper type bounds
      CompilerUtil.checkTParams(tparams, true)(classTable2)

      // Extract the methods and fields from the `template`
      // There can be declarations and definitions
      template match {
        case Template(early, inits, Self(Name(""), None), statsWithProofs) => {
          if (early.nonEmpty) throw ParseError(s"Early initializers are not supported.", traitt.pos)

          // Get the super trait this trait extends and update this trait in the class table
          val superTrait = ClassCompiler.getSuperTrait(inits, traitName)(classTable2)
          val trait1WithSuper = trait1.copy(sup = superTrait)
          val intermediateClassTable = classTable2 + (traitName.value -> trait1WithSuper) // store the supertrait of this trait already because it is needed by `getFullSuperTrait`
          val (_, superTypeParams, superTypeArgs, superTypeIR) = getFullSuperTrait(inits, traitName)(thisTrait, None, intermediateClassTable)

          // Check that the super trait is not a sealed trait
          // because only classes may extend a sealed trait
          superTrait match {
            case Some(sup) if sup.isSealed => throw ParseError(s"Only classes may extend sealed traits.", traitt.pos)
            case _ => ()
          }

          // Check that the type args we pass to the super trait match its type parameters
          Types.matchTypeParamsForStructure(superTypeParams, superTypeArgs, traitt)(thisTrait, None, intermediateClassTable)

          // Also fetch all the proofs and store them in the trait
          val (proofs, statsWithoutProofs) = statsWithProofs.partition(ProofCompiler.isProof)

          val trait2 = trait1WithSuper.copy(proofs = proofs)
          val classTable3 = intermediateClassTable + (traitName.value -> trait2)

          // All statements in the trait's body must be values, methods, predicates, or proofs
          statsWithoutProofs.find(!CompilerUtil.isValueOrMethodOrPredicateOrProof(_)) match {
            case Some(stat) => throw ParseError(s"Illegal statement $stat in body of trait $traitName.", stat.pos)
            case None => {
              // Compile the values, methods, and predicates

              // Compute the order of the methods based on their dependencies
              val thisConcreteMethods = statsWithoutProofs.filter(stat => stat.isInstanceOf[Defn.Def] || stat.isInstanceOf[Decl.Def])
              // Fetch the value declarations
              val groupedStats = statsWithoutProofs.groupBy(stat => if (MethodCompiler.isValueDeclaration(stat)) "value" else "methodOrPred")
              val valueDecls = groupedStats.getOrElse("value", List())
              // Keep all stats except the value declarations, i.e. keep all method definitions and declarations
              val methodsAndPreds = groupedStats.getOrElse("methodOrPred", List())

              // Compute the order of methods based on their dependencies
              val allPreds = getPredicates(methodsAndPreds)
              val predMap = getPredicateMap(allPreds)
              val methodDefOrder = ClassCompiler.computeMethodOrder(thisConcreteMethods, predMap)
              val orderMap = methodDefOrder.zipWithIndex.toMap

              val orderedMethodsAndPreds = methodsAndPreds.sortBy(stat => orderMap.get(stat.methodName()))
              // Put the value declarations first and all the methods after that
              val valuesThenOrderedMethodsAndPreds = valueDecls ++ orderedMethodsAndPreds

              // IMPORTANT: First compile the methods and then compile the predicates!
              //            If we try to compile a predicate first it will fail because it won't find the method it is associated to
              val orderedStats = valuesThenOrderedMethodsAndPreds.sortWith(methodBeforePredicate) // sorting algorithm must be stable such that the order in which methods are defined is preserved

              // Compile the stats one-by-one and extend the trait in the class table each time
              val (classTable4, compiledStatsReversed) = orderedStats.foldLeft((classTable3, List.empty[Term])) {
                case ((classTable, compiledStats), stat) => {
                  val (compiledStat, newClassTable) = compileStat(stat)(thisTrait, classTable)
                  (newClassTable, compiledStat :: compiledStats)
                }
              }

              val irStats = compiledStatsReversed.reverse
              val irTypeParams = tparams.map(tparam => IRConversions.typeParam2IR(tparam)(thisTrait, None, classTable4))

              val traitIR = q"Defn.Trait(List(..$annotsIR), ${traitName.value}, List(..$irTypeParams), List(..$irStats), $superTypeIR)"

              val trait3 = classTable4.get(traitName.value).get.asInstanceOf[Trait]
              val internalTrait = trait3.copy(compiled = traitIR)

              // Check that the trait uses the `override` modifier where needed and only there
              internalTrait.checkFieldAndMethodTypes()(classTable4)
              internalTrait.checkOverrides()(classTable4)

              internalTrait
            }
          }
        }
      }
    }
  }

  def transformClass(clazz: Defn.Class): Clazz = clazz match {
    case Defn.Class(mods, className, tparams, ctor, template) => {
      implicit val thisClass = Types.classNameAndTParamsToType(className, tparams)

      // Check that there are no duplicate type parameters
      CompilerUtil.checkTParams(tparams)(classTable)
      // Check that all class modifiers are annotations
      // And transform those annotations to IR annotations
      val annotsIR = checkAndGetAnnotations(mods)

      // extract the fields from the `ctor` and the methods from the `template`
      ctor match {
        case Ctor.Primary(mods, _, paramss) => {
          if (mods.nonEmpty) throw ParseError(s"Modifiers not allowed on constructor, in:\n$ctor", ctor.pos)
          if (paramss.length > 1) throw ParseError(s"Multiple argument lists not supported, in:\n$ctor", ctor.pos)

          val args = paramss.headOption.getOrElse(List())

          val classTable2 = classTable + (className.value -> Clazz(className, tparams, List(), Map(), Map(), None, clazz, null))
          val fields = CompilerUtil.termParamToFieldMap(args)
          val fieldTypes = fields.mapValues(_.tpe).toMap
          args.foreach(arg => Types.isConcrete(arg.decltpe.get)(thisClass, None, classTable2))

          // Make sure that the arguments are well-formed (i.e. once a default value is provided, all subsequent arguments must have default values too)
          CompilerUtil.checkArgumentListCorrectness(args, fieldTypes, tparams, clazz)(thisClass, classTable2)

          template match {
            case Template(early, inits, Self(Name(""), None), stats) => {
              if (early.nonEmpty) throw ParseError(s"Early initializers are not supported.", template.pos)

              // Get the super trait this trait extends and update this class in the class table
              val superTrait = ClassCompiler.getSuperTrait(inits, className)(classTable2)
              val classTable3 = classTable2 + (className.value -> Clazz(className, tparams, args, fields, Map(), superTrait, clazz, null))
              val (_, superTypeParams, superTypeArgs, superTypeIR) = getFullSuperTrait(inits, className)(thisClass, None, classTable3)
              //val classTable3 = classTable2WithSuperTrait + (className.value -> Clazz(className, tparams, args, fields, Map(), superTrait, clazz, null))

              // Check that the type args we pass to the super trait match its type parameters
              Types.matchTypeParamsForStructure(superTypeParams, superTypeArgs, clazz)(thisClass, None, classTable3)

              // Fetch all methods and predicates
              val allMethodsAndPreds = (stats.methodsAndPredsOnly() ++ superTrait.map(_.getMethodDefAndPredStats()).getOrElse(List.empty[Stat])).distinctMethodsAndPreds
              // Compute the order of the methods based on their dependencies
              val allPreds = getPredicates(allMethodsAndPreds)
              val predMap = getPredicateMap(allPreds) // Map of method name to list of its predicates
              // Pass the map of predicates also to computeMethodOrder such that it also takes into account the dependencies of the predicates
              val allMethods = allMethodsAndPreds.filter(_.isInstanceOf[Defn.Def])
              val methodDefOrder = ClassCompiler.computeMethodOrder(allMethods, predMap)
              // Order them according to `methodDefOrder`
              // We create a map of the indices such that we do not need to search `methodDefOrder` to compute the index of every element every time in `sortBy`
              val orderMap = methodDefOrder.zipWithIndex.toMap
              // Check that all predicates are associated to existing methods
              allPreds.find(tup => !orderMap.contains(tup._1)).foreach(tup => throw ParseError(s"Cannot associate predicate to non-existent method ${Types.getClassName(thisClass)}.${tup._1}", tup._2.pos))

              stats.find(stat => !MethodCompiler.isMethod(stat) && !MethodPredicate.isMethodPred(stat)) match {
                case Some(stat) => throw ParseError(s"Illegal statement $stat in body of class $className.", stat.pos)
                case None => {
                  def compileMethodsAndPreds(stats: List[Stat], classTable: Classes): (List[Term], Classes) = {
                    val sortedStats = stats.sortBy(stat => orderMap.get(stat.methodName).get) // sort the methods based on their dependencies
                    // IMPORTANT: First compile the methods and then compile the predicates!
                    //            If we try to compile a predicate first it will fail because it won't find the method it is associated to
                    val methodsFollowedByPredicates = sortedStats.sortWith(methodBeforePredicate) // sorting algorithm must be stable such that the order in which methods are defined is preserved
                    val (newClassTable, methodsAndPredsReversed) = methodsFollowedByPredicates.foldLeft((classTable, List.empty[Term])) {
                      case ((classTable, compiledStats), stat) => {
                        val (compiledStat, newClassTable) = compileMethodOrPred(stat)(thisClass, classTable)
                        (newClassTable, compiledStat :: compiledStats)
                      }
                    }
                    val methodsAndPreds = methodsAndPredsReversed.reverse
                    (methodsAndPreds, newClassTable)
                  }

                  // Compile the methods and predicates
                  val (methodsAndPreds, classTable4) = compileMethodsAndPreds(stats, classTable3)

                  // Compile all the methods and predicates (i.e. from this class and the entire chain of super traits) with the old class table!
                  val (inheritedMethodsAndPreds, _) = compileMethodsAndPreds(allMethodsAndPreds, classTable3)

                  val methodDefs = stats.filter(MethodCompiler.isMethod(_)).asInstanceOf[List[Defn.Def]]
                  val methods = methodDefs.map(defToMethod(_)(thisClass, classTable4)).map(m => (m.name.value, m)).toMap

                  val irTypeParams = tparams.map(tparam => IRConversions.typeParam2IR(tparam)(thisClass, None, classTable4))
                  val irFields = args.map(IRConversions.termParam2IRField(_)(thisClass, None, classTable4))

                  // Check that there is no private modifier on a field (because `termParam2IRField` allows private modifiers)
                  args foreach {
                    case param =>
                      param.mods.find(mod => isPrivateModifier(mod)) match {
                        case Some(mod) => throw ParseError(s"Illegal private modifier on $param in $className", mod.pos)
                        case None => ()
                      }
                  }

                  val compiledClazz = q"Defn.ClassDef(List(..$annotsIR), ${className.value}, List(..$irTypeParams), List(..$irFields), List(..$methodsAndPreds), List(..$inheritedMethodsAndPreds), $superTypeIR)".asInstanceOf[Term]
                  val internalClazz = Clazz(className, tparams, args, fields, methods, superTrait, clazz, compiledClazz)

                  // Check that fields and methods respect the types declared/defined in the super trait
                  internalClazz.checkFieldAndMethodTypes()(classTable4)
                  // Check that the class defines all abstract fields and methods declared by the `superTrait` it extends
                  internalClazz.implementsSuperCheck()(classTable4)
                  // Check that the class uses the `override` modifier where needed and only there
                  internalClazz.checkOverrides()(classTable4)

                  internalClazz
                }
              }
            }
            case x => throw ParseError(s"Class $className uses self-type which is not supported.", x.pos)
          }
        }
      }
    }
  }
}

/**
 * @param mods List of modifiers associated to this method
 * @param name Name of the method
 * @param tparams List of type parameters of this method
 * @param argList List of arguments in order
 * @param args Maps argument names to their Scala type
 * @param retTpe Scala return type
 */
final case class Method(mods: List[Mod], name: Term.Name, tparams: List[Type.Param], argList: List[Term.Param], args: Map[String, Type], retTpe: Type, exp: Stat) {
  val defaults: List[Option[Term]] = argList.map(_.default)

  /**
   * @return The type signature of this method
   */
  def tpe(): Type = Type.Function(argList.map(_.decltpe.get), retTpe)

  def isPrivate(): Boolean = mods.find(_.isInstanceOf[Mod.Private]).nonEmpty

  /**
   * @return A copy of this class in which the type parameters `tparams` are replaced by the provided types in `types`.
   */
  def fillTypeParameters(tpeCtorName: Tree, tparams: List[Type], types: List[Type]): Method = {
    val filledArgList = argList.map(arg => Types.fillTypeParameters(tpeCtorName, tparams, types, arg))
    val filledArgs = args.view.mapValues(argType => Types.fillTypeParameters(tpeCtorName, tparams, types, argType)).toMap
    val filledRetTpe = Types.fillTypeParameters(tpeCtorName, tparams, types, retTpe)
    copy(argList = filledArgList, args = filledArgs, retTpe = filledRetTpe)
  }

  /**
   * Checks whether this method implements `superMethod`,
   * i.e. the argument types must match exactly and the return type must be a subtype of the return type of `superMethod`.
   * @return `Some(error)` if this method does not implement `superMethod` and `None` if it implements the method.
   */
  def implements(superMethod: Method, className: Type.Name)(implicit classTable: Types.Classes, thisClass: Type): Option[TypeError] = {
    val expectedArgTypes = superMethod.argList.map(_.decltpe.get)
    val actualArgTypes = argList.map(_.decltpe.get)

    if (!Types.matchTypesExactly(expectedArgTypes, actualArgTypes))
      Some(TypeError(s"Incompatible argument types in overriding for method $className.$name\n${Types.makeExpectedVsActualTypeStr(expectedArgTypes, actualArgTypes)}", exp.pos))
    else {
      val expectedRetType = superMethod.retTpe
      if (!(retTpe <:< expectedRetType))
        Some(TypeError(s"Incompatible return type in overriding for method $className.$name\n${Types.makeExpectedVsActualTypeStr(List(expectedRetType), List(retTpe))}", exp.pos))
      else
        None
    }
  }

  override def toString: String = s"${mods.mkString(" ")} def $name(${argList.mkString(", ")}): $retTpe" // don't print `exp` because it does not contain filled type parameters
}

final case class Field(mods: List[Mod], name: String, tpe: Type, exp: Any) {
  /**
   * Checks whether this field implements `superField`,
   * i.e. the field type must be a subtype of the `superField` type.
   * Returns `Some(error)` if this method does not implement `superField` and `None` if it implements the field.
   */
  def implements(superField: Field, className: Type.Name)(implicit classTable: Types.Classes, thisClass: Type): Option[TypeError] = {
    // Check that the type corresponds to the type in the super class
    if (!(tpe <:< superField.tpe))
      Some(TypeError(s"Incompatible type in overriding for field $exp in $className\nfound: $tpe\nrequired: ${superField.tpe}", tpe.pos))
    else
      None
  }

  def fillTypeParameters(tpeCtorName: Tree, tparams: List[Type], newTypes: List[Type]) =
    copy(tpe = Types.fillTypeParameters(tpeCtorName, tparams, newTypes, tpe))

  override def toString: String = s"${mods.mkString(" ")} val $name: $tpe" // don't print `exp` because it does not contain filled type parameters
}

sealed trait ClassOrTraitOrEnum {
  val name: Type.Name
  val compiled: Term
}

case class Enum(name: Type.Name, compiled: Term) extends ClassOrTraitOrEnum

sealed trait ClassOrTrait extends ClassOrTraitOrEnum {
  val compiled: Term
  val isFullyDefined = compiled != null
  def fillTypeParameters(types: List[Type]): ClassOrTrait

  val name: Type.Name
  val tparams: List[Type.Param]
  val concreteTParams: List[Type]
  val fields: Map[String, Field]
  val methods: Map[String, Method]
  val sup: Option[Trait]

  def getType(): Type = Types.classNameAndTArgsToType(name, concreteTParams)
  implicit val thisClass = this.getType()

  /**
   * Checks if this class/trait is a subtype of the given type.
   * In VeriFx all classes are covariant, hence,
   * `Set[A]` is a subtype of `Set[B]` iff `A` is a subtype of `B`.
   *
   * Covariance is needed for enumerations.
   * For example:
   * {{{
   * enum Op {
   *   Enable() | Disable() | Clear()
   * }
   *
   * val ops: Set[TaggedOp[Op]] = new Set[Op]().add(new TaggedOp(new Enable(), t1))
   * }}}
   *
   * Without covariance we could not add the tagged operation `Enable` because
   * `add` expects an argument of type `TaggedOp[Op]` but got a `TaggedOp[Enable]`.
   */
  def <:<(tpe: Type)(implicit classTable: Types.Classes): Boolean = {
    // same type
    getType =:= tpe ||
    // or, our super type is a subtype of `tpe`
    (sup.nonEmpty && sup.get <:< tpe) ||
    // or, this type and `tpe` belong to the same enumeration
    Types.getEnumType(getType, List(tpe)).nonEmpty ||
    // or, covariance
    (this.name.value == Types.getClassName(tpe) && // don't use `name` because it's the name of the method
     {
       val supTypeArgs = Types.getTypeArgs(tpe) // same type name
       concreteTParams.size == supTypeArgs.size && // same amount of type arguments
         (concreteTParams zip supTypeArgs).forall { case (subParam, supParam) => subParam <:< supParam } // every type arg must also be a subtype
     })
  }

  /**
   * Checks that the types of all fields and methods match the types of those fields and methods in the super trait
   */
  def checkFieldAndMethodTypes()(implicit classTable: Types.Classes): Unit = checkFieldsAndMethodTypes(fields, methods)

  protected def checkFieldsAndMethodTypes(fields: Map[String, Field], methods: Map[String, Method])(implicit classTable: Types.Classes): Unit = {
    sup foreach {
      supert => {
        fields.values.foreach {
          case field: Field => {
            val concreteField = supert.getConcreteField(field.name)
            val abstractField = supert.getAbstractField(field.name)
            concreteField.foreach(field.implements(_, name).foreach(throw _))
            abstractField.foreach(field.implements(_, name).foreach(throw _))
          }
        }

        methods.values.foreach {
          case method: Method => {
            val concreteMethod = supert.getConcreteMethod(method.name.value)
            val abstractMethod = supert.getAbstractMethod(method.name.value)
            concreteMethod.foreach(method.implements(_, name).foreach(throw _))
            abstractMethod.foreach(method.implements(_, name).foreach(throw _))
          }
        }
      }
    }
  }

  /**
   * Checks 2 things:
   * 1. that all fields and methods of this class/trait that override a concrete field/method from a super class have the "override" modifier and the correct type.
   * 2. that all fields and methods of this class/trait that have the override modifier, actually override something
   */
  def checkOverrides()(implicit classTable: Types.Classes): Unit = {
    sup match {
      case None =>
      case Some(supert) => {
        // Check 1: every field/method that overrides another should have the "override" modifier and the types must match
        val concreteSuperFields = supert.getConcreteFieldNames()
        val concreteSuperMethods = supert.getConcreteMethodNames()

        val thisFields = fields.keySet
        val thisMethods = methods.keySet

        val overriddenFields = concreteSuperFields intersect thisFields
        val overriddenMethods = concreteSuperMethods intersect thisMethods

        overriddenFields foreach {
          case fieldName => {
            val thisField = fields.get(fieldName).get

            // Check override modifier
            if (!thisField.mods.exists(_.isEqual(Mod.Override())))
              throw ParseError(s"Missing override modifier for field $fieldName in $name", thisField.tpe.pos)

            // Check that `thisField` implements `superField`
            val superField = supert.getConcreteField(fieldName).get
            thisField.implements(superField, name) map {
              case error => throw error
            }
          }
        }

        overriddenMethods foreach {
          case methodName => {
            val thisMethod = methods.get(methodName).get

            // Check override modifier
            if (!thisMethod.mods.exists(_.isEqual(Mod.Override())))
              throw ParseError(s"Missing override modifier for method $methodName in $name", thisMethod.exp.pos)

            // Check that this method implements the super method
            val superMethod = supert.getConcreteMethod(methodName).get
            thisMethod.implements(superMethod, name) map {
              case error => throw error
            }
          }
        }

        // Check 2: all fields and methods of this class/trait that have the override modifier must actually override something
        val fieldsWithOverride = fields.values.filter(_.mods.exists(_.isEqual(Mod.Override())))
        val methodsWithOverride = methods.values.filter(_.mods.exists(_.isEqual(Mod.Override())))

        fieldsWithOverride foreach {
          case field => {
            val fieldName = field.name
            if (supert.getConcreteField(fieldName).isEmpty)
              throw ParseError(s"Field $fieldName in $name overrides nothing.", field.tpe.pos)
          }
        }

        methodsWithOverride foreach {
          case method => {
            val methodName = method.name.value
            if (supert.getConcreteMethod(methodName).isEmpty) {
              throw ParseError(s"Method $methodName in $name overrides nothing.", method.exp.pos)
            }
          }
        }
      }
    }
  }

  /**
   * @return A set containing the names of all concrete fields of this class
   */
  // we only return the names of the fields/methods because if we put `Type`s in the set then we get into trouble because Type.Name("a") != Type.Name("a")
  protected def getConcreteFieldNames(): Set[String] = fields.keySet ++ sup.map(_.getConcreteFieldNames).getOrElse(Set())

  /**
   * Returns the names of all the concrete methods.
   * Note that VeriFx does not support method overloading.
   * @return A set containing the names of all concrete methods of this class.
   */
  protected def getConcreteMethodNames(): Set[String] = methods.keySet ++ sup.map(_.getConcreteMethodNames).getOrElse(Set())

  /*
  /**
   * @return A map of all the concrete fields of this class, the keys are the fields names
   */
  def getConcreteFields(): Map[String, Field] =
    getConcreteFieldNames().map(fieldName => (fieldName, getField(fieldName).get)).toMap
   */

  /**
   * @return A map of all the concrete methods of this class, they keys are the method names
   */
  def getConcreteMethods(): Map[String, Method] =
    getConcreteMethodNames().map(methodName => (methodName, getMethod(methodName).get)).toMap

  /**
   * Field lookup.
   * Searches for the field in this class and goes to the super class if not found here.
   * @param name Name of the field we are looking for.
   */
  def getConcreteField(name: String): Option[Field] = {
    fields.get(name) match {
      case f @ Some(_) => f
      case None => sup.flatMap(_.getConcreteField(name))
    }
  }

  /**
   * Method lookup.
   * Searches for the method in this class and goes to the super class if not found here.
   * VeriFx does not support method overloading!
   * @param name Name of the method we are looking for.
   */
  def getConcreteMethod(name: String): Option[Method] = {
    methods.get(name) match {
      case m @ Some(_) => m
      case None => sup.flatMap(_.getConcreteMethod(name))
    }
  }

  /**
   * Field lookup.
   * Searches for the field in this class and goes to the super class if not found here.
   * @param name Name of the field we are looking for.
   */
  def getField(name: String): Option[Field] = {
    fields.get(name) match {
      case f @ Some(_) => f
      case None => sup.flatMap(_.getField(name))
    }
  }

  /**
   * Method lookup.
   * Searches for the method in this class and goes to the super class if not found here.
   * VeriFx does not support method overloading!
   * @param name Name of the method we are looking for.
   */
  def getMethod(name: String): Option[Method] = {
    methods.get(name) match {
      case m @ Some(_) => m
      case None => sup.flatMap(_.getMethod(name))
    }
  }

  /*
  implicit class MapWithDiff[K, V](m: Map[K, V]) {
    def \[K, V](m1: Map[K, V], m2: Map[K, V]): Map[K, V] =
      (m1.toSet -- m2.toSet).toMap
  }
  */

  def cpy(name: Type.Name = name, tparams: List[Type.Param] = tparams, fields: Map[String, Field] = fields, methods: Map[String, Method] = methods, compiled: Term = compiled): ClassOrTrait = this match {
    case clazz: Clazz => clazz.copy(name = name, tparams = tparams, fields = fields, methods = methods, compiled = compiled)
    case t: Trait => t.copy(name = name, tparams = tparams, fields = fields, methods = methods, compiled = compiled)
  }
}

object Clazz {
  def apply(name: Type.Name, tparams: List[Type.Param], args: List[Term.Param], fields: Map[String, Field], methods: Map[String, Method], sup: Option[Trait], classDef: Defn.Class, compiled: Term) =
    new Clazz(name, tparams, Types.tparamsToTypes(tparams), args, fields, methods, sup, classDef, compiled)
}

/**
 * @param name Name of the class
 * @param tparams Abstract type parameters
 * @param args Arguments taken by the class constructor
 * @param fields Scala type of the fields of this class
 * @param methods Methods defined by this class
 */
final case class Clazz(name: Type.Name, tparams: List[Type.Param], concreteTParams: List[Type], args: List[Term.Param], fields: Map[String, Field], methods: Map[String, Method], sup: Option[Trait], classDef: Defn.Class, compiled: Term) extends ClassOrTrait {
  val fieldNames: List[String] = args.map(_.name.value)
  val defaults: List[Option[Term]] = args.map(_.default)

  /**
   * Returns a copy of this class in which the type parameters are replaced by the provided `types`.
   */
  def fillTypeParameters(types: List[Type]): Clazz = {
    val filledTParams = concreteTParams.map(tparam => Types.fillTypeParameters(name, concreteTParams, types, tparam))
    val filledArgs = args.map(arg => Types.fillTypeParameters(name, concreteTParams, types, arg))
    val filledFields = fields.view.mapValues(_.fillTypeParameters(name, concreteTParams, types)).toMap
    val filledMethods = methods.view.mapValues(method => method.fillTypeParameters(name, concreteTParams, types)).toMap

    // Don't fill the `tparams` of `classDef` because we can't replace type parameter names by complex types,
    // e.g. in `class Foo[A]` we can't replace `A` by `Set[Int]` because `Set[Int]` is a `Type.Apply` but the type parameter's name field must be a `Type.Name`
    val filledClassDef = classDef.copy(
      ctor = Types.fillTypeParameters(name, concreteTParams, types, classDef.ctor),
      templ = Types.fillTypeParameters(name, concreteTParams, types, classDef.templ))

    val filledSuper = sup.map(s => {
      // Replace the `tparams` by `types` in the `concreteTParams` of the super
      // and then fill the super with his modified tparams
      val newSuperTParams = s.concreteTParams.map(superTParam => Types.substituteTypes(concreteTParams, types, superTParam))
      s.fillTypeParameters(newSuperTParams)
    })
    copy(concreteTParams = filledTParams, args = filledArgs, fields = filledFields, methods = filledMethods, sup = filledSuper, classDef = filledClassDef)
  }

  /**
   * Checks that this class implements all abstract fields and methods from it's super type (i.e. the trait it extends).
   */
  def implementsSuperCheck()(implicit classTable: Types.Classes): Unit = {
    sup match {
      case None =>
      case Some(supert) => {
        // Check that the class implements the necessary fields and methods
        val fieldNamesToImplement = supert.getAbstractFieldNames
        val methodNamesToImplement = supert.getAbstractMethodNames

        val unimplementedFields = fieldNamesToImplement -- getConcreteFieldNames
        val unimplementedMethods = methodNamesToImplement -- getConcreteMethodNames

        if (unimplementedFields.nonEmpty)
          throw TypeError(s"Class $name misses implementation for field(s) ${unimplementedFields.map(supert.getAbstractField(_).get).mkString(", ")} inherited from trait ${supert.getType}", name.pos)
        if (unimplementedMethods.nonEmpty)
          throw TypeError(s"Class $name misses implementation for method(s) ${unimplementedMethods.map(supert.getAbstractMethod(_).get).mkString(", ")} inherited from trait ${supert.getType}", name.pos)

        // Check that the field types correspond to the types declared in the super trait
        fieldNamesToImplement foreach {
          case fieldName => {
            val thisField = getConcreteField(fieldName).get
            val abstractField = supert.getAbstractField(fieldName).get

            thisField.implements(abstractField, name) map {
              case error => throw error
            }
          }
        }

        methodNamesToImplement foreach {
          case methodName => {
            val thisMethod = getConcreteMethod(methodName).get
            val abstractMethod = supert.getAbstractMethod(methodName).get

            thisMethod.implements(abstractMethod, name) map {
              case error => throw error
            }
          }
        }
      }
    }
  }
}

object Trait {
  def apply(isSealed: Boolean, name: Type.Name, tparams: List[Type.Param], fields: Map[String, Field], methods: Map[String, Method], proofs: List[Stat],
            abstractFields: Map[String, Field], abstractMethods: Map[String, Method], sup: Option[Trait], traitDef: Defn.Trait, compiled: Term) =
    new Trait(isSealed, name, tparams, Types.tparamsToTypes(tparams), fields, methods, proofs, abstractFields, abstractMethods, sup, traitDef, compiled)
}

/**
 * @param isSealed Indicates if this is a sealed trait or not.
 * @param name Name of the trait
 * @param tparams Abstract type parameters
 * @param fields Fields defined by the trait
 * @param methods Methods defined by the trait
 * @param abstractFields Abstract fields declared by the trait
 * @param abstractMethods Abstract methods declared by the trait
 * @param traitDef Scala meta trait definition
 * @param compiled Compiled trait (IR representation)
 */
final case class Trait(isSealed: Boolean, name: Type.Name, tparams: List[Type.Param], concreteTParams: List[Type], fields: Map[String, Field], methods: Map[String, Method], proofs: List[Stat], abstractFields: Map[String, Field], abstractMethods: Map[String, Method], sup: Option[Trait], traitDef: Defn.Trait, compiled: Term) extends ClassOrTrait {
  def getAllProofs(): List[Stat] = this.sup.map(_.getAllProofs()).getOrElse(List()) ++ this.proofs

  /**
   * Returns a copy of this trait in which the type parameters are replaced by the provided `types`.
   */
  def fillTypeParameters(types: List[Type]): Trait = {
    val filledTParams = concreteTParams.map(tparam => Types.fillTypeParameters(name, concreteTParams, types, tparam))
    val filledFields = fields.view.mapValues(_.fillTypeParameters(name, concreteTParams, types)).toMap
    val filledMethods = methods.view.mapValues(method => method.fillTypeParameters(name, concreteTParams, types)).toMap
    val filledProofs = proofs.map(proof => Types.fillTypeParameters(name, concreteTParams, types, proof))
    val filledAbstractFields = abstractFields.view.mapValues(_.fillTypeParameters(name, concreteTParams, types)).toMap
    val filledAbstractMethods = abstractMethods.view.mapValues(method => method.fillTypeParameters(name, concreteTParams, types)).toMap

    // Don't fill the `tparams` of `traitDef` because we can't replace type parameter names by complex types,
    // e.g. in `trait Foo[A]` we can't replace `A` by `Set[Int]` because `Set[Int]` is a `Type.Apply` but the type parameter's name field must be a `Type.Name`
    val filledTraitDef = traitDef.copy(
      ctor = Types.fillTypeParameters(name, concreteTParams, types, traitDef.ctor),
      templ = Types.fillTypeParameters(name, concreteTParams, types, traitDef.templ))

    val filledSuper = sup.map(s => {
      // Replace the `tparams` by `types` in the `concreteTParams` of the super
      // and then fill the super with his modified tparams
      val newSuperTParams = s.concreteTParams.map(superTParam => Types.substituteTypes(concreteTParams, types, superTParam))
      s.fillTypeParameters(newSuperTParams)
    })
    copy(concreteTParams = filledTParams, fields = filledFields, methods = filledMethods, proofs = filledProofs,
         abstractFields = filledAbstractFields, abstractMethods = filledAbstractMethods, sup = filledSuper, traitDef = filledTraitDef)
  }

  def getAbstractField(field: String): Option[Field] = abstractFields.get(field) match {
    case abstractField @ Some(_) => abstractField
    case None => sup.flatMap(_.getAbstractField(field))
  }

  def getAbstractMethod(method: String): Option[Method] = abstractMethods.get(method) match {
    case abstractMethod @ Some(_) => abstractMethod
    case None => sup.flatMap(_.getAbstractMethod(method))
  }

  /*
  def getAbstractFields(): Map[String, Field] = getAbstractFieldNames.map(field => (field, getField(field).get)).toMap
  def getAbstractMethods(): Map[String, Method] = getAbstractMethodNames.map(method => (method, getMethod(method).get)).toMap
  */

  def getAbstractFieldNames(): Set[String] = abstractFields.keySet ++ sup.map(_.getAbstractFieldNames()).getOrElse(Set())
  def getAbstractMethodNames(): Set[String] = abstractMethods.keySet ++ sup.map(_.getAbstractMethodNames()).getOrElse(Set())

  /**
   * Compute the order in which methods are declared/defined through the chain of super traits.
   * Methods in the super trait must be declared/defined before methods from subtraits.
   * @return A list containing the method names in their order of declaration/definition through the chain of super traits.
   */
  def getMethodOrder(): List[String] = {
    val superMethods = sup.map(_.getMethodOrder()).getOrElse(List.empty[String])
    val thisMethods = traitDef.templ.stats.filter(stat => MethodCompiler.isMethod(stat) || MethodCompiler.isMethodDeclaration(stat)).map {
      case m: Defn.Def => m.name.value
      case m: Decl.Def => m.name.value
    }

    (superMethods ++ thisMethods).distinct
  }

  /**
   * Fetches all method definitions and predicates in the chain of super traits.
   * Replaces the type of the trait by the type of the extending trait/class in every method and predicate.
   */
  def getMethodDefAndPredStats(): List[Stat] = {
    val thisMethodDefsAndPreds = traitDef.templ.stats.methodsAndPredsOnly()
    val superMethodDefsAndPreds = sup.map(s => s.getMethodDefAndPredStats()).getOrElse(List.empty[Stat])
    (thisMethodDefsAndPreds ++ superMethodDefsAndPreds).distinctMethodsAndPreds
  }

  /**
   * Field lookup.
   * Searches for the field in this class' concrete and abstract fields and goes to the super class if not found here.
   * @param name Name of the field we are looking for.
   */
  override def getField(name: String): Option[Field] = getConcreteField(name) match {
    case f @ Some(_) => f
    case None => {
      getAbstractField(name) match {
        case f @ Some(_) => f
        case None => super.getField(name)
      }
    }
  }

  /**
   * Method lookup.
   * Searches for the method in this class' concrete and abstract methods and goes to the super class if not found here.
   * VeriFx does not support method overloading!
   * @param name Name of the method we are looking for.
   */
  override def getMethod(name: String): Option[Method] = getConcreteMethod(name) match {
    case m @ Some(_) => m
    case None => {
      getAbstractMethod(name) match {
        case m @ Some(_) => m
        case None => super.getMethod(name)
      }
    }
  }

  override def checkFieldAndMethodTypes()(implicit classTable: Types.Classes): Unit = {
    checkFieldsAndMethodTypes(abstractFields, abstractMethods)
    super.checkFieldAndMethodTypes()
  }
}
