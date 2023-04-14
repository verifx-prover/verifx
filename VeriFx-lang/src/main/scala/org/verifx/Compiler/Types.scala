package org.verifx.Compiler

import scala.meta.Term.Eta
import scala.meta._
import scala.meta.contrib._
import cats.implicits._
import org.verifx.{ClassNotFound, Compiler, FieldDoesNotExist, MatchError, MethodNotFound, ParseError, TypeError, UnknownIdentifierError}

import java.lang.Enum

/**
 * Implements all functionality related to types.
 * Also implements a type inferencer which is the method `inferType`.
 */
object Types {
  type ClassTable = Map[String, ClassOrTrait]
  final case class Classes(classTable: ClassTable = Map(),
                           classDefinitionOrder: List[ClassOrTraitOrEnum] = List(),
                           fullyDefinedClasses: Set[String] = Set(),
                           enumClasses: Set[String] = Set(),
                           enums: Map[String, Set[String]] = Map()) {
    def +(clazzOrTrait: (String, ClassOrTrait)) = {
      if (fullyDefinedClasses.contains(clazzOrTrait._1) || !clazzOrTrait._2.isFullyDefined || enumClasses.contains(clazzOrTrait._1)) {
        copy(classTable + clazzOrTrait, classDefinitionOrder, fullyDefinedClasses)
      } else {
        // Only add a class definition to the `classDefinitionOrder` when the class is fully defined
        copy(classTable + clazzOrTrait, classDefinitionOrder.appended(clazzOrTrait._2), fullyDefinedClasses + clazzOrTrait._1)
      }
    }

    /**
     * Remembers all classes and traits that are part of an enumeration.
     * These classes and traits won't be added to the `classDefinitionOrder`.
     * @param enumName Type of the enumeration
     * @param classes All data constructors for this enumeration
     */
    def announceEnum(enumName: String, classes: Set[String]): Classes =
      this.copy(enumClasses = enumClasses + enumName ++ classes, enums = enums + (enumName -> classes))

    /**
     * Adds a compiled enumeration to the `classDefinitionOrder`.
     */
    def +(enumIR: Compiler.Enum): Classes = this.copy(classDefinitionOrder = classDefinitionOrder.appended(enumIR))

    def contains(clazz: String): Boolean = classTable.contains(clazz)
    def contains(classType: Type): Boolean = contains(getClassName(classType))

    def get(clazz: String): Option[ClassOrTrait] = classTable.get(clazz)
    def get(classType: Type): Option[ClassOrTrait] = get(getClassName(classType))

    /**
     * Looks up the class/trait and replaces the type parameters by the concrete type parameters in `classType`
     */
    def getConcrete(classType: Type): Option[ClassOrTrait] = {
      get(classType) map {
        clazz => {
          classType match {
            case Type.Name(_) => clazz
            case Type.Apply(_, tparams) => clazz.fillTypeParameters(tparams)
          }
        }
      }
    }

    def isClass(tpe: Type): Boolean = get(tpe).map(_.isInstanceOf[Clazz]).getOrElse(false)

    def isTrait(tpe: Type, tparams: List[Type.Param]): Boolean = {
      def isTParam() = {
        val tpeName = getClassName(tpe)
        tparams.find(_.name.value == tpeName).nonEmpty
      }

      if (isFunctionType(tpe) || isTParam() || isPrimitiveType(tpe))
        false
      else {
        val name = tpe match {
          case Type.Name(name) => name
          case Type.Apply(Type.Name(name), _) => name
        }

        classTable.get(name) match {
          case None => throw ClassNotFound(name, tpe.pos)
          case Some(classOrTrait) =>
            classOrTrait.isInstanceOf[Trait]
        }
      }
    }

    def isEnum(tpe: Type): Boolean = enumClasses.contains(Types.getClassName(tpe)) // normally we should check that `tpe` is not a method or class type param

    def isSealedTrait(tpe: Type, tparams: List[Type.Param]): Boolean = {
      val name = tpe match {
        case Type.Name(name) => name
        case Type.Apply(Type.Name(name), _) => name
      }

      isTrait(tpe, tparams) && classTable.get(name).get.asInstanceOf[Trait].isSealed
    }
  }

  /**
   * Represents a proof
   * @param name Name of the proof
   * @param containerObject Name of the object in which the proof is defined
   * @param compiled IR representation of the proof
   * @param dummyClassName Name of the dummy class in which the proof is defined
   */
  final case class Proof(name: String, containerObject: String, compiled: Term, dummyClassName: String)

  def makeExpectedVsActualTypeStr(expected: List[Any], actual: List[Any]) = {
    s"""Expected: ${expected.mkString(", ")}
       |Actual: ${actual.mkString(", ")}""".stripMargin
  }

  def isPrimitiveType(tpe: Type): Boolean = tpe match {
    case Type.Name("Unit") | Type.Name("Boolean") | Type.Name("Int") | Type.Name("String") => true
    case _ => false
  }

  def isFunctionType(tpe: Type): Boolean = {
    tpe match {
      case Type.Function(_, _) => true
      case _ => false
    }
  }

  def isSetOrMap(tpe: Type): Boolean = {
    tpe match {
      case Type.Apply(Type.Name("Set"), _) => true
      case Type.Apply(Type.Name("Map"), _) => true
      case _ => false
    }
  }

  def getTypeArgs(tpe: Type): List[Type] = tpe match {
    case Type.Name(_) => Nil
    case Type.Apply(_, args) => args
    case _ => throw new InternalError(s"Unexpected type $tpe")
  }

  /**
   * Replaces the occurences of the given type parameters by concrete types.
   * @param name Name of the class or method we are filling.
   * @param tparams The type parameters
   * @param newTypes The types that must replace the type parameters
   * @param node The AST node in which to replace the type parameters
   * @return The modified AST node
   */
  def fillTypeParameters[A <: Tree](name: Tree, tparams: List[Type], newTypes: List[Type], node: A): A = {
    if (newTypes.length != tparams.length)
      throw TypeError(s"$name expects ${tparams.length} type parameters but got ${newTypes.length}.\n${makeExpectedVsActualTypeStr(tparams, newTypes)}", name.pos)
    substituteTypes(tparams, newTypes, node)
  }

  def substituteTypes[A <: Tree](types: List[Type], newTypes: List[Type], node: A): A = {
    val substitution = types.zip(newTypes)

    // Sometimes it may happen that we need to substitute `V` by `Foo[V]`
    // Hence, we cannot use `transform` on trees because it recursively visits the transformed nodes
    // e.g.: V -> Foo[V] -> Foo[Foo[V]] -> Foo[Foo[Foo[V]]] -> ... (infinite recursion)
    //       the above example occurs for instance in this code: `class Test[V](t: Set[Foo[V]] = new Set[Foo[V]]())`
    //       because when concretizing the class `Set[Foo[V]]` we try to replace the type parameter `V` of `Set` by `Foo[V]`
    // To solve this problem we implement a custom transformation that does not visit transformed trees
    val transformer = new Transformer {
      override def apply(tree: Tree): Tree = tree match {
        case x => {
          substitution.find(sub => x.isEqual(sub._1)) match {
            case Some((_, newTpe)) => newTpe.asInstanceOf[A] // avoid recursive call to the transformed tree here
            case None => super.apply(x)
          }
        }
      }
    }

    transformer(node).asInstanceOf[A]
  }

  /**
   * Infers the Scala type of each argument.
   */
  def inferArgTypes(args: List[Term], env: Map[String, Type])(implicit classTable: Classes): List[Type] =
    args.map(Types.inferType(_, env))

  implicit class TypeWithSubtypeCheck(tpe: Type)(implicit classTable: Types.Classes, thisClass: Type) {
    /**
     * Checks if this type is a subtype of `sup`.
     */
    def <:<(sup: Type): Boolean = {
      (tpe, sup) match {
        case (Type.Function(params1, res1), Type.Function(params2, res2)) => {
          // a function type `tpe` matches a function type `sup` iff its parameters are supertypes and its return type is a subtype
          matchSubtypes(params1, params2) && res1 <:< res2
        }
        case (Type.Function(_, _), _) => false
        case (_, Type.Function(_, _)) => false
        case _ => {
          classTable.getConcrete(tpe) match {
            case None =>
              // Primitive types, built-in types, and type parameters are not in the class table
              val sameType = tpe =:= sup
              val upperBoundIsSubtype =
                getClassM(tpe, classTable, thisClass) // will look up the class of the upper type bound if `tpe` is a type parameter
                  .map(upperTypeBound => upperTypeBound <:< sup) // upper type bound must be a subtype of `sup`
                  .getOrElse(false) // if `tpe` is not a type parameter or it is but has no upper type bound then it will not find a class, so we just return false (because `tpe` must be a primitive or a built-in type).
              sameType || upperBoundIsSubtype
            case Some(sub) =>
              sub <:< sup
          }
        }
      }
    }
  }

  implicit class TypeWithEqualsCheck(tpe: Type) {
    def =:=(other: Type): Boolean = tpe.isEqual(other)
  }

  /**
   * Takes a set of types and returns the least common supertype, if any.
   * With "least common" we mean the supertype that is closest to the types in the hierarchy.
   */
  def getLeastCommonType(types: List[Type])(implicit classTable: Types.Classes, thisClass: Type): Option[Type] = {
    def checkHierarchy(tpe: Type, sup: Option[ClassOrTrait], otherTypes: List[Type]): Option[Type] = {
      // Check that all other types are a subtype of this one
      if (otherTypes.forall(_ <:< tpe)) {
        // `tpe` is the least common supertype
        Some(tpe)
      }
      else {
        // Go up in the type hierarchy
        sup.flatMap(supClazzOrTrait => {
          checkHierarchy(supClazzOrTrait.getType(), supClazzOrTrait.sup, otherTypes)
        })
      }
    }

    types match {
      case Nil => None
      case tpe :: otherTypes => {
        // `tpe` may be a primitive type in which case it won't be in the class table (and also has no super type).
        checkHierarchy(tpe, classTable.getConcrete(tpe).flatMap(_.sup), otherTypes)
      }
    }
  }

  // If all the types are inhabitants of the same enumeration
  // it returns the type of the enumeration (or the common constructor type if they are all the same constructor),
  // returns None otherwise
  def getEnumType(tpe: Type, otherTypes: List[Type])(implicit classTable: Types.Classes): Option[Type] = {
    val allTypes = tpe :: otherTypes

    // Check that they are all enumerations
    if (allTypes.forall(classTable.isEnum)) {
      // Fetch the top level enumeration type (which is its super trait (or itself if it has no super))
      val allEnumTypes = allTypes.map(t => classTable.getConcrete(t).flatMap(_.sup).map(_.getType()).getOrElse(t))
      // Check that they are all the same enumeration type
      allEnumTypes match {
        case supTpe :: restSupers => {
          if (restSupers.forall(TypeWithEqualsCheck(supTpe) =:= _))
            Some(supTpe)
          else
            None
        }
      }
    }
    else
      None
  }

  /**
   * @return True if the actual argument types match the expected argument types
   */
  def matchTypesExactly(expected: List[Type], actual: List[Type]): Boolean = {
    (expected.length == actual.length) && (expected zip actual).forall {
      case (expectedType, actualType) => expectedType =:= actualType
    }
  }

  def matchSubtypes(expected: List[Type], actual: List[Type])(implicit classTable: Types.Classes, thisClass: Type): Boolean = {
    (expected.length == actual.length) && (expected zip actual).forall {
      case (expectedType, actualType) => actualType <:< expectedType
    }
  }

  def matchTypesForStructure(typeParam: Type.Param, concreteType: Type)(implicit thisClass: Type, thisMethod: Option[String], classTable: Classes): Boolean = {
    // To check if a concrete type matches a type param:
    //  - is type param a Type.Name?
    //      - then: check that concrete type is a concrete type (i.e. not a type constructor), so it must be a primitive type or a class or a concrete method or class type parameter
    //  - is type param a Type.Apply?
    //      - then: the concrete type must have the same structure.
    //              Check how many tparams it expects and check that concrete type is a kind (the type constructor)
    //              and that that kind expects as many type parameters
    //              check each of the type params in the concrete type and type param recursively (they should be equal if we remove the abstract type names)

    val expectedTypeArgs = typeParam.tparams

    // Check that `concreteType` is a valid type
    concreteType match {
      case tpe @ Type.Name(name) => {
        if (!Types.isPrimitiveType(tpe) && !classTable.contains(name) && !isMethodTypeParam(tpe) && !isClassTypeParam(tpe)) {
          throw TypeError(s"Unknown type $concreteType", concreteType.pos)
        }
      }
      case Type.Apply(tpe @ Type.Name(className), args) => {
        // Types cannot be partially applied so you cannot write `Map[Int]`
        // instead you need to fully provide the type

        // Fetch the type's type parameters, then compare them to `args`
        val tparams =
          if (isMethodTypeParam(tpe)) // is it a type param of this method?
            getMethodTypeParam(tpe).tparams
          else if (isClassTypeParam(tpe)) // is it a type param of this class?
            getClassTypeParam(tpe).tparams
          else if (classTable.contains(className)) // is it a class?
            classTable.get(className).get.tparams // the class' type parameters
          else
            throw ClassNotFound(className, concreteType.pos)

        if (tparams.length != args.length)
          throw TypeError(s"Class $tpe takes ${tparams.length} type parameters but got ${args.length}, in: $concreteType", concreteType.pos)
      }
      case _ => throw TypeError(s"Illegal type $concreteType", concreteType.pos)
    }

    // Match the concrete type against the structure of the type parameter
    if (expectedTypeArgs.isEmpty) {
      // Expects a proper type (e.g. Int, Set[String], ...)
      // i.e. a concrete type and not a type constructor

      // `concreteType` may be a type parameter of the method and is not a type constructor (i.e. does not take type parameters itself)
      // e.g. def foo[A, B[_]](...) --> A is fine, B not because B is a type constructor
      (concreteType.isInstanceOf[Type.Name] &&
        isMethodTypeParam(concreteType.asInstanceOf[Type.Name]) &&
        getMethodTypeParam(concreteType.asInstanceOf[Type.Name]).tparams.isEmpty) ||
      // same as above but `concreteType` is a type parameter of the form P[...]
      (concreteType.isInstanceOf[Type.Apply] &&
        isMethodTypeParam(concreteType.asInstanceOf[Type.Apply].tpe.asInstanceOf[Type.Name]) &&
        getMethodTypeParam(concreteType.asInstanceOf[Type.Apply].tpe.asInstanceOf[Type.Name]).tparams.length == concreteType.asInstanceOf[Type.Apply].args.length) ||
      // or, `concreteType` is a type parameter of the class and is not a type constructor (i.e. does not take type parameters itself)
      // e.g. class Foo[A, B[_]] --> A is fine, B not because B is a type constructor
      (concreteType.isInstanceOf[Type.Name] &&
        isClassTypeParam(concreteType.asInstanceOf[Type.Name]) &&
        getClassTypeParam(concreteType.asInstanceOf[Type.Name]).tparams.isEmpty) ||
      // same as above but `concreteType` is a type parameter of the form P[...]
      (concreteType.isInstanceOf[Type.Apply] &&
        isClassTypeParam(concreteType.asInstanceOf[Type.Apply].tpe.asInstanceOf[Type.Name]) &&
        getClassTypeParam(concreteType.asInstanceOf[Type.Apply].tpe.asInstanceOf[Type.Name]).tparams.length == concreteType.asInstanceOf[Type.Apply].args.length) ||
      // or `concreteType` is a primitive type or a function type
      Types.isPrimitiveType(concreteType) ||
      Types.isFunctionType(concreteType) ||
      // The concrete type may also be a class but then it needs to be a concrete class and not a type constructor
      // e.g. `Set[Int]` is fine but `Set` isn't because set is a type constructor * -> *
      // so we check that we know the class and that if it is a Type.Name it is not a type constructor
      // or if it is a Type.Apply then its type arguments must match the type parameters of the class
      (classTable.contains(concreteType) &&
        ((concreteType.isInstanceOf[Type.Name] && classTable.get(concreteType).get.tparams.isEmpty) ||
         (concreteType.isInstanceOf[Type.Apply] && concreteType.asInstanceOf[Type.Apply].args.length == classTable.get(concreteType).get.tparams.length)))
    }
    else {
      // Expects a type constructor
      concreteType match {
        case concreteType: Type.Name => {
          // Fetch the type parameters expected by this type constructor (which can be a class, method type param, or class type param)
          val concreteArgs =
            if (isMethodTypeParam(concreteType))
              getMethodTypeParam(concreteType).tparams
            else if (isClassTypeParam(concreteType))
              getClassTypeParam(concreteType).tparams
            else
              classTable.get(concreteType) match {
                case Some(kind) => kind.tparams
                case None => throw TypeError(s"Unknown type constructor $concreteType", concreteType.pos)
              }

          // Now check that this type constructor expects as many arguments as the type parameter
          (expectedTypeArgs.length == concreteArgs.length) && {
            // Both `expectedTypeArgs` and `concreteArgs` are lists of Type.Param
            // they thus only contain abstract type parameters
            // if we replace the names of these abstract type parameters, they should have the same structure
            def removeParamNames(param: Type.Param): Type.Param = param.transform {
              case Type.Name(_) => Name("")
            }.asInstanceOf[Type.Param]

            def matchTypeParams(param1: Type.Param, param2: Type.Param): Boolean = {
              val p1 = removeParamNames(param1)
              val p2 = removeParamNames(param2)
              p1 isEqual p2
            }

            (expectedTypeArgs zip concreteArgs).forall((matchTypeParams _).tupled)
          }
        }
        case _ => {
          // The type arg is not a type constructor because it is already applied to some types (if it is a Type.Apply, otherwise it is a function and is also not a valid type constructor)!
          false
        }
      }
    }
  }

  def matchTypeParamsForStructure(expectedTypeParams: List[Type.Param], concreteTypes: List[Type], exp: Any)(implicit thisClass: Type, thisMethod: Option[String], classTable: Classes): Unit = {
    (expectedTypeParams zip concreteTypes).find(tup => !matchTypesForStructure(tup._1, tup._2)) match {
      case Some((typeParam, concreteType)) => throw TypeError(s"$concreteType does not match type parameter $typeParam in $exp", concreteType.pos)
      case None => ()
    }
  }

  // Returns true if the type parameter represents a type constructor (i.e. takes one or more type parameters itself)
  def typeParamExpectsTypeCtor(tparam: Type.Param) = tparam.tparams.nonEmpty

  def inferTypeParam(declType: Type, properType: Type, paramNames: Set[String], exp: Tree, paramTypes: Map[String, Type] = Map()): Map[String, Type] = {
    declType match {
      case Type.Name(name) if paramNames.contains(name) => {
        // it is a type parameter
        paramTypes.get(name) match {
          case Some(tpe) if !tpe.isEqual(properType) =>
            throw TypeError(s"Inferred conflicting types for $declType ($tpe and $properType)", exp.pos)
          case _ => ()
        }
        paramTypes + (name -> properType)
      }
      case Type.Name(_) => {
        // it is a concrete type
        paramTypes
      }
      case Type.Apply(abstractName, params) => {
        // `declType` is a compound type, e.g. `Set[Int]`
        // and `properType` should also be a compound type
        // Then infer the type params on their first parts,
        // and then on their type parameters recursively.
        properType match {
          case Type.Apply(name, args) => {
            if (params.length != args.length) throw TypeError(s"Expected $declType but got $properType", exp.pos)
            val newParamTypes = inferTypeParam(abstractName, name, paramNames, exp, paramTypes)
            (params zip args).foldLeft(newParamTypes) {
              case (paramTypes, (declType, properType)) =>
                inferTypeParam(declType, properType, paramNames, exp, paramTypes)
            }
          }
          case _ => throw TypeError(s"Expected $declType but got $properType", exp.pos)
        }
      }
      case Type.Function(declParams, declRes) => {
        // `declType` is a function type, e.g. (A, B) => Boolean
        // check that `properType`is also a function type
        // Then infer the type params of the function type and then the return type
        properType match {
          case Type.Function(actualParams, actualRes) => {
            if (declParams.length != actualParams.length) throw TypeError(s"Expected $declType but got $properType", exp.pos)
            // Infer the type params based on the function's arguments types
            val newParamTypes = (declParams zip actualParams).foldLeft(paramTypes) {
              case (paramTypes, (declType, properType)) =>
                inferTypeParam(declType, properType, paramNames, exp, paramTypes)
            }
            // Infer the type params based on the function's return type
            inferTypeParam(declRes, actualRes, paramNames, exp, newParamTypes)
          }
          case _ => throw TypeError(s"Expected $declType but got $properType", exp.pos)
        }
      }
      case _ => throw TypeError(s"Illegal type $declType", declType.pos)
    }
  }

  /**
   * Infers the types of the class' type parameters based on the provided arguments.
   * @param clazz The class for which to infer the type of the type parameters
   * @param args Arguments passed to the type constructor
   * @param exp Overall AST expression (used for reporting useful errors)
   * @return A tuple containing the list of inferred types (in order of the type parameters) and the updated class table
   */
  def inferTypeParams(clazz: Clazz, args: List[Term], env: Map[String, Type], exp: Term)(implicit classTable: Classes): List[Type] = {
    if (clazz.classDef.ctor.paramss.size > 1)
      throw ParseError(s"Multiple argument lists are not supported", exp.pos)

    val typeParams = clazz.classDef.tparams
    val declTypes = clazz.classDef.ctor.paramss.headOption.map(_.map(_.decltpe.get)).getOrElse(List())

    inferTypeParams(typeParams, declTypes, args, env, exp)
  }

  /**
   * Infers the types of the method's type parameters based on the provided arguments.
   * @param method The method for which to infer the type of the type parameters
   * @param args Arguments passed to the method
   */
  def inferTypeParams(method: Method, args: List[Term], env: Map[String, Type], exp: Term)(implicit classTable: Classes): List[Type] = {
    val typeParams = method.tparams
    val declTypes = method.argList.map(_.decltpe.get)

    inferTypeParams(typeParams, declTypes, args, env, exp)
  }

  /**
   * Infer the type of `typeParams` based on the declared types `declTypes` and the provided arguments `args`.
   * The declared types `declTypes` are the declared types of the arguments `args`.
   * By inferring the actual type of `args` and matching it to `declTypes` we can infer the type of the type parameters `typeParams`.
   */
  def inferTypeParams(typeParams: List[Type.Param], declTypes: List[Type], args: List[Term], env: Map[String, Type], exp: Term)(implicit classTable: Classes): List[Type] = {
    // Check the argument's declTpe and properTpe against each other
    // - keep a map of typeParamName -> Set[properTpe]
    // - if declTpe is Type.Name:
    //    - is this a type param?
    //        - yes: assign properTpe to this type param (put it in the map)
    //        - no: end (return the map)
    // - if declTpe is Type.Apply(abstractName, params):
    //    - check that propertTpe is also a Type.Apply(name, args) and that the args correspond
    //      then, call ourself recursively on abstractName and name
    //      also call recursively on corresponding entries of params and args
    //      merge all the results together (i.e. the corresponding sets)
    // - if declTpe is a Type.Function(params, res):
    //    - check the params against each other
    //    - check the return type against each other
    // - else: error
    val paramNames = typeParams.map(_.name.value)

    val inferredTypeArgs = Types.inferArgTypes(args, env)
    val inferredTypeParams = (declTypes zip inferredTypeArgs).foldLeft(Map.empty[String, Type]) {
      case (paramTypes, (declType, properType)) =>
        Types.inferTypeParam(declType, properType, paramNames.toSet, exp, paramTypes)
    }

    // Check if we were able to infer all type parameters based on the arguments
    paramNames.find(!inferredTypeParams.contains(_)) match {
      case Some(paramName) => throw ParseError(s"Could not infer type of type parameter $paramName", exp.pos)
      case None => ()
    }

    // Transform this call `new tpe(..args)` into `new tpe[inferredTypes](..args)`
    val paramTypes = paramNames.map(inferredTypeParams.get(_).get)
    paramTypes
  }

  /**
   * Transforms a list of types.
   */
  def transformTypes(types: List[Type], typeParamTypes: Set[String] = Set())(implicit thisClass: Type, thisMethod: Option[String], classTable: Classes): List[Term] = {
    types.map(transformer(_, typeParamTypes))
  }

  // Returns true if `tpe` is a type parameter of `thisClass`
  private def isClassTypeParam(tpe: Type.Name)(implicit thisClass: Type, classTable: Classes) =
    classTable.get(thisClass).get.tparams.map(_.name.value).contains(tpe.value)

  private def getClassTypeParam(tpe: Type.Name)(implicit thisClass: Type, classTable: Classes) =
    classTable.get(thisClass).get.tparams.find(_.name.value == tpe.value).get

  private def isMethodTypeParam(tpe: Type.Name)(implicit thisClass: Type, thisMethod: Option[String], classTable: Classes) = {
    thisMethod match {
      case Some(methodName) => {
        val clazz = classTable.get(thisClass).get
        val method = clazz.getMethod(methodName).get
        method.tparams.map(_.name.value).contains(tpe.value)
      }
      case None => false
    }
  }

  private def getMethodTypeParam(name: Type.Name)(implicit thisClass: Type, thisMethod: Option[String], classTable: Classes) = {
    thisMethod match {
      case Some(methodName) => {
        val clazz = classTable.get(thisClass).get
        val method = clazz.getMethod(methodName).get
        method.tparams.find(_.name.value == name.value).get
      }
      case None => throw new InternalError(s"Method is missing from call to `getMethodTypeParam`")
    }
  }

  private def checkNumberOfTypeArgs(tpe: Type, typeArgs: List[Type], tparam: Type.Param) = {
    val numberOfExpectedTypeArgs = tparam.tparams.size
    if (typeArgs.size != numberOfExpectedTypeArgs) throw TypeError(s"Type $tpe expects ${tparam.tparams.size} type arguments but got ${typeArgs.size}.", tpe.pos)
  }

  // `typeParamTypes` are the types that are introduced by a type parameter
  // e.g. `trait CRDTProof[ T[A] <: CRDT[T[A]] ]`
  //      here the type parameter `T` must be a type constructor and introduces a new type `A` which is a valid only in the upper type bound of `T[A]]`, i.e. only valid in `CRDT[T[A]]`
  //      note that it could introduce several types, e.g.: `T[A[B]]`
  // `typeParamTypes` should only be passed when transforming the upper type bound of a type parameter
  def transformer(tpe: Type, typeParamTypes: Set[String] = Set())(implicit thisClass: Type, thisMethod: Option[String], classTable: Classes): Term = tpe match {
    case typeName: Type.Name if typeParamTypes.contains(typeName.value) || isMethodTypeParam(typeName) || isClassTypeParam(typeName) =>
      q"Type.Complex(${typeName.value}, Nil)" // this type refers to a type parameter of the method or class
    case Type.Name("Boolean") => q"Type.Boolean"
    case Type.Name("Int")     => q"Type.Int"
    case Type.Name("String")  => q"Type.String"
    case Type.Name(t) if classTable.contains(t) => {
        q"Type.Complex($t, Nil)"
    }
    case Type.Name(t) => throw ParseError(s"Unknown type: $t", tpe.pos)
    case Type.Apply(Type.Name(className), args) => {
      // Transform the type parameters
      val transformedArgs = transformTypes(args, typeParamTypes)
      q"Type.Complex($className, List(..$transformedArgs))"
    }
    case Type.Function(params, res) => {
      // Transform the types of the parameters and the result type
      val transformedParams = transformTypes(params, typeParamTypes)
      val transformedRes = transformer(res, typeParamTypes)
      q"Type.Function(List(..$transformedParams), $transformedRes)"
    }
    case _ => throw ParseError(s"Illegal type $tpe", tpe.pos)
  }

  // Checks that the given type is concrete
  // e.g. `Set[Int]` is a concrete type but `Set` is not because `Set` is a type constructor
  def isConcrete(tpe: Type)(implicit thisClass: Type, thisMethod: Option[String], classTable: Classes): Unit = tpe match {
    case typeName: Type.Name if isMethodTypeParam(typeName) => {
      val tparam = getMethodTypeParam(typeName)
      checkNumberOfTypeArgs(tpe, List(), tparam)
    }
    case typeName: Type.Name if isClassTypeParam(typeName) => {
      val tparam = getClassTypeParam(typeName)
      checkNumberOfTypeArgs(tpe, List(), tparam)
    }
    case Type.Name("Boolean") | Type.Name("Int") | Type.Name("String") => ()
    case Type.Name(t) if classTable.contains(t) => {
      val clazzOrTrait = classTable.get(t).get
      val tparams = clazzOrTrait.tparams
      if (tparams.size > 0) throw TypeError(s"Type $tpe expects ${tparams.size} type arguments but got 0.", tpe.pos)
    }
    case Type.Name(t) => throw TypeError(s"Unknown type: $t", tpe.pos)
    case Type.Apply(typeName @ Type.Name(_), args) if isMethodTypeParam(typeName) => {
      val tparam = getMethodTypeParam(typeName)
      checkNumberOfTypeArgs(tpe, args, tparam)
    }
    case Type.Apply(typeName @ Type.Name(_), args) if isClassTypeParam(typeName) => {
      val tparam = getClassTypeParam(typeName)
      checkNumberOfTypeArgs(tpe, args, tparam)
    }
    case Type.Apply(Type.Name(className), args) => {
      // Check that the number of type arguments is correct
      classTable.get(className) match {
        case Some(clazzOrTrait) => {
          val tparams = clazzOrTrait.tparams
          if (args.size != tparams.size) throw TypeError(s"Type $className expects ${tparams.size} type arguments but got ${args.size}: $tpe.", tpe.pos)
        }
        case None => throw ClassNotFound(className, tpe.pos)
      }
    }
    case Type.Function(params, res) => {
      params.foreach(isConcrete)
      isConcrete(res)
    }
    case _ => throw ParseError(s"Illegal type $tpe", tpe.pos)
  }

  /**
   * @return The type of the identifier in the environment.
   */
  def getType(ident: Term.Name, env: Map[String, Type]): Type = {
    val name = ident.value
    env.get(name) match {
      case Some(tpe) => tpe
      case None => throw UnknownIdentifierError(name, ident.pos)
    }
  }

  def getClassM(classType: Type, classTable: Classes, thisClass: Type): Option[ClassOrTrait] = {
    // Lookup the class of `classType`
    // If it's not found, it could be that `classType` is a type parameter
    // the type parameter may have an upper type bound (e.g. `V <: CRDT`) in which case we need to fetch the class of the upper type bound
    // note that it could have an upper type bound which is another type parameter so look it up recursively, e.g.: `A <: CRDT` and `B <: A`
    // so looking up B would need to lookup A which will lookup CRDT.
    // While this method can lookup the upper type bound of B which refers to a prior type parameter A,
    // it will not be allowed by the compiler because A must be a concrete type (i.e. a primitive or a class) and hence, B will need to subtype A
    // but we cannot subtype primitives or classes. So A and B will need to be the same, thus, there is no need to have 2 type parameters like this.
    classTable.getConcrete(classType) match {
      case Some(clazz) => Some(clazz)
      case None => {
        // Check if it is a type parameter and look it up if it is the case
        val maybeTParam = for {
          cType <- classType match {
            case t: Type.Name => Some(t)
            case _ => None // if `classType` is not a `Type.Name` then it must be a concrete type (it can't be a type param)
          }
          tparam <-
            if (isClassTypeParam(cType)(thisClass, classTable)) {
              Some(getClassTypeParam(cType)(thisClass, classTable))
            }
            else {
              None
            }
        } yield tparam

        maybeTParam.flatMap {
          case tparam => {
            //  look up the class of its upper type bound
            tparam.tbounds.hi.flatMap {
              case upperTypeBound =>
                getClassM(upperTypeBound, classTable, thisClass)
            }
          }
        }
      }
    }
  }

  def getClass(classType: Type, classTable: Classes, thisClass: Type): ClassOrTrait = {
    getClassM(classType, classTable, thisClass) match {
      case Some(clazz) => clazz
      case None => throw ClassNotFound(classType.toString, classType.pos)
    }
  }

  def getClass(classType: Type, classTable: Classes, env: Map[String, Type]): ClassOrTrait = {
    env.get("this") match {
      case Some(thisClass) => getClass(classType, classTable, thisClass)
      case None => throw new Error(s"Environment does not define `this` variable.")
    }
  }

  /**
   * @return The type of the field or method in the given class.
   */
  def getFieldType(field: Term.Name, classType: Type, env: Map[String, Type])(implicit classTable: Classes): Type = {
    val clazz = getClass(classType, classTable, env)
    // Look up the field in the class
    clazz.getField(field.value) match {
      case Some(f) => f.tpe
      case None => {
        // No such field
        throw FieldDoesNotExist(classType.toString, field.value, field.pos)
      }
    }
  }

  def getMethodType(method: Term.Name, classType: Type, env: Map[String, Type])(implicit classTable: Classes): Type = {
    val clazz = getClass(classType, classTable, env)
    // Look up the method in the class
    clazz.getMethod(method.value) match {
      case Some(m) => m.tpe
      case None => throw MethodNotFound(method.value, classType.toString, method.pos)
    }
  }

  def getMethodReturnType(method: Term.Name, classType: Type, env: Map[String, Type])(implicit classTable: Classes): Type = {
    val clazz = getClass(classType, classTable, env)
    // Look up the field in the class
    clazz.getMethod(method.value) match {
      case None =>
        throw MethodNotFound(method.value, classType.toString, method.pos)
      case Some(m) => m.retTpe
    }
  }

  private def getLiteralType(l: Lit): Type.Name = {
    val scalaType = l match {
      case _: Lit.Unit    => Type.Name("Unit")
      case _: Lit.Boolean => Type.Name("Boolean")
      case _: Lit.Int     => Type.Name("Int")
      case _: Lit.String  => Type.Name("String")
      case _ => throw ParseError(s"Illegal literal: $l", l.pos)
    }
    scalaType
  }

  private def inferFunCallType(fun: Term, args: List[Term], exp: Term, env: Map[String, Type])(implicit classTable: Classes): Type = {
    fun match {
      // If it's not a built-in function then look it up in the environment
      case fun: Term.Name => {
        getType(fun, env) match {
          case Type.Function(_, res) => res
          case tpe => throw TypeError(s"Can't apply $fun because it is not a function, it has type: $tpe", fun.pos)
        }
      }
      case _ => throw ParseError(s"Unknown function $fun", fun.pos)
    }
  }

  def getClassName(tpe: Type) = tpe match {
    case Type.Name(className) => className
    case Type.Apply(Type.Name(className), _) => className
    case t => throw TypeError(s"Unknown type $t", tpe.pos)
  }

  // Transforms a type parameter into a type
  // e.g. trait Foo[ A[B] <: CRDT[A[B]] ] -> then the type param A[B] <: CRDT[A[B]] will become the type A[B]
  def typeParamToType(tparam: Type.Param): Type = {
    tparam.name match {
      case name: Type.Name => {
        tparam.tparams match {
          case Nil => name
          case tparams => {
            val tparamTypes = tparams.map(typeParamToType)
            Type.Apply(name, tparamTypes)
          }
        }
      }
      case tpe: Name.Anonymous => Type.Name("_")
      case tpe => throw new InternalError(s"Unexpected name: $tpe")
    }
  }

  // Transforms a type parameter into a type of its name
  // but ignores nested types
  // e.g. trait Foo[ A[B] ] -> then the type param A[B] will become the type A (so it ignores the B)
  // This is because we cannot extend Foo with a concrete type, only a type constructor, so we will match the type constructor against A.
  // Note that the B may be needed if we have trait Foo[ A[B] <: CRDT[A[B]]] because then B can be inferred from the super type.
  // In that case we will use `typeParamToType` above.
  def tparamsToTypes(params: List[Type.Param]): List[Type] = {
    /**
     * Turn every type parameter into it's name
     * e.g. class Foo[S[_]]
     * Then the type parameter S[_] should be turned into Type.Name("s")
     * because you cannot extend `Foo` with a concrete type, only with a type constructor, e.g. `extends Foo[Set]` but not `Foo[Set[Int]]`
     */
    params.map {
      case Type.Param(_, Name(name), _, _, _, _) => Type.Name(name)
    }
  }

  def typeToTParam(tpe: Type): Type.Param = {
    val tparams = getTypeArgs(tpe).map(typeToTParam)
    Type.Param(Nil, Type.Name(getClassName(tpe)), tparams, Type.Bounds(None, None), Nil, Nil)
  }

  def classNameAndTParamsToType(className: Type.Name, tparams: List[Type.Param]): Type = if (tparams.isEmpty) className else Type.Apply(className, tparamsToTypes(tparams))
  def classNameAndTArgsToType(className: Type.Name, targs: List[Type]): Type = if (targs.isEmpty) className else Type.Apply(className, targs)

  def getTermParamType(param: Term.Param): Type = param.decltpe match {
    case Some(tpe) => tpe
    case None => throw ParseError(s"Missing type declaration for parameter: $param", param.pos)
  }

  private def getMethodCallOrFunctionCallType(methodName: Term.Name, classType: Type, args: List[Term], env: Map[String, Type], exp: Term)(implicit classTable: Classes) = {
    // `method` can be a method of the `classType` class
    // or it can be a field that is a function
    val clazz = getClass(classType, classTable, env)
    clazz.getMethod(methodName.value) match {
      case Some(method) => {
        // Infer the method's type arguments
        val typeArgs = Types.inferTypeParams(method, args, env, exp)
        // Fill the type arguments in the method
        val filledMethod = method.fillTypeParameters(method.name, Types.tparamsToTypes(method.tparams), typeArgs)
        // Fetch the method's return type
        filledMethod.retTpe
      }
      case None => {
        // It must be a field
        clazz.getField(methodName.value) match {
          case Some(field) => {
            field.tpe match {
              case Type.Function(_, res) => res
              case _ => throw TypeError(s"Cannot apply field ${methodName.value} because it is not a function", exp.pos)
            }
          }
          case None => throw MethodNotFound(methodName.value, clazz.name.value, exp.pos)
        }
      }
    }
  }

  def extendEnvWithPattern(pat: Pat, env: Map[String, Type], enumType: Type)(implicit classTable: Classes): Map[String, Type] = pat match {
    // Typed patterns such as: `case ins: T => ...`
    case Pat.Typed(Pat.Var(Term.Name(name)), tpe) => env + (name -> tpe)
    // Typed patterns such as: `case _: T => ...`
    case Pat.Typed(Pat.Wildcard(), _) => env
    // Wildcard patterns: `case _ => ...`
    case Pat.Wildcard() => env
    // Extraction patterns such as: `case Insert(pos, char) => ...`
    case Pat.Extract(ctorTerm @ Term.Name(className), vars) => {
      classTable.get(className) match {
        case Some(clazz @ Clazz(_, tparams, _, _, _, _, _, _, _)) => {
          val concreteClazz = if (tparams.nonEmpty) {
            // This class has type parameters
            // infer them from the expression being matched
            enumType match {
              case Type.Apply(_, targs) => {
                if (clazz.tparams.length != targs.length) throw TypeError(s"expression of type $enumType will never match pattern $pat", pat.pos)
                clazz.fillTypeParameters(targs)
              }
              case _ => throw TypeError(s"Cannot infer type parameters of $className from an expression of type $enumType", pat.pos)
            }
          } else {
            clazz
          }

          val args = concreteClazz.args
          if (vars.length != args.length) throw MatchError(s"$className takes ${args.length} arguments but pattern specifies ${vars.length} arguments", ctorTerm.pos)
          val fieldTypes = args.map(_.decltpe.get)
          val varsAndFieldTypes = vars zip fieldTypes

          // We do not support nested extraction patterns because pattern match expressions need to be exhaustive
          // so then we would have to check that the nested patterns are also exhaustive
          // e.g.: enum Foo { Bar(a: Option[Int]) | Baz() }
          //       someFoo match {
          //         case Bar(Some(a)) => ...
          //         case Baz() => ...
          //       }
          // The above pattern match is not exhaustive because we also need to handle the case of `Bar(None) => ...`
          varsAndFieldTypes.foldLeft(env) {
            case (env, (pat, fieldType)) => pat match {
              case Pat.Var(Term.Name(fieldName)) => env + (fieldName -> fieldType)
              case Pat.Wildcard() => env
            }
          }
        }
        case Some(_) => throw MatchError("Pattern matching is allowed on algebraic data types only.", ctorTerm.pos)
        case None => throw ClassNotFound(className, ctorTerm.pos)
      }
    }
    case _ => throw ParseError(s"Pattern not supported.", pat.pos)
  }

  def inferCaseType(c: Case, env: Map[String, Type], enumType: Type)(implicit classTable: Classes): Type = c match {
    case Case(pat, _, body) => {
      // Extend the environment with the constants introduced by the pattern
      // Then infer the type of the body using this extended environment
      val extendedEnv = extendEnvWithPattern(pat, env, enumType)
      inferType(body, extendedEnv)
    }
    case _ => throw ParseError(s"Expected a case but got something else.", c.pos)
  }

  def getThisClassFromEnv(env: Map[String, Type]): Type = {
    env.get("this") match {
      case Some(thisClass) => thisClass
      case None => throw new Error("Environment does not contain `this` variable.")
    }
  }

  /**
   * Infers the Scala type of the given expression.
   * If we encounter a concrete type (e.g. `Foo[Int]`) which does not yet exist (but the kind `Foo` exists)
   * then we extend the class table with a concrete `Foo[Int]` class.
   * @param exp The expression
   * @return The type of the expression
   */
  def inferType(exp: Term, env: Map[String, Type])(implicit classTable: Classes): Type = exp match {
    case l: Lit => getLiteralType(l)
    case t: Term.Name => getType(t, env)
    case _: Term.This => getType(Term.Name("this"), env)
    // the below pattern matches type casts
    case Term.ApplyType(Term.Select(_, Term.Name("asInstanceOf")), List(tpe)) => tpe
    // the below pattern matches method calls `obj.method(...)`
    case Term.Apply(Term.Select(obj, method), args) => {
      val objTpe = inferType(obj, env)
      getMethodCallOrFunctionCallType(method, objTpe, args, env, exp)
    }
    case Term.Apply(Term.ApplyType(Term.Select(obj, method), _), args) => {
      // Matches method calls with explicit type arguments
      // e.g. `obj.method[String](...)`
      // correctness of type arguments is checked by the compiler
      val objTpe = inferType(obj, env)
      getMethodCallOrFunctionCallType(method, objTpe, args, env, exp)
    }
    case Term.Select(qual, field) => {
      // infer type of `qual` then lookup type of `field` in that class
      val tpe = inferType(qual, env)
      getFieldType(field, tpe, env)
    }
    case Eta(Term.Select(qual, method)) => {
      // `qual.method _` --> infer the type of the method
      val tpe = inferType(qual, env)
      getMethodType(method, tpe, env)
    }
    // quantifier formulas (forall and exists)
    case Term.Apply(Term.Apply(Term.Name(quantifier), vars), List(body @ Term.Block(_))) if quantifier == "forall" || quantifier == "exists" => {
      val varTypes = CompilerUtil.varsToTypeMap(vars, env)
      inferType(body, env ++ varTypes) match {
        case tpe @ Type.Name("Boolean") => tpe
        case tpe => throw TypeError(s"Body of quantified formula has type $tpe, expected Boolean.", exp.pos)
      }
    }
    case Term.Apply(fun, args) => inferFunCallType(fun, args, exp, env)
    case Term.ApplyType(Term.Name("Dummy"), targs) => {
      // `Dummy` is a special function that is used by built-in classes
      // `Dummy[T]` types to `T`
      if (targs.size == 1)
        targs.head
      else
        throw ParseError(s"Dummy expects exactly one type argument but got ${targs.size}.", exp.pos)
    }
    case Term.ApplyInfix(lhs, op, _, _) => {
      val objTpe = inferType(lhs, env)
      if (CompilerUtil.isComparisonOperator(op.value) || CompilerUtil.isBooleanOperator(op.value) || CompilerUtil.isLogicOperator(op.value))
        Type.Name("Boolean")
      else if (isPrimitiveType(objTpe))
        objTpe // operators on primitive types result in the same type (e.g. int + int yields an int)
      else
        getFieldType(op, objTpe, env)
    }
    case Term.ApplyUnary(_, arg) => inferType(arg, env) // unary operators !arg and -arg do not change the type
    case Term.New(Init(tpe, _, argss)) => {
      val args = argss.headOption.getOrElse(List())

      if (!classTable.isClass(tpe))
        throw TypeError(s"Cannot instantiate $tpe", exp.pos)

      tpe match {
        case t @ Type.Name(name) if !classTable.contains(name) =>
          throw TypeError(s"Unknown type $t", exp.pos)
        case t @ Type.Name(name) if classTable.get(name).get.tparams.nonEmpty => {
          // It's a generic type but the user did not provide the type parameters explicitly
          // so we need to infer them based on the arguments provided to the class constructor
          val clazz = classTable.get(name).get.asInstanceOf[Clazz]
          val paramTypes = Types.inferTypeParams(clazz, args, env, exp)
          Type.Apply(t, paramTypes)
        }
        case Type.Name(_) => tpe
        case Type.Apply(Type.Name(className), _) if classTable.contains(className) =>
          tpe
        case _ => throw ClassNotFound(getClassName(tpe), exp.pos)
      }
    }
    case Term.If(_, thenB, _) => inferType(thenB, env)
    case block @ Term.Block(stats) => {
      // TODO: make it tail recursive!
      if (stats.isEmpty) throw ParseError(s"Illegal empty block.", exp.pos)

      // Go over the stats in the block
      // For every value definition we extend the environment
      val newEnv = stats.foldLeft(env) {
        case (env, valDef @ Defn.Val(Nil, List(Pat.Var(Term.Name(valueName))), declTpe, value)) => {
          val valueTpe = inferType(value, env)
          declTpe match {
            case None => ()
            case Some(tpe) => {
              implicit val thisClass = getThisClassFromEnv(env)
              if (!(valueTpe <:< tpe))
                throw TypeError(s"Value type $valueTpe does not correspond to declared type $tpe in $valDef", valueTpe.pos)
            }
          }

          env + (valueName -> valueTpe)
        }
        case (_, valDef @ Defn.Val(_, _, _, _)) => throw ParseError(s"Malformed value definition: $valDef", valDef.pos)
        case (env, _) => env
      }

      // Determine the type of the last statement
      // using the extended environment
      stats.last match {
        case last: Term => inferType(last, newEnv)
        case last => throw ParseError(s"Last statement of a block must be a term, but got $last", last.pos)
      }
    }
    case Term.Function(params, body) => {
      // fetch the declared types of the parameters
      val paramTypes = params map getTermParamType
      // extend the environment with the parameters
      val paramTypeMap = CompilerUtil.termParamsToTypeMap(params)
      val extendedEnv = env ++ paramTypeMap
      // infer the return type
      val resultType = inferType(body, extendedEnv)
      Type.Function(paramTypes, resultType)
    }
    case pm @ Term.Match(exp, cases) => {
      // Type of the pattern match is the common type of all cases
      val enumType = inferType(exp, env)
      val caseTypes = cases.map(inferCaseType(_, env, enumType))
      implicit val thisClass = getThisClassFromEnv(env)
      Types.getLeastCommonType(caseTypes) match {
        case Some(tpe) => tpe
        case None => throw TypeError(s"Cases have no common super type.", pm.pos)
      }
    }
    case _ => throw ParseError(s"Illegal expression $exp.", exp.pos)
  }
}
