package be.vub.kdeporre.verifx.Compiler

import cats.data.State
import cats.implicits._

class IR2LanguageCompiler[C, M <: C, S](val plugin: Plugin[C, M, S]) {
  import be.vub.kdeporre.verifx.Compiler.IR._

  def compileArg(arg: Defn.Arg) = arg match {
    case Defn.Arg(name, tpe) => compile(tpe) map {
      ctpe => Plugin.Arg(name, ctpe)
    }
  }

  def compileField(field: Defn.Field) = field match {
    case Defn.Field(mods, name, tpe) =>
      for {
        cmods <- mods traverse compile
        ctpe <- compile(tpe)
      } yield Plugin.Field(cmods, name, ctpe)
  }

  def compileEnumCtor(ctor: Defn.DataCtor) = ctor match {
    case Defn.DataCtor(name, tparams, fields) =>
      for {
        ctparams <- tparams traverse compile
        cfields <- fields traverse compileField
      } yield Plugin.EnumCtor(name, ctparams, cfields)
  }

  def compilePattern(pat: Pat.Pat): State[S, Plugin.Pat.Pat] = pat match {
    case Pat.Typed(p, tpe) => {
      for {
        ctype <- compile(tpe)
        cpat <- compilePattern(p)
      } yield Plugin.Pat.Typed(cpat.asInstanceOf[Plugin.Pat.VarOrWildcard], ctype)
    }
    case Pat.Var(name) => State.pure[S, Plugin.Pat.Pat](Plugin.Pat.Var(name))
    case Pat.Wildcard => State.pure[S, Plugin.Pat.Pat](Plugin.Pat.Wildcard)
    case Pat.Extract(tpe, vars) => {
      for {
        ctype <- compile(tpe)
        cvars <- vars traverse compilePattern
      } yield Plugin.Pat.Extract(ctype, cvars.asInstanceOf[List[Plugin.Pat.VarOrWildcard]])
    }
  }

  def compileCase(casee: Term.Case) = casee match {
    case Term.Case(pat, body) =>
      for {
        cpat <- compilePattern(pat)
        cbody <- compile(body)
      } yield Plugin.Case(cpat, cbody)
  }

  def compileOptionalStat(s: Option[IR]): State[S, Option[C]] = {
    s match {
      case None => State.pure[S, Option[C]](None)
      case Some(stat) => compile(stat).map(Some(_))
    }
  }

  def compileMethodOrPred(m: Defn.MethodOrPredicate) = m match {
    case Defn.MethodDef(name, mods, tparams, args, body, retType, isRecursive) =>
      for {
        cmods <- mods traverse compile
        ctparams <- tparams traverse compile
        cargs <- args traverse compileArg
        cbody <- compile(body)
        cRetType <- compile(retType)
        cMethodDef <- plugin.makeMethodDef(name, cmods, ctparams, cargs, cbody, cRetType, isRecursive)
      } yield cMethodDef
    case Defn.CtxDef(methodName, tparams, args, body) =>
      for {
        ctparams <- tparams traverse compile
        cargs <- args traverse compileArg
        cbody <- compile(body)
        cCtxDef <- plugin.makeContextDef(methodName, ctparams, cargs, cbody)
      } yield cCtxDef
    case Defn.PreDef(methodName, tparams, args, body) =>
      for {
        ctparams <- tparams traverse compile
        cargs <- args traverse compileArg
        cbody <- compile(body)
        cPreDef <- plugin.makePreconditionDef(methodName, ctparams, cargs, cbody)
      } yield cPreDef
    case Defn.InvDef(methodName, tparams, args, body) =>
      for {
        ctparams <- tparams traverse compile
        cargs <- args traverse compileArg
        cbody <- compile(body)
        cInvDef <- plugin.makeInvariantDef(methodName, ctparams, cargs, cbody)
      } yield cInvDef
  }

  def compileStat(stat: IR): State[S, C] = stat match {
    case mOrP: Defn.MethodOrPredicate => compileMethodOrPred(mOrP).asInstanceOf[State[S, C]]
    case _ => compile(stat)
  }

  def compile(stat: IR): State[S, C] = {
    stat match {
      // Types
      case Type.Boolean => State.pure(plugin.bool_type)
      case Type.Int     => State.pure(plugin.int_type)
      case Type.String  => State.pure(plugin.string_type)
      case Type.Complex(name, targs) =>
        for {
          ctargs <- targs.traverse(compile)
          ctype  <- plugin.makeComplexType(name, ctargs)
        } yield ctype
      case Type.TypeParameter(name, tparams, upperTypeBound) => {
        for {
          ctparams <- tparams traverse compile
          ctparam <- upperTypeBound match {
            case None =>
              plugin.makeTypeParam(name, ctparams, None)
            case Some(upperType) =>
              for {
                cupperType <- compile(upperType)
                ctparam <- plugin.makeTypeParam(name, ctparams, Some(cupperType))
              } yield ctparam
          }
        } yield ctparam
      }
      case Type.Function(params, res) =>
        for {
          cparams <- params traverse compile
          cres <- compile(res)
          cfunctionType <- plugin.makeFunctionType(cparams, cres)
        } yield cfunctionType

      // Terms
      case l: Term.Literal => plugin.makeLiteral(l)
      case Term.Reference(name) => plugin.makeReference(name)
      case Term.BinaryOperation(operator, left, right) =>
        for {
          l <- compile(left)
          r <- compile(right)
          op <- plugin.makeBinaryOperation(operator, l, r)
        } yield op

      case Term.UnaryOperation(operator, arg) =>
        for {
          carg <- compile(arg)
          op <- plugin.makeUnaryOperation(operator, carg)
        } yield op

      case Term.InstantiateClass(clazz, tparams, args) =>
        for {
          ctparams <- tparams traverse compile
          cargs <- args traverse compile
          classInst <- plugin.instantiateClass(clazz, ctparams, cargs)
        } yield classInst

      case Term.FieldAccess(classType, obj, field, tpe) =>
        for {
          clazz <- compile(classType)
          cobj <- compile(obj)
          ctpe <- compile(tpe)
          fa <- plugin.makeFieldAccess(clazz, cobj, field, ctpe)
        } yield fa

      case Term.MethodReference(classType, obj, method, signature) =>
        for {
          clazz <- compile(classType)
          cobj <- compile(obj)
          csignature <- compile(signature)
          ref <- plugin.makeMethodReference(clazz, cobj, method, csignature)
        } yield ref

      case Term.MethodCall(classType, obj, method, targs, args, argTypes) =>
        for {
          clazz <- compile(classType)
          ctargs <- targs traverse compile
          cobj <- compile(obj)
          cargs <- args traverse compile
          cargTypes <- argTypes traverse compile
          methodCall <- plugin.makeMethodCall(clazz, cobj, method, ctargs, cargs, cargTypes)
        } yield methodCall

      case Term.If(cond, thenb, elseb) =>
        for {
          ccond  <- compile(cond)
          cthenb <- compile(thenb)
          celseb <- compile(elseb)
          ifStat <- plugin.makeIf(ccond, cthenb, celseb)
        } yield ifStat

      case Term.Block(stats) => stats match {
        case stat :: Nil => compile(stat) // it is a single expression, ignore the block
        case _ =>
          for {
            cstats <- stats traverse compile
            block <- plugin.makeBlock(cstats)
          } yield block
      }

      case Term.Function(params, body) =>
        for {
          cparams <- params traverse compileArg
          cbody <- compile(body)
          cfun <- plugin.makeFunction(cparams, cbody)
        } yield cfun

      case Term.FunctionCall(fun, args) =>
        for {
          cargs <- args traverse compile
          cfun <- compile(fun)
          cfunCall <- plugin.makeFunctionCall(cfun, cargs)
        } yield cfunCall

      case Term.Match(exp, cases) =>
        for {
          cexp <- compile(exp)
          ccases <- cases traverse compileCase
          cmatch <- plugin.makePatternMatch(cexp, ccases)
        } yield cmatch

      case mod: Mod.Mod => plugin.makeMod(mod)

      // Declarations
      case Decl.Val(name, tpe) =>
        for {
          ctpe <- compile(tpe)
          valDecl <- plugin.makeValDecl(name, ctpe)
        } yield valDecl

      case Decl.Method(name, mods, tparams, args, retType) =>
        for {
          cmods <- mods traverse compile
          ctparams <- tparams traverse compile
          ctpe <- compile(retType)
          cargs <- args traverse compileArg
          methodDecl <- plugin.makeMethodDecl(cmods, name, ctparams, cargs, ctpe)
        } yield methodDecl

      // Definitions
      case Defn.ValDef(name, tpe, value) =>
        for {
          ctpe   <- compile(tpe)
          cvalue <- compile(value)
          valDef <- plugin.makeValDef(name, ctpe, cvalue)
        } yield valDef

      case Defn.ClassDef(annots, name, tparams, fields, methods, inheritedMethods, maybeSuper) =>
        for {
          ctparams <- tparams traverse compile
          cfields <- fields traverse compileField
          cmethods <- methods traverse compileMethodOrPred
          cInheritedMethods <- inheritedMethods traverse compileMethodOrPred
          cSuper <- compileOptionalStat(maybeSuper)
          classDef <- plugin.makeClassDef(annots.map(_.name), name, ctparams, cfields, cmethods, cInheritedMethods, cSuper)
        } yield classDef

      case Defn.Trait(mods, name, tparams, stats, maybeSuper) =>
        for {
          cmods <- mods traverse compile
          ctparams <- tparams traverse compile
          cstats <- stats traverse compileStat
          cSuper <- compileOptionalStat(maybeSuper)
          traitDef <- plugin.makeTraitDef(cmods, name, ctparams, cstats, cSuper)
        } yield traitDef

      case Defn.Enum(name, tparams, ctors) =>
        for {
          ctparams <- tparams traverse compile
          cCtors <- ctors traverse compileEnumCtor
          enumDef <- plugin.makeEnumDef(name, ctparams, cCtors)
        } yield enumDef

      // Imports
      case Import(pkg) => plugin.makeImport(pkg)

      // Logic
      case Logic.Variable(name, tpe) =>
        for {
          ctpe <- compile(tpe)
          cvar <- plugin.makeLogicVar(name, ctpe)
        } yield cvar

      case Logic.Implication(antecedent, consequent) =>
        for {
          cante <- compile(antecedent)
          cconse <- compile(consequent)
          cimplication <- plugin.makeImplication(cante, cconse)
        } yield cimplication

      case Logic.Forall(vars, body) =>
        for {
          cvars <- vars.toList traverse compile
          cbody <- compile(body)
          cforall <- plugin.makeForall(cvars.toSet, cbody)
        } yield cforall

      case Logic.Exists(vars, body) =>
        for {
          cvars <- vars.toList traverse compile
          cbody <- compile(body)
          cexists <- plugin.makeExists(cvars.toSet, cbody)
        } yield cexists

    }
  }
}
