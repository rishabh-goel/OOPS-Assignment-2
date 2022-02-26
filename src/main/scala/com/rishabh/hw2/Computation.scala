package com.rishabh.hw2

import com.rishabh.hw2.Computation.SetExp.Assign
import com.rishabh.hw2.Computation.attrMap

import java.lang
import scala.collection.{immutable, mutable}
import scala.collection.mutable.*

object Computation:

  import SetExp.*

  // Aliasing 'Any' to avoid hardcoding of Variable types
  type BasicType = Any

  // Map to store the macros
  val macroMap: Map[BasicType, SetExp] = Map()

  // Map to store variables and scopes
  val scopeMap: Map[BasicType, BasicType] = Map()

  // Map to store class definitions
  val classMap: Map[BasicType, BasicType] = Map()

  // Map to store objects of a class
  val objectMap: Map[BasicType, BasicType] = Map()

  // Map to store attributes an object can access
  val attrMap: Map[BasicType, BasicType] = Map()

  // Map to store inheritance
  val inheritanceMap: Map[BasicType, BasicType] = Map()

  // Map to store inheritance
  val nestedClassMap: Map[BasicType, BasicType] = Map()

  // Map to monitor access of fields and methods
  val accessMap: Map[BasicType, BasicType] = Map("public" -> Map(), "private" -> Map(), "protected" -> Map())

  enum SetExp:
    case Value(input: BasicType) // Get the value of the element passed
    case Variable(name: String) // Fetch the value assigned to the variable
    case Check(list: SetExp, item: SetExp) // Check if item present in set
    case Assign(name: String, item: SetExp) // Assign value to a variable
    case Insert(set: SetExp, item: SetExp) // Insert an item into the set
    case Delete(set: SetExp, item: SetExp) // Delete an item from the set
    case Union(set1: SetExp, set2: SetExp) // Union of 2 sets
    case Intersect(set1: SetExp, set2: SetExp) // Intersection of 2 sets
    case Diff(set1: SetExp, set2: SetExp) // Difference of 2 sets
    case Cross(set1: SetExp, set2: SetExp) // Cartesian Product of 2 sets
    case SetMacro(macroName: String, op: SetExp) // Create a macro in macroMap Map
    case GetMacro(macroName: String) // Fetch a macro from macroMap Map
    case Scope(name: String, op: SetExp) // Set Scope of variables
    case ClassDef(className: String, expr: SetExp*)
    case InnerClass(outerClass: String, innerClass: String)
    case Field(name: String, expr: SetExp*)
    case Constructor(expr: SetExp)
    case NewObject(className: String, expr: SetExp, parentObj: String*)
    case InvokeObject(className: SetExp, objectName: SetExp, attrName: SetExp, actualParams: SetExp*)
    case GetObject(className: String, objectName: SetExp, expr: SetExp)
    case CreateMethod(methodName: String, params: Params, methodType: SetExp*)
    case InvokeMethod(methodName: ListBuffer[SetExp], formalparam: SetExp, actualparam: Any)
    case Public(param: SetExp)
    case Private(param: SetExp)
    case Protected(param: SetExp)
    case Params(parameters: String*)

    def Extends(superClass: SetExp) = {
      val parent = superClass.eval().asInstanceOf[Map[BasicType, BasicType]]
      val child = this.eval().asInstanceOf[Map[BasicType, BasicType]]

      val childName = scopeMap.find(_._2 == child).map(_._1) match {
        case Some(m) => m.asInstanceOf[String]
        case None => throw new Exception("Child class not found")
      }

      val parentName = scopeMap.find(_._2 == parent).map(_._1) match {
        case Some(m) => m.asInstanceOf[String]
        case None => throw new Exception("Parent class not found")
      }

      if(inheritanceMap.contains(childName))
        throw new Exception("Cannot support multiple inheritance")
      else {
        val privateMembers = accessMap("private").asInstanceOf[Map[BasicType, BasicType]](parentName)

        val constructorMap = parent("constructor").asInstanceOf[Map[BasicType, BasicType]].++(child("constructor").asInstanceOf[Map[BasicType, BasicType]])
        val fieldMap = parent("field").asInstanceOf[Map[BasicType, BasicType]].++(child("field").asInstanceOf[Map[BasicType, BasicType]])
        privateMembers.asInstanceOf[Map[BasicType, BasicType]].keySet.foreach(i => {
          fieldMap -= i
        })

        val methodMap = parent("method").asInstanceOf[Map[BasicType, BasicType]].++(child("method").asInstanceOf[Map[BasicType, BasicType]])
        privateMembers.asInstanceOf[Map[BasicType, BasicType]].keySet.foreach(i => {
          methodMap -= i
        })

        val publicParentMap = accessMap("public").asInstanceOf[Map[BasicType, BasicType]](parentName).asInstanceOf[Map[BasicType, BasicType]]
        val publicChildMap = accessMap("public").asInstanceOf[Map[BasicType, BasicType]](childName).asInstanceOf[Map[BasicType, BasicType]]
        val newpublicChildMap = publicChildMap.++(publicParentMap)

        val protectedParentMap = accessMap("protected").asInstanceOf[Map[BasicType, BasicType]](parentName).asInstanceOf[Map[BasicType, BasicType]]
        val protectedChildMap = accessMap("protected").asInstanceOf[Map[BasicType, BasicType]](childName).asInstanceOf[Map[BasicType, BasicType]]
        val newprotectedChildMap = protectedChildMap.++(protectedParentMap)

        accessMap.update("public", accessMap("public").asInstanceOf[Map[BasicType, BasicType]] += (childName -> newpublicChildMap))
        accessMap.update("protected", accessMap("protected").asInstanceOf[Map[BasicType, BasicType]] += (childName -> newprotectedChildMap))

        inheritanceMap += (childName -> parentName)

        val map: Map[BasicType, BasicType] = Map("field" -> fieldMap, "constructor" -> constructorMap, "method" -> methodMap)
        scopeMap += (childName -> map)
      }
    }

    def eval(scope: Map[BasicType, BasicType] = scopeMap, access: Map[BasicType, BasicType]*): BasicType =
      this match {

        case Value(i) => i

        case Variable(name) =>
          if(scope.contains(name))
            scope(name)
          else
            //throw new Exception("Variable " + name + " not found in current scope")
            Value(name).eval()


        case Check(set, item) =>
          val s: Set[BasicType] = set.eval(scope).asInstanceOf[Set[BasicType]]
          s.contains(item.eval(scope))

        case Assign(name, item) =>
          scope += (name -> item.eval(scope))

        case Insert(set, item) =>
          scope.update(set.eval(scope), set.eval(scope).asInstanceOf[Set[BasicType]] += item.eval(scope))
          scope(set.eval(scope))

        case Delete(set, item) =>
          scope.update(set.eval(scope), set.eval(scope).asInstanceOf[Set[BasicType]] -= item.eval(scope))
          scope(set.eval(scope))

        case Union(set1, set2) =>
          val s1: Set[BasicType] = set1.eval(scope).asInstanceOf[Set[BasicType]]
          val s2: Set[BasicType] = set2.eval(scope).asInstanceOf[Set[BasicType]]
          val result: Set[BasicType] = s1.union(s2)
          result

        case Intersect(set1, set2) =>
          val s1: Set[BasicType] = set1.eval(scope).asInstanceOf[Set[BasicType]]
          val s2: Set[BasicType] = set2.eval(scope).asInstanceOf[Set[BasicType]]
          val result: Set[BasicType] = s1.intersect(s2)
          result

        case Diff(set1, set2) =>
          val s1: Set[BasicType] = set1.eval(scope).asInstanceOf[Set[BasicType]]
          val s2: Set[BasicType] = set2.eval(scope).asInstanceOf[Set[BasicType]]
          val list_concat: Set[BasicType] = s1.union(s2)
          val list_intersect: Set[BasicType] = s1.intersect(s2)
          val result = list_concat.diff(list_intersect)
          result

        case Cross(set1, set2) =>
          val s1: Set[BasicType] = set1.eval(scope).asInstanceOf[Set[BasicType]]
          val s2: Set[BasicType] = set2.eval(scope).asInstanceOf[Set[BasicType]]
          val result: Set[BasicType] = s1.flatMap(a => s2.map(b => (a, b)))
          result

        case SetMacro(macroName, op) =>
          macroMap += (macroName -> op)

        case GetMacro(macroName) =>
          macroMap(macroName)

        case Scope(name, op) =>
          val key = if(name.equals("")){
            "anon"
          }
          else
            name

          val sc = scope.get(key)
          val currentScope = sc match {
            case Some(s) => s.asInstanceOf[Map[BasicType, BasicType]]

            case None => {
              val temp: Map[BasicType, BasicType] = Map()
              scope += (key -> temp)
              temp
            }
          }
          op.eval(currentScope)

        case InnerClass(outerClass, innerClass) =>
          val inner = scope(innerClass).asInstanceOf[Map[String, Any]]
          val outer = scope(outerClass).asInstanceOf[Map[String, Any]]

          if(nestedClassMap.contains(outer))
            throw new Exception("Cannot create multiple nested classes")

          outer += ("innerClass" -> inner)
          nestedClassMap += (outerClass -> innerClass)
          scope.remove(innerClass)

        case ClassDef(className, expr*) =>
          val fieldMap: Map[String, Any] = Map()
          val constructorMap: Map[BasicType, BasicType] = Map()
          val methodMap: Map[String, ListBuffer[SetExp]] = Map()

          if(scope.contains(className))
            scope(className)
          else {
            val map: Map[BasicType, BasicType] = Map("field" -> fieldMap, "constructor" -> constructorMap, "method" -> methodMap)
            val publicClassMap: Map[BasicType, BasicType] = Map(className -> Map())
            val privateClassMap: Map[BasicType, BasicType] = Map(className -> Map())
            val protectedClassMap: Map[BasicType, BasicType] = Map(className -> Map())

            expr.foreach(i => {
              i.eval(map, publicClassMap, privateClassMap, protectedClassMap)
            })

            scope += (className -> map)
            accessMap.update("public", accessMap("public").asInstanceOf[Map[BasicType, BasicType]] += (publicClassMap.head._1 -> publicClassMap.head._2))
            accessMap.update("private", accessMap("private").asInstanceOf[Map[BasicType, BasicType]] += (privateClassMap.head._1 -> privateClassMap.head._2))
            accessMap.update("protected", accessMap("protected").asInstanceOf[Map[BasicType, BasicType]] += (protectedClassMap.head._1 -> protectedClassMap.head._2))
            scope(className)
          }

        case Field(name, expr*) =>
          val fieldMap = scope("field").asInstanceOf[Map[BasicType, BasicType]]

          if(expr.length == 0)
            fieldMap += (name -> null)
            Map(name -> null)
          else
            fieldMap += (name -> expr(0).eval(scope))
            Map(name -> expr(0).eval(scope))

        case Constructor(expr) =>
          val constructorMap = scope("constructor").asInstanceOf[Map[BasicType, BasicType]]
          expr.eval(constructorMap)

        case Params(params*) =>
          params


        case CreateMethod(methodName, params, expr*) =>
          val methodMap = scope("method").asInstanceOf[Map[BasicType, ListBuffer[SetExp]]]
          val list = new ListBuffer[SetExp]
          list.addOne(params)

          expr.foreach(i => list.addOne(i))
          methodMap += (methodName -> list)
          Map(methodName -> list)


        case InvokeMethod(method, formalparam, actualparam) =>
          val fp: immutable.ArraySeq[String] = formalparam.asInstanceOf[SetExp].eval(scope).asInstanceOf[immutable.ArraySeq[String]]
          val ap: immutable.ArraySeq[SetExp] = actualparam.asInstanceOf[immutable.ArraySeq[SetExp]]

          if(fp.length.equals(ap.length)) {
            val list = fp.zip(ap)
            list.foreach(i => {
              scope += (i._1 -> i._2.eval(scope))
            })

            method.zipWithIndex.foreach(i => {
              if(i._2 != method.length-1) {
                i._1.asInstanceOf[SetExp].eval(scope)
              }
            })

            method(method.length-1).asInstanceOf[SetExp].eval(scope)
          }
          else {
            throw new Exception("Parameters missing")
          }


        case NewObject(className, expr, parentObj*) =>
          if(objectMap.contains(className)){
            if(parentObj.length == 0) {
              val list: ListBuffer[Any] = objectMap(className).asInstanceOf[ListBuffer[Any]]
              list += expr.eval()
              objectMap += (className -> list)
              attrMap += (expr.eval() -> scope(className).asInstanceOf[Map[String, Any]].clone())

              val objectAttr = attrMap(expr.eval(scope)).asInstanceOf[Map[String, Any]]
              objectAttr -= "innerClass"
              attrMap += (expr.eval(scope) -> objectAttr)
            }
            else {
              if(nestedClassMap.exists(_._2 == className)){
                val list: ListBuffer[Any] = objectMap(className).asInstanceOf[ListBuffer[Any]]
                list += expr.eval()
                objectMap += (className -> list)

                val outerClassName = nestedClassMap.find(_._2 == className).map(_._1) match {
                  case Some(m) => m
                  case None => throw new Exception("Outer class not found")
                }
                val outerClassObjects = objectMap(outerClassName).asInstanceOf[ListBuffer[Any]]

                if(outerClassObjects.contains(parentObj(0))) {
                  val outerClassAttr = scope(outerClassName).asInstanceOf[Map[String, Any]]
                  println("MyClass map is " + outerClassAttr)
                  attrMap += (expr.eval() -> outerClassAttr("innerClass"))
                }
                else {
                  throw new Exception("Parent object doesn't exist")
                }
              }
              else {
                throw new Exception(className + " is not an inner class. Parent object not needed")
              }
            }

            val objectAttr = attrMap(expr.eval(scope)).asInstanceOf[Map[String, Any]].clone()
            val objectConstructor = objectAttr("constructor").asInstanceOf[Map[String, Any]].clone()
            val objectField = objectAttr("field").asInstanceOf[Map[String, Any]].clone()
            val result = objectConstructor.++(objectField)

            attrMap.update(expr.eval(scope), objectAttr += ("field" -> result))
            attrMap.update(expr.eval(scope), objectAttr -= "constructor")
          }
          else {
            if(parentObj.length == 0) {
              val list: ListBuffer[Any] = ListBuffer()
              list += expr.eval(scope)
              objectMap += (className -> list)
              attrMap += (expr.eval(scope) -> scope(className).asInstanceOf[Map[String, Any]].clone())

              val objectAttr = attrMap(expr.eval(scope)).asInstanceOf[Map[String, Any]]
              objectAttr -= "innerClass"
              attrMap += (expr.eval(scope) -> objectAttr)
            }
            else {
              if(nestedClassMap.exists(_._2 == className)){
                val outerClassName = nestedClassMap.find(_._2 == className).map(_._1) match {
                  case Some(m) => m
                  case None => throw new Exception("Outer class not found")
                }
                val outerClassObjects = objectMap(outerClassName).asInstanceOf[ListBuffer[Any]]

                if(outerClassObjects.contains(parentObj(0))) {
                  val list: ListBuffer[Any] = ListBuffer()
                  list += expr.eval()
                  objectMap += (className -> list)
                  val outerClassAttr = scope(outerClassName).asInstanceOf[Map[String, Any]]
                  println("MyClass map is " + outerClassAttr)
                  attrMap += (expr.eval() -> outerClassAttr("innerClass"))
                }
                else {
                  throw new Exception("Parent object doesn't exist")
                }
              }
              else {
                throw new Exception(className + " is not an inner class. Parent object not needed")
              }
            }

            val objectAttr = attrMap(expr.eval(scope)).asInstanceOf[Map[String, Any]].clone()
            val objectConstructor = objectAttr("constructor").asInstanceOf[Map[String, Any]].clone()
            val objectField = objectAttr("field").asInstanceOf[Map[String, Any]].clone()
            val result = objectConstructor.++(objectField)

            attrMap.update(expr.eval(scope), objectAttr += ("field" -> result))
            attrMap.update(expr.eval(scope), objectAttr -= "constructor")
          }

        case InvokeObject(className, objectName, attrName, actualParams*) =>
          if(!objectMap.contains(className.eval(scope)))
            throw new Exception("Class "+ className.eval(scope) + " does not have any object")
          else {
            val list: ListBuffer[BasicType] = objectMap(className.eval(scope)).asInstanceOf[ListBuffer[BasicType]]

            if(!list.contains(objectName.eval(scope)))
              throw new Exception("Object "+ objectName.eval(scope) + " does not exist")
            else {
              val map: Map[BasicType, BasicType] = attrMap(objectName.eval(scope)).asInstanceOf[Map[BasicType, BasicType]]

              // Executing the attribute requested by constructor
              if(actualParams.length == 0){
                val fieldMap = map("field").asInstanceOf[Map[BasicType, BasicType]]
                fieldMap(attrName.eval(scope))
              }
              else{
                val methodMap = map("method").asInstanceOf[Map[BasicType, BasicType]]
                val method = methodMap(attrName.eval(scope)).asInstanceOf[ListBuffer[SetExp]]
                val formalParams = method(0)
                val tempMap: Map[Any, Any] = Map()
                InvokeMethod(method.drop(1), formalParams, actualParams).eval(tempMap)
              }
            }
          }

        case Public(expr) =>
          val publicMap = access(0).asInstanceOf[Map[BasicType, BasicType]]
          val result = expr.eval(scope).asInstanceOf[Map[BasicType, BasicType]]
          publicMap.update(publicMap.head._1, publicMap(publicMap.head._1).asInstanceOf[Map[BasicType, BasicType]] += (result.head._1 -> result.head._2))


        case Private(expr) =>
          val privateMap = access(1).asInstanceOf[Map[BasicType, BasicType]]
          val result = expr.eval(scope).asInstanceOf[Map[BasicType, BasicType]]
          privateMap.update(privateMap.head._1, privateMap(privateMap.head._1).asInstanceOf[Map[BasicType, BasicType]] += (result.head._1 -> result.head._2))


        case Protected(expr) =>
          val protectedMap = access(2).asInstanceOf[Map[BasicType, BasicType]]
          val result = expr.eval(scope).asInstanceOf[Map[BasicType, BasicType]]
          protectedMap.update(protectedMap.head._1, protectedMap(protectedMap.head._1).asInstanceOf[Map[BasicType, BasicType]] += (result.head._1 -> result.head._2))

      }

  @main def runArithExp: Unit =
    import SetExp.*

//    ClassDef("MyClass", Public(Field("f")), Private(Field("ff", Value(2))), Constructor(Assign("fff", Value(5))), Private(CreateMethod("m1", Params("a", "b"), Assign("c", Union(Variable("a"), Variable("b"))), Variable("c"))), CreateMethod("m2", Params("X", "Y"), Union(Variable("X"), Variable("Y")))).eval()
//    ClassDef("DerivedClass", Public(Field("e")), Private(Field("ee", Value(2))), Protected(Field("eeee", Value(5))), Protected(Field("eeeee")), Constructor(Assign("eee", Value(10)))).eval()
//    ClassDef("DerivedClass") Extends ClassDef("MyClass")
//    NewObject("MyClass", Variable("z")).eval()
//    NewObject("DerivedClass", Variable("y")).eval()
//    println(classMap("MyClass"))
//    println(classMap("DerivedClass"))
//    println(objectMap)
//    println(attrMap)
//    println("public - "+accessMap("public"))
//    println("private - "+accessMap("private"))
//    println("protected - "+accessMap("protected"))
//    println(InvokeObject(Value("MyClass"), Value("z"), Value("m1"), Value(Set(1,2)), Value(Set(3,4))).eval())

    ClassDef("MyClass", Public(Field("f")), Constructor(Assign("ff", Value(5))), Private(CreateMethod("m1", Params("a", "b"), Assign("c", Union(Variable("a"), Variable("b"))), Variable("c")))).eval()
    ClassDef("MyInner", Public(Field("e")), Constructor(Assign("ee", Value(100))), Private(CreateMethod("m2", Params("a", "b"), Assign("c", Intersect(Variable("a"), Variable("b"))), Variable("c")))).eval()
    InnerClass("MyClass", "MyInner").eval()
    NewObject("MyClass", Variable("z")).eval()
    NewObject("MyClass", Variable("AB")).eval()
    NewObject("MyInner", Variable("innerZ"), "z").eval()
    NewObject("MyInner", Variable("innerZZZZ"), "z").eval()
    ClassDef("DerivedClass", Public(Field("e")), Private(Field("ee", Value(2))), Protected(Field("eeee", Value(5))), Protected(Field("eeeee")), Constructor(Assign("eee", Value(10)))).eval()
    ClassDef("DerivedClass") Extends ClassDef("MyClass")
    NewObject("DerivedClass", Variable("y")).eval()
    println(scopeMap("MyClass"))
    println(scopeMap("DerivedClass"))
    println(objectMap)
    println(attrMap)
    println(InvokeObject(Value("MyClass"), Value("z"), Value("m1"), Value(Set(1,2)), Value(Set(3,4))).eval())
    println(InvokeObject(Value("MyInner"), Value("innerZ"), Value("m2"), Value(Set(1,2)), Value(Set(1,4))).eval())
    println(scopeMap)
    println(objectMap)
    println(attrMap)




