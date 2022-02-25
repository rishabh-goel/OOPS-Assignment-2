package com.rishabh.hw2

import com.rishabh.hw2.Computation.SetExp.Assign

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

  // Map to monitor access of fields and methods
  val accessMap: Map[BasicType, BasicType] = Map("public" -> Map(), "private" -> Map(), "protected" -> Map())

//  // Map to monitor public access modifier
//  val publicMap: Map[BasicType, BasicType] = Map()
//
//  // Map to monitor private access modifier
//  val privateMap: Map[BasicType, BasicType] = Map()
//
//  // Map to monitor protected access modifier
//  val protectedMap: Map[BasicType, BasicType] = Map()

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
    case Field(name: String, expr: SetExp*)
    case Constructor(expr: SetExp)
    case NewObject(className: String, expr: SetExp)
    case InvokeObject(className: SetExp, objectName: SetExp, attrName: SetExp)
    case GetObject(className: String, objectName: SetExp, expr: SetExp)
    case CreateMethod(methodName: String, methodType: SetExp)
    case InvokeMethod(methodName: String, param: SetExp*)
    case Public(param: SetExp)
    case Private(param: SetExp)
    case Protected(param: SetExp)

    def Extends(superClass: SetExp) = {
      val parent = superClass.eval().asInstanceOf[Map[BasicType, BasicType]]
      val child = this.eval().asInstanceOf[Map[BasicType, BasicType]]

      val constructorArray = parent("constructor").asInstanceOf[Map[BasicType, BasicType]].++(child("constructor").asInstanceOf[Map[BasicType, BasicType]])
      val fieldMap = parent("field").asInstanceOf[Map[BasicType, BasicType]].++(child("field").asInstanceOf[Map[BasicType, BasicType]])
      val methodMap = parent("method").asInstanceOf[Map[BasicType, BasicType]].++(child("method").asInstanceOf[Map[BasicType, BasicType]])

      val childName = classMap.find(_._2 == child).map(_._1) match {
        case Some(m) => m.asInstanceOf[String]
      }

      val map: Map[BasicType, BasicType] = Map("field" -> fieldMap, "constructor" -> constructorArray, "method" -> methodMap)
      classMap += (childName -> map)
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

        case ClassDef(className, expr*) =>
          val fieldMap: Map[String, Any] = Map()
          val constructorArray: Map[BasicType, BasicType] = Map()
          val methodMap: Map[String, SetExp] = Map()

          if(classMap.contains(className))
            classMap(className)
          else {
            val map: Map[BasicType, BasicType] = Map("field" -> fieldMap, "constructor" -> constructorArray, "method" -> methodMap)
            val publicClassMap: Map[BasicType, BasicType] = Map(className -> Map())
            val privateClassMap: Map[BasicType, BasicType] = Map(className -> Map())
            val protectedClassMap: Map[BasicType, BasicType] = Map(className -> Map())

            expr.foreach(i => {
              i.eval(map, publicClassMap, privateClassMap, protectedClassMap)
            })

            classMap += (className -> map)
            accessMap.update("public", accessMap("public").asInstanceOf[Map[BasicType, BasicType]] += (publicClassMap.head._1 -> publicClassMap.head._2))
            accessMap.update("private", accessMap("private").asInstanceOf[Map[BasicType, BasicType]] += (privateClassMap.head._1 -> privateClassMap.head._2))
            accessMap.update("protected", accessMap("protected").asInstanceOf[Map[BasicType, BasicType]] += (protectedClassMap.head._1 -> protectedClassMap.head._2))

            classMap(className)
          }

        case Field(name, expr*) =>
          val fieldMap = scope("field").asInstanceOf[Map[BasicType, BasicType]]

          if(expr.length == 0)
            fieldMap += (name -> null)
            Map(name -> null)
          else
            fieldMap += (name -> expr(0).eval())
            Map(name -> expr(0).eval())

        case Constructor(expr) =>
          val constructorMap = scope("constructor").asInstanceOf[Map[BasicType, BasicType]]
          expr.eval(constructorMap)


        case CreateMethod(methodName, expr) =>
          val methodMap = scope("method").asInstanceOf[Map[BasicType, SetExp]]
          methodMap += (methodName -> expr)


        case InvokeMethod(methodName, param*) =>
          //TODO: How to extract formal params and replace with actual params

        case NewObject(className, expr) =>
          if(objectMap.contains(className)){
            val list: ListBuffer[Any] = objectMap(className).asInstanceOf[ListBuffer[Any]]
            list += expr.eval()
            objectMap += (className -> list)
          }
          else {
            val list: ListBuffer[Any] = ListBuffer()
            list += expr.eval()
            objectMap += (className -> list)
          }

          attrMap += (expr.eval() -> classMap(className))

        case InvokeObject(className, objectName, attrName) =>
          if(!objectMap.contains(className.eval()))
            throw new Exception("Class "+ className.eval() + " does not have any object")
          else {
            val list: ListBuffer[BasicType] = objectMap(className.eval()).asInstanceOf[ListBuffer[BasicType]]

            if(!list.contains(objectName.eval()))
              throw new Exception("Object "+ objectName.eval() + " does not exist")
            else {
              val map: Map[BasicType, BasicType] = attrMap(objectName.eval()).asInstanceOf[Map[BasicType, BasicType]]

              // Executing the constructor
              val constructorMap = map("constructor").asInstanceOf[Map[BasicType, BasicType]]
              val fieldMap = map("field").asInstanceOf[Map[BasicType, BasicType]].++(constructorMap)
              val newfieldMap = fieldMap
              map.put("constuctor", constructorMap.clear())
              map.put("field", newfieldMap)

              // Executing the attribute requested by constructor
              attrMap(attrName)
            }
          }

        case Public(expr) =>
          val publicMap = access(0).asInstanceOf[Map[BasicType, BasicType]]
          publicMap += (publicMap.head._1 -> expr.eval(scope))


        case Private(expr) =>
          val privateMap = access(1).asInstanceOf[Map[BasicType, BasicType]]
          privateMap += (privateMap.head._1 -> expr.eval(scope))

        case Protected(expr) =>
          val protectedMap = access(2).asInstanceOf[Map[BasicType, BasicType]]
          protectedMap += (protectedMap.head._1 -> expr.eval(scope))

      }

  @main def runArithExp: Unit =
    import SetExp.*

    ClassDef("MyClass", Field("f"), Field("ff", Value(2)), Constructor(Assign("fff", Value(5))), CreateMethod("m1", Value(1)), CreateMethod("m2", Union(Variable("X"), Variable("Y")))).eval()
    ClassDef("DerivedClass", Public(Field("e")), Private(Field("ee", Value(2))), Protected(Field("eeeee", Value(5))), Constructor(Assign("eee", Value(10)))).eval()
    ClassDef("DerivedClass") Extends ClassDef("MyClass")
    NewObject("MyClass", Variable("z")).eval()
    NewObject("DerivedClass", Variable("y")).eval()
    println(classMap("MyClass"))
    println(classMap("DerivedClass"))
    println(objectMap)
    println(attrMap)
    println(accessMap)




