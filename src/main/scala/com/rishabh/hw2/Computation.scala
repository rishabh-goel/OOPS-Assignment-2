package com.rishabh.hw2

import com.rishabh.hw2.Computation.SetExp.Assign

object Computation:

  import SetExp.*
  import scala.collection.mutable.Map
  import scala.collection.mutable.Set

  // Aliasing 'Any' to avoid hardcoding of Variable types
  type BasicType = Any

  // Map to store the macros
  val macroMap: scala.collection.mutable.Map[BasicType, SetExp] = scala.collection.mutable.Map()

  // Map to store variables and scopes
  val scopeMap: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map()

  // Map to store class definitions
  val classMap: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map()

  // Map to store objects of a class
  val objectMap: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map()

  // Map to store attributes an object can access
  val attrMap: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map()

  // Map to store method comoputation
  val methodMap: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map()

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
    case Field(name: String)
    case Constructor(expr: SetExp)
    case NewObject(className: String, expr: SetExp)
    case InvokeObject(className: SetExp, objectName: SetExp, attrName: SetExp)
    case GetObject(className: String, objectName: SetExp, expr: SetExp)
    case CreateMethod(methodName: String, methodType: String)
    case InvokeMethod(methodName: String, param1: SetExp, param2: SetExp)

    def Extends(superClass: SetExp) = {

      val parent: scala.collection.mutable.Map[BasicType, BasicType] = superClass.eval().asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]
      val child: scala.collection.mutable.Map[BasicType, BasicType] = this.eval().asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]
      val childClass = classMap.find(_._2 == child).map(_._1) match {
        case Some(m) => m.asInstanceOf[String]
      }

      val result: scala.collection.mutable.Map[BasicType, BasicType] = child.++(parent)

      classMap += (childClass -> result)
    }

    def eval(scope: scala.collection.mutable.Map[BasicType, BasicType] = scopeMap): BasicType =
      this match {

        case Value(i) => i

        case Variable(name) =>
          if(scope.contains(name))
            scope(name)
          else
            throw new Exception("Variable " + name + " not found in current scope")


        case Check(set, item) =>
          val s: scala.collection.mutable.Set[BasicType] = set.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          s.contains(item.eval(scope))

        case Assign(name, item) =>
          scope += (name -> item.eval(scope))
          scope
          (name -> item.eval(scope))

        case Insert(set, item) =>
          scope.update(set.eval(scope), set.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]] += item.eval(scope))
          scope(set.eval(scope))

        case Delete(set, item) =>
          scope.update(set.eval(scope), set.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]] -= item.eval(scope))
          scope(set.eval(scope))

        case Union(set1, set2) =>
          val s1: scala.collection.mutable.Set[BasicType] = set1.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          val s2: scala.collection.mutable.Set[BasicType] = set2.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          val result: scala.collection.mutable.Set[BasicType] = s1.union(s2)
          result

        case Intersect(set1, set2) =>
          val s1: scala.collection.mutable.Set[BasicType] = set1.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          val s2: scala.collection.mutable.Set[BasicType] = set2.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          val result: scala.collection.mutable.Set[BasicType] = s1.intersect(s2)
          result

        case Diff(set1, set2) =>
          val s1: scala.collection.mutable.Set[BasicType] = set1.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          val s2: scala.collection.mutable.Set[BasicType] = set2.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          val list_concat: scala.collection.mutable.Set[BasicType] = s1.union(s2)
          val list_intersect: scala.collection.mutable.Set[BasicType] = s1.intersect(s2)
          val result = list_concat.diff(list_intersect)
          result

        case Cross(set1, set2) =>
          val s1: scala.collection.mutable.Set[BasicType] = set1.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          val s2: scala.collection.mutable.Set[BasicType] = set2.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          val result: scala.collection.mutable.Set[BasicType] = s1.flatMap(a => s2.map(b => (a, b)))
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
            case Some(s) => s.asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]

            case None => {
              println("Create new scope - " + key)
              val temp: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map()
              scope += (key -> temp)
              temp
            }
          }
          op.eval(currentScope)

        case ClassDef(className, expr*) =>
          if(classMap.contains(className))
            classMap(className)
          else {
            if(expr.length == 0)
              classMap += (className -> scala.collection.mutable.Map())
            else {
              classMap += (className -> scala.collection.mutable.Map())
              expr.foreach(i => {
                classMap.update(className, classMap(className).asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]] += i.eval().asInstanceOf[(BasicType, BasicType)])
              })
              classMap(className)
            }
          }

        case Field(name) =>
          (name -> null)

        case Constructor(expr) =>
          expr.eval(scope)

        case NewObject(className, expr) =>
          if(objectMap.contains(className)){
            val list: scala.collection.mutable.ListBuffer[Any] = objectMap(className).asInstanceOf[scala.collection.mutable.ListBuffer[Any]]
            list += expr.eval()
            objectMap += (className -> list)
          }
          else {
            val list: scala.collection.mutable.ListBuffer[Any] = scala.collection.mutable.ListBuffer()
            list += expr.eval()
            objectMap += (className -> list)
          }

          attrMap += (expr.eval() -> classMap(className))

        case InvokeObject(className, objectName, attrName) =>
          if(!objectMap.contains(className.eval()))
            throw new Exception("Class "+ className.eval() + " does not have any object")
          else {
            val list: scala.collection.mutable.ListBuffer[BasicType] = objectMap(className.eval()).asInstanceOf[scala.collection.mutable.ListBuffer[BasicType]]

            if(!list.contains(objectName.eval()))
              throw new Exception("Object "+ objectName.eval() + " does not exist")
            else {
              val map: scala.collection.mutable.Map[BasicType, BasicType] = attrMap(objectName.eval()).asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]
              map(attrName.eval())
            }
          }

        case CreateMethod(methodName, methodType) =>
          methodMap += (methodName -> methodType)

        case InvokeMethod(methodName, param1, param2) =>
          val methodType: String = methodMap(methodName).asInstanceOf[String]

          val result = methodType match {
            case "UNION" => Union(param1, param2).eval()
            case "INTERSECT" => Intersect(param1, param2).eval()
            case "DIFFERENCE" => Diff(param1, param2).eval()
            case "CROSS_PRODUCT" => Cross(param1, param2).eval()
            case "CHECK" => Check(param1, param2).eval()
          }

      }

  @main def runArithExp: Unit =
    import SetExp.*


//    println(Assign("B", Union(Value(Set(1,2)), Value(Set(3,4)))).eval())
//    println(Variable("B").eval())
    ClassDef("MyClass", Assign("A", Value(Set(1,2))), Assign("B", Value(Set(3,4))), Assign("C", Union(Variable("A"), Variable("B"))), Field("f"), Constructor(Assign("f", Value(2)))).eval()
//    println(NewObject("MyClass", Value("z")).eval())
//    println(NewObject("MyClass", Value("y")).eval())
//
//    println(InvokeObject(Value("MyClass"), Value("z"), Value("f")).eval())

    //println(ClassDef("derivedClassName", Field("ff")) Extends ClassDef("MyClass"))

    ClassDef("DerivedClass", Field("ff")) Extends ClassDef("MyClass")
    println(ClassDef("MyClass").eval())
    println(ClassDef("DerivedClass").eval())
    println(classMap)



