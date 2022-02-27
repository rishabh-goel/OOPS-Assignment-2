package com.rishabh.hw2

import java.lang
import scala.collection.{immutable, mutable}
import scala.collection.mutable.*

object Computation:

  import SetExp.*

  // Aliasing 'Any' to avoid hardcoding of Variable types
  type BasicType = Any

  // Map to store the macros
  val macroMap: Map[BasicType, SetExp] = Map()

  // Map to store classes, variables and scopes
  val scopeMap: Map[BasicType, BasicType] = Map()

  // Map to identify which class has which object
  val objectMap: Map[BasicType, BasicType] = Map()

  // Map to store attributes an object can access
  val attrMap: Map[BasicType, BasicType] = Map()

  // Map to store class inheritance(child -> parent)
  val inheritanceMap: Map[BasicType, BasicType] = Map()

  // Map to store inheritance(outer -> inner)
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
    case ClassDef(className: String, expr: SetExp*) // Create Class Definition
    case InnerClass(outerClass: String, innerClass: String) // Define Outer and Inner classes
    case Field(name: String, expr: SetExp*) // Create field of class
    case Constructor(expr: SetExp) // Create constructor of class
    case NewObject(className: String, expr: SetExp, params: Params, values: ListBuffer[SetExp], parentObj: String*) // Create object of class
    case InvokeObject(className: SetExp, objectName: SetExp, attrName: SetExp, actualParams: SetExp*) // Access class attribute using an object
    case CreateMethod(methodName: String, params: Params, methodType: SetExp*) // Create method of a class
    case InvokeMethod(methodName: ListBuffer[SetExp], formalparam: SetExp, actualparam: Any) // Invoke method of a class
    case Public(param: SetExp) // Create Public access modifier
    case Private(param: SetExp) // Create Private access modifier
    case Protected(param: SetExp) // Create Protected access modifier
    case Params(parameters: String*) // Stores formal parameters for Constructors and Methods

    // Infix method to provide inheritance
    def Extends(superClass: SetExp) = {
      // Get elements of parent class
      val parent = superClass.eval().asInstanceOf[Map[BasicType, BasicType]].clone()

      // Get elements of child class
      val child = this.eval().asInstanceOf[Map[BasicType, BasicType]].clone()

      // Get name of child class
      val childName = scopeMap.find(_._2 == child).map(_._1) match {
        case Some(m) => m.asInstanceOf[String]
        case None => throw new Exception("Child class not found")
      }

      // Get name of parent class
      val parentName = scopeMap.find(_._2 == parent).map(_._1) match {
        case Some(m) => m.asInstanceOf[String]
        case None => throw new Exception("Parent class not found")
      }

      // Check if child class alreads inherits from some parent class
      if(inheritanceMap.contains(childName))
        throw new Exception("Cannot support multiple inheritance")
      else {

        // Get private access members of parent class
        val privateMembers = accessMap("private").asInstanceOf[Map[BasicType, BasicType]](parentName)

        // Create new Constructor, Field and Method maps by first merging members of parent and child class
        // Then removing private members from fields and methods as they are not inherited
        val constructorMap = parent("constructor").asInstanceOf[Map[BasicType, BasicType]].++(child("constructor").asInstanceOf[Map[BasicType, BasicType]])
        val fieldMap = parent("field").asInstanceOf[Map[BasicType, BasicType]].++(child("field").asInstanceOf[Map[BasicType, BasicType]])
        privateMembers.asInstanceOf[Map[BasicType, BasicType]].keySet.foreach(i => {
          fieldMap -= i
        })

        val methodMap = parent("method").asInstanceOf[Map[BasicType, BasicType]].++(child("method").asInstanceOf[Map[BasicType, BasicType]])
        privateMembers.asInstanceOf[Map[BasicType, BasicType]].keySet.foreach(i => {
          methodMap -= i
        })

        // Get public access members of parent and child class and create a new map for child class
        val publicParentMap = accessMap("public").asInstanceOf[Map[BasicType, BasicType]](parentName).asInstanceOf[Map[BasicType, BasicType]]
        val publicChildMap = accessMap("public").asInstanceOf[Map[BasicType, BasicType]](childName).asInstanceOf[Map[BasicType, BasicType]]
        val newpublicChildMap = publicChildMap.++(publicParentMap)

        // Get protected access members of parent and child class and create a new map for child class
        val protectedParentMap = accessMap("protected").asInstanceOf[Map[BasicType, BasicType]](parentName).asInstanceOf[Map[BasicType, BasicType]]
        val protectedChildMap = accessMap("protected").asInstanceOf[Map[BasicType, BasicType]](childName).asInstanceOf[Map[BasicType, BasicType]]
        val newprotectedChildMap = protectedChildMap.++(protectedParentMap)

        // Update the access modifiers for child class after inheriting from the parent class
        accessMap.update("public", accessMap("public").asInstanceOf[Map[BasicType, BasicType]] += (childName -> newpublicChildMap))
        accessMap.update("protected", accessMap("protected").asInstanceOf[Map[BasicType, BasicType]] += (childName -> newprotectedChildMap))

        // keep track of which class inherits which class to avoid multiple inheritance
        inheritanceMap += (childName -> parentName)

        // Map with updated values to be put again to the scopeMap
        val map: Map[BasicType, BasicType] = Map("field" -> fieldMap, "constructor" -> constructorMap, "method" -> methodMap)
        scopeMap += (childName -> map)
      }
    }

    // Method where the execution of our program begins
    def eval(scope: Map[BasicType, BasicType] = scopeMap, access: Map[BasicType, BasicType]*): BasicType =
      this match {

        case Value(i) => i

        case Variable(name) =>
          if(scope.contains(name))
            scope(name)
          else
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

          // Get the current scope
          val sc = scope.get(key)

          // If a scope exists then return that scope otherwise create a new one
          val currentScope = sc match {
            case Some(s) => s.asInstanceOf[Map[BasicType, BasicType]]

            case None => {
              val temp: Map[BasicType, BasicType] = Map()
              scope += (key -> temp)
              temp
            }
          }
          // Perform operations in the current scope
          op.eval(currentScope)


        case InnerClass(outerClass, innerClass) =>
          // Get elements of inner and outer class from the current scope
          val inner = scope(innerClass).asInstanceOf[Map[String, Any]]
          val outer = scope(outerClass).asInstanceOf[Map[String, Any]]

          // Allow only 1 nested class to get created
          if(nestedClassMap.contains(outer))
            throw new Exception("Cannot create multiple nested classes")

          // Update the map of outer class to include details of inner class
          outer += ("innerClass" -> Map(innerClass -> inner))
          nestedClassMap += (outerClass -> innerClass)

          // Remove the inner class from current scope as it has been moved to inside outer class
          scope.remove(innerClass)


        case ClassDef(className, expr*) =>

          // Create map to store fields, methods and constructors for a class
          val fieldMap: Map[String, Any] = Map()
          val constructorMap: Map[BasicType, BasicType] = Map()
          val methodMap: Map[String, ListBuffer[SetExp]] = Map()

          // If class is already created, return the elements of the class
          if(scope.contains(className))
            scope(className)
          else {

            // Create a map that will be used to store all elements of the class
            val map: Map[BasicType, BasicType] = Map("field" -> fieldMap, "constructor" -> constructorMap, "method" -> methodMap)

            // Create access modifier maps
            val publicClassMap: Map[BasicType, BasicType] = Map(className -> Map())
            val privateClassMap: Map[BasicType, BasicType] = Map(className -> Map())
            val protectedClassMap: Map[BasicType, BasicType] = Map(className -> Map())

            // Evaluate each expression inside class def with the appropriate scope
            expr.foreach(i => {
              i.eval(map, publicClassMap, privateClassMap, protectedClassMap)
            })

            // Update the maps with access modifiers and class definition
            scope += (className -> map)
            accessMap.update("public", accessMap("public").asInstanceOf[Map[BasicType, BasicType]] += (publicClassMap.head._1 -> publicClassMap.head._2))
            accessMap.update("private", accessMap("private").asInstanceOf[Map[BasicType, BasicType]] += (privateClassMap.head._1 -> privateClassMap.head._2))
            accessMap.update("protected", accessMap("protected").asInstanceOf[Map[BasicType, BasicType]] += (protectedClassMap.head._1 -> protectedClassMap.head._2))
            scope(className)
          }

        case Field(name, expr*) =>
          val fieldMap = scope("field").asInstanceOf[Map[BasicType, BasicType]]

          // Creating a field map in which all the values will be added
          // If a field comes only with name, we assign null value to it. Otherwise the 1st value of expr is assigned to it
          if(expr.length == 0)
            fieldMap += (name -> null)
            Map(name -> null)
          else
            fieldMap += (name -> expr(0).eval(scope))
            Map(name -> expr(0).eval(scope))

        case Constructor(expr) =>
          val constructorMap = scope("constructor").asInstanceOf[Map[BasicType, BasicType]]
          // Adding the evaluated expressions to the constructor map
          expr.eval(constructorMap)

        case Params(params*) =>
          // Return the list of formal parameters for Methods and Constructors
          params


        case CreateMethod(methodName, params, expr*) =>
          // Create a method map scope in which we add the formal parameters and the method definition statements
          val methodMap = scope("method").asInstanceOf[Map[BasicType, ListBuffer[SetExp]]]
          val list = new ListBuffer[SetExp]
          list.addOne(params)

          expr.foreach(i => list.addOne(i))
          methodMap += (methodName -> list)
          Map(methodName -> list)


        case InvokeMethod(method, formalparam, actualparam) =>
          // Get the list of formal and actual parameters
          val fp: immutable.ArraySeq[String] = formalparam.asInstanceOf[SetExp].eval(scope).asInstanceOf[immutable.ArraySeq[String]]
          val ap: immutable.ArraySeq[SetExp] = actualparam.asInstanceOf[immutable.ArraySeq[SetExp]]

          // Check if the length of formal and actual parameters is same
          // If they are same, perform the next steps. Otherwise throw an exception
          if(fp.length.equals(ap.length)) {

            // Zip formal parameters with actual parameters
            val list = fp.zip(ap)
            list.foreach(i => {
              scope += (i._1 -> i._2.eval(scope))
            })

            // Evaluate all the statements inside the function definition except the last one
            method.zipWithIndex.foreach(i => {
              if(i._2 != method.length-1) {
                i._1.asInstanceOf[SetExp].eval(scope)
              }
            })

            // Evaluate the last term of the function definition and return the result
            method(method.length-1).asInstanceOf[SetExp].eval(scope)
          }
          else {
            throw new Exception("Insufficient parameters")
          }


        case NewObject(className, expr, params, values, parentObj*) =>
          val parameters = params.eval(scope).asInstanceOf[immutable.ArraySeq[String]]

          // Check if formal parameters and actual parameters for constructors are equal
          if(parameters.length != values.length) {
            throw new Exception("Parameter list doesn't have equal number of initializing values")
          }
          else{

            // Create a list of values being passed to the constructor
            val valueList: ListBuffer[BasicType] = ListBuffer()
            values.foreach(i => {
              valueList.addOne(i.eval(scope))
            })

            // Zip the formal parameters with actual parameters
            val result = parameters zip valueList

            val initializationMap: Map[String, Any] = Map()

            // Initialization map will have the new values for the fields for a particular object
            result.foreach(i => {
              initializationMap += (i._1 -> i._2)
            })

            // Check if already an object exists for the class
            if(objectMap.contains(className)){
              // Check if any value for the object of outer class provided or not
              // If creating for outer class, parentObj would not be provided. Otherwise name of outer class object should be provided
              if(parentObj.length == 0) {
                // Get the list of objects for the current class
                val list: ListBuffer[Any] = objectMap(className).asInstanceOf[ListBuffer[Any]]
                list += expr.eval()

                // Update the objectMap to include the current object to the class list
                objectMap += (className -> list)
                // Update the attrMap to include the current object and the list of elements it can access
                attrMap += (expr.eval() -> scope(className).asInstanceOf[Map[String, Any]].clone())

                // If we have an inner class, the object should not access that. Hence, we remove the innerClass entry from the map
                val objectAttr = attrMap(expr.eval(scope)).asInstanceOf[Map[String, Any]]
                objectAttr -= "innerClass"
                attrMap += (expr.eval(scope) -> objectAttr)
              }
              else {
                // Parent object was provided for the current object
                // Check if the object's class is an inner class or not
                if(nestedClassMap.exists(_._2 == className)){
                  val list: ListBuffer[Any] = objectMap(className).asInstanceOf[ListBuffer[Any]]
                  list += expr.eval()
                  objectMap += (className -> list)

                  // Get the name of the outer class
                  val outerClassName = nestedClassMap.find(_._2 == className).map(_._1) match {
                    case Some(m) => m
                    case None => throw new Exception("Outer class not found")
                  }

                  // Get the list of objects created for the outer class
                  val outerClassObjects = objectMap(outerClassName).asInstanceOf[ListBuffer[Any]]

                  // Check if the parent object exists or not
                  if(outerClassObjects.contains(parentObj(0))) {
                    val outerClassAttr = scope(outerClassName).asInstanceOf[Map[String, Any]]
                    // Associate the members of the inner class with the current object
                    attrMap += (expr.eval() -> outerClassAttr("innerClass").asInstanceOf[mutable.Map[String, Any]](className))
                  }
                  else {
                    // Throw an exception because outer class object doesn't exist
                    throw new Exception("Outer class object doesn't exist")
                  }
                }
                else {
                  // Throw an exception because the current object's class is not an inner class
                  throw new Exception(className + " is not an inner class. Outer class object not needed")
                }
              }

              // Once the object has been created, the constructor for the object should be called that will initialize the fields
              // Once the Constructor statements have been executed, remove the fields from constructor map and add to the fields map
              val objectAttr = attrMap(expr.eval(scope)).asInstanceOf[Map[String, Any]].clone()
              val objectConstructor = objectAttr("constructor").asInstanceOf[Map[String, Any]].clone()
              val objectField = objectAttr("field").asInstanceOf[Map[String, Any]].clone()
              val result = objectConstructor.++(objectField)

              initializationMap.foreach(i => {
                if(!result.contains(i._1))
                  throw new Exception("Class doesn't have field " + i._1)
              })

              // Update the attrMap to hold the new values for fields as they have been initialized after calling the constructor
              val finalMap = result.++(initializationMap)
              attrMap.update(expr.eval(scope), objectAttr += ("field" -> finalMap))
              attrMap.update(expr.eval(scope), objectAttr -= "constructor")
            }
            else {
              // Creating the object of a class for the 1st time
              // Check if any value for the object of outer class provided or not
              // If creating for outer class, parentObj would not be provided. Otherwise name of outer class object should be provided
              if(parentObj.length == 0) {
                // Create the list of objects for the current class
                val list: ListBuffer[Any] = ListBuffer()
                list += expr.eval(scope)
                // Update the objectMap to include the current object to the class list
                objectMap += (className -> list)
                // Update the attrMap to include the current object and the list of elements it can access
                attrMap += (expr.eval(scope) -> scope(className).asInstanceOf[Map[String, Any]].clone())

                // If we have an inner class, the object should not access that. Hence, we remove the innerClass entry from the map
                val objectAttr = attrMap(expr.eval(scope)).asInstanceOf[Map[String, Any]]
                objectAttr -= "innerClass"
                attrMap += (expr.eval(scope) -> objectAttr)
              }
              else {
                // Parent object was provided for the current object
                // Check if the object's class is an inner class or not
                if(nestedClassMap.exists(_._2 == className)){

                  // Get the name of the outer class
                  val outerClassName = nestedClassMap.find(_._2 == className).map(_._1) match {
                    case Some(m) => m
                    case None => throw new Exception("Outer class not found")
                  }

                  // Get the list of objects created for the outer class
                  val outerClassObjects = objectMap(outerClassName).asInstanceOf[ListBuffer[Any]]

                  // Check if the parent object exists or not
                  if(outerClassObjects.contains(parentObj(0))) {
                    val list: ListBuffer[Any] = ListBuffer()
                    list += expr.eval()
                    objectMap += (className -> list)
                    val outerClassAttr = scope(outerClassName).asInstanceOf[Map[String, Any]]
                    // Associate the members of the inner class with the current object
                    attrMap += (expr.eval() -> outerClassAttr("innerClass").asInstanceOf[mutable.Map[String, Any]](className))
                  }
                  else {
                    // Throw an exception because outer class object doesn't exist
                    throw new Exception("Parent object doesn't exist")
                  }
                }
                else {
                  // Throw an exception because the current object's class is not an inner class
                  throw new Exception(className + " is not an inner class. Parent object not needed")
                }
              }

              // Once the object has been created, the constructor for the object should be called that will initialize the fields
              // Once the Constructor statements have been executed, remove the fields from constructor map and add to the fields map
              val objectAttr = attrMap(expr.eval(scope)).asInstanceOf[Map[String, Any]].clone()
              val objectConstructor = objectAttr("constructor").asInstanceOf[Map[String, Any]].clone()
              val objectField = objectAttr("field").asInstanceOf[Map[String, Any]].clone()
              val result = objectConstructor.++(objectField)

              initializationMap.foreach(i => {
                if(!result.contains(i._1))
                  throw new Exception("Class doesn't have field " + i._1)
              })

              // Update the attrMap to hold the new values for fields as they have been initialized after calling the constructor
              val finalMap = result.++(initializationMap)
              attrMap.update(expr.eval(scope), objectAttr += ("field" -> finalMap))
              attrMap.update(expr.eval(scope), objectAttr -= "constructor")
            }
          }

        case InvokeObject(className, objectName, attrName, actualParams*) =>
          // Check if we have created the class for which we want to use an object
          if(!objectMap.contains(className.eval(scope)))
            throw new Exception("Class "+ className.eval(scope) + " does not have any object")
          else {
            // Get the list of objects created for the current class
            val list: ListBuffer[BasicType] = objectMap(className.eval(scope)).asInstanceOf[ListBuffer[BasicType]]

            // Check if we have created the object for which we want to invoke a method or get a field
            if(!list.contains(objectName.eval(scope)))
              throw new Exception("Object "+ objectName.eval(scope) + " does not exist")
            else {
              // Get the elements that can be accessed by the object from attrMap
              val map: Map[BasicType, BasicType] = attrMap(objectName.eval(scope)).asInstanceOf[Map[BasicType, BasicType]]

              // Check if any actual parameters provided for the method
              // If they are not provided, we want to access a variable of the class
              if(actualParams.length == 0){
                val fieldMap = map("field").asInstanceOf[Map[BasicType, BasicType]]
                fieldMap(attrName.eval(scope))
              }
              else{
                // Get the list of methods the object can access
                val methodMap = map("method").asInstanceOf[Map[BasicType, BasicType]]
                val method = methodMap(attrName.eval(scope)).asInstanceOf[ListBuffer[SetExp]]
                // Fetch the formal parameters
                val formalParams = method(0)
                val tempMap: Map[Any, Any] = Map()
                // Invoke the method by passing the method definition, formal parameters and actual parameters
                InvokeMethod(method.drop(1), formalParams, actualParams).eval(tempMap)
              }
            }
          }

        case Public(expr) =>
          // From eval(), we get the map at 0th index to store members with public access
          val publicMap = access(0).asInstanceOf[Map[BasicType, BasicType]]
          val result = expr.eval(scope).asInstanceOf[Map[BasicType, BasicType]]
          publicMap.update(publicMap.head._1, publicMap(publicMap.head._1).asInstanceOf[Map[BasicType, BasicType]] += (result.head._1 -> result.head._2))


        case Private(expr) =>
          // From eval(), we get the map at 1st index to store members with private access
          val privateMap = access(1).asInstanceOf[Map[BasicType, BasicType]]
          val result = expr.eval(scope).asInstanceOf[Map[BasicType, BasicType]]
          privateMap.update(privateMap.head._1, privateMap(privateMap.head._1).asInstanceOf[Map[BasicType, BasicType]] += (result.head._1 -> result.head._2))


        case Protected(expr) =>
          // From eval(), we get the map at 2nd index to store members with protected access
          val protectedMap = access(2).asInstanceOf[Map[BasicType, BasicType]]
          val result = expr.eval(scope).asInstanceOf[Map[BasicType, BasicType]]
          protectedMap.update(protectedMap.head._1, protectedMap(protectedMap.head._1).asInstanceOf[Map[BasicType, BasicType]] += (result.head._1 -> result.head._2))

      }

  @main def runArithExp: Unit =
    import SetExp.*
