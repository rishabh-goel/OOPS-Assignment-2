import com.rishabh.hw2.Computation.{BasicType, accessMap, attrMap, inheritanceMap, macroMap, nestedClassMap, objectMap, scopeMap}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import com.rishabh.hw2.Computation.SetExp.*

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map, Set}

class ComputationTest extends AnyFlatSpec with Matchers{

  // create "MyClass" class
  ClassDef("MyClass", Private(Field("f1")), Constructor(Assign("f1", Value(5))), Public(CreateMethod("m1", Params("a", "b"), Assign("c", Union(Variable("a"), Variable("b"))), Variable("c")))).eval()

  // create "MyInner" class
  ClassDef("MyInner", Public(Field("e1")), Constructor(Assign("e1", Value(100))), Private(CreateMethod("m1", Params("a", "b"), Assign("c", Intersect(Variable("a"), Variable("b"))), Variable("c")))).eval()

  // "MyInner" is inner class of "MyClass"
  InnerClass("MyClass", "MyInner").eval()

  // Create object of MyClass with constructor value f1->50
  NewObject("MyClass", Variable("obj1"), Params("f1"), ListBuffer(Value(50))).eval()

  // Create object of MyClass with constructor value f1->15
  NewObject("MyClass", Variable("obj2"), Params("f1"), ListBuffer(Value(15))).eval()

  // Create object of MyInner with constructor value e1->0
  NewObject("MyInner", Variable("innerObj1"), Params("e1"), ListBuffer(Value(0)), "obj1").eval()

  // Create object of MyInner with constructor value e1->20
  NewObject("MyInner", Variable("innerObj2"), Params("e1"), ListBuffer(Value(20)), "obj1").eval()

  // create "DerivedClass" class
  ClassDef("DerivedClass", Public(Field("d1")), Private(Field("d2", Value(2))), Protected(Field("d3", Value(5))), Protected(Field("d4")), Constructor(Assign("d1", Value(10)))).eval()

  // DerivedClass extends attributes of MyClass
  ClassDef("DerivedClass") Extends ClassDef("MyClass")

  // Create object of MyInner with constructor value d1->20
  NewObject("DerivedClass", Variable("derivedObj"), Params("d1"), ListBuffer(Value(20))).eval()

  // Invoke method m1 of MyClass using obj1 to perform Union of 2 sets
  Assign("result1", InvokeObject(Value("MyClass"), Value("obj1"), Value("m1"), Value(Set(1,2)), Value(Set(3,4)))).eval()

  // Invoke method m1 of MyInner using innerObj1 to perform Intersection of 2 sets
  Assign("result2", InvokeObject(Value("MyInner"), Value("innerObj1"), Value("m1"), Value(Set(1,2)), Value(Set(1,4)))).eval()

  // Trying to catch exception thrown on creating MyInner object as outer class object(obj3) is not created
  val caught = intercept[Exception] {
    NewObject("MyInner", Variable("innerObj3"), Params("e1"), ListBuffer(Value(20)), "obj3").eval()
  }

  // Test 1
  it should "check if outer and inner classes are created" in {
    // check if Outer class has been created
    scopeMap("MyClass") != null

    // check if inner class has been created inside outer class
    val innerClassMap = scopeMap("MyClass").asInstanceOf[Map[BasicType, BasicType]]("innerClass")
    innerClassMap.asInstanceOf[Map[BasicType, BasicType]]("MyInner") != null
  }

  // Test 2
  it should "check if we are able to implement class inheritance with required accesses" in {

    // check if public method of parent class is inherited or not
    val derivedMethodMap = scopeMap("DerivedClass").asInstanceOf[Map[String, Any]]("method").asInstanceOf[Map[String, Any]]
    assert(derivedMethodMap.contains("m1") === true)

    // check if private field of parent class is inherited or not
    val derivedFieldMap = scopeMap("DerivedClass").asInstanceOf[Map[String, Any]]("field").asInstanceOf[Map[String, Any]]
    assert(derivedFieldMap.contains("f1") === false)
  }

  // Test 3
  it should "check if we can create an object of class with constructor parameters passed" in {

    // 2 different of objects of same class have different values of same field assigned through constructor
    val fieldMap1 = attrMap("obj1").asInstanceOf[Map[BasicType, BasicType]]("field").asInstanceOf[Map[BasicType, BasicType]]
    val fieldMap2 = attrMap("obj2").asInstanceOf[Map[BasicType, BasicType]]("field").asInstanceOf[Map[BasicType, BasicType]]
    assert(fieldMap1("f1") === 50)
    assert(fieldMap2("f1") === 15)
  }

  // Test 4
  it should "check if we are able to invoke a method through an object" in {

    // evalue the result of invoking a method using object of respective classes
    assert(Variable("result1").eval() === mutable.HashSet(1, 2, 3, 4))
    assert(Variable("result2").eval() === mutable.HashSet(1))
  }

  // Test 5
  it should "check that an exception is thrown" in {
    //inner class object should be created only after outer class object has been created
    assert(caught.getMessage === "Outer class object doesn't exist")
  }
}
