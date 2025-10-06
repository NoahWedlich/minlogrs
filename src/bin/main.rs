#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestObject {}

impl Test for TestObject {
    fn example_method_2(&self, x: i32, y: i32) -> i32 {
        x + y
    }
}

lib::wrapper_enum!{
    @default { TestObject }
    pub trait Test: Sized {
        fn example_method_1(&Self, x: i32) -> i32 {
            x + 1
        }
        
        fn example_method_2(&Self, x: i32, y: i32) -> i32
    }
    
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum MyEnum {
        Variant1(TestObject),
        Variant2(),
    }
}

fn main() {
    let obj1 = MyEnum::Variant1(TestObject{});
    let obj2 = MyEnum::Variant2(TestObject{});
    
    println!("obj1.method1(5) = {}", obj1.example_method_1(5)); // Should print 6
    println!("obj2.method1(10) = {}", obj2.example_method_1(10)); // Should print 11
    
    println!("obj1.method2(3, 4) = {}", obj1.example_method_2(3, 4)); // Should print 7
    println!("obj2.method2(7, 8) = {}", obj2.example_method_2(7, 8)); // Should print 15
}