

/*
Example wrapper enum:

$crate::wrapper_enum! {
    @default { DefaultType }
    pub trait BodyTrait: Debug {
        pub fn method1(params...) -> ReturnType {
            // default implementation
        }
        
        pub fn method2(params...) -> ReturnType
    }

    @pass_through {
        // Any trait methods here are also added to BodyTrait
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum MyEnum {
        Variant1(||name1|| Type1),
        Variant2(|name2>|),
        Variant3()
        ...
    }
    
    @forward {
        pub fn method_to_forward(params...) -> ReturnType {
            @pre { ... } // optional code before matching
            @match { expression_to_match }
            @method { static_method_to_call(params...) }
            @post { |$result| ... } // optional code after matching
        }
    }
    
    impl SomeTrait {
        fn some_method_1(args...) -> ReturnType;
        fn some_method_2(args...) -> ReturnType;
        ...
    }
    
    impl AnotherTrait {
        fn another_method_1(args...) -> ReturnType;
        ...
    }
} Expands to: {
    pub trait BodyTrait: Debug + SomeTrait + AnotherTrait {
        fn method1(params...) -> ReturnType;
        fn method2(params...) -> ReturnType;
        ...
        
        // Any methods from @pass_through { ... } are also included
    }
    
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum MyEnum
    where 
        Type1: BodyTrait,
        // ... for each explicit field type
    {
        Variant1(Type1),
        Variant2(DefaultType),  // Uses default if no type specified
    }
    
    impl MyEnum {
        pub fn variant_count() -> usize {
            2  // Computed at compile time
        }
        
        // For each method in BodyTrait, generate a forwarding method:
        fn method1(params...) -> ReturnType {
            match self {
                MyEnum::Variant1(inner) => inner.method1(params...),
                MyEnum::Variant2(inner) => inner.method1(params...),
                // ...
            }
        }
        
        // For each variant with a body name |name| or ||name||, generate a test:
        pub fn is_name1(&self) -> bool {
            matches!(self, MyEnum::Variant1(_))
        }

        // For each variant with a body name ||name||, generate an accessor:
        pub fn to_name1(&self) -> Option<&Type1> {
            match self {
                MyEnum::Variant1(inner) => Some(inner),
                _ => None,
            }
        }
        
        // For each method in @forward { ... }, generate a forwarding method:
        pub fn method_to_forward(params...) -> ReturnType {
            // optional code before matching
            ...
            let result = match expression_to_match {
                MyEnum::Variant1(inner) => Type1::static_method_to_call(inner, params...),
                MyEnum::Variant2(inner) => DefaultType::static_method_to_call(inner, params...),
                ...
            };
            // optional code after matching
            ...
            result
        }
    }
    
    impl SomeTrait for MyEnum {
        // For each method specified in SomeTrait, generate a forwarding method:
        fn some_method_1(args...) -> ReturnType {
            match self {
                MyEnum::Variant1(inner) => inner.some_method_1(args...),
                MyEnum::Variant2(inner) => inner.some_method_1(args...),
                // ...
            }
        }
        
        // ...
    }
    
    // Similarly for AnotherTrait
    impl AnotherTrait for MyEnum {
        ...
    }
}
*/

#[macro_export]
macro_rules! wrapper_enum {
    (
        $( @default { $default_type:ty } )?
        $body_trait_vis:vis trait $body_trait:ident $(: $( $trait_bound:ident ),* )? {
            $(
                $method_vis:vis fn $method_name:ident
                    ($self_decl:ty $( ,$param_name:ident : $param_type:ty )* $(,)? ) $( -> $return_type:ty )? $({
                        $( $default_impl: tt )*
                    })?
            )*
        }
        
        $(
            @pass_through {
                $( $pass_through_impl:tt )*
            }
        )?
        
        $(#[$enum_derives:meta])*
        $enum_vis:vis enum $enum_name:ident {
            $(
                $variant_name:ident ( $( |$variant_test:ident| )? $(||$variant_access:ident|| )? $($variant_type:ty )? )
            ),* $(,)?
        }
        
        $(
            @forward {
                $(
                    $forward_method_vis:vis fn $forward_method_name:ident
                        ( $( $forward_param_name:ident : $forward_param_type:ty ),* $(,)? ) $( -> $forward_return_type:ty )? {
                            $( @pre { $( $pre_code:tt )* } )?
                            @match { $match_expr:expr }
                            @method { $static_method_name:ident ( $( $static_param: expr ),* $(,)? ) }
                            $( @post { |$result:ident| $( $post_code:tt )* } )?
                        }
                )*
            }
        )?
        
        $(
            impl $impl_trait:ident $(< $( $impl_type:ty ),* >)? {
                $(
                    fn $impl_method_name:ident
                        ($self_decl_impl:ty $( ,$impl_param_name:ident : $impl_param_type:ty )* $(,)? ) $( -> $impl_return_type:ty )?;
                )*
            }
        )*
    ) => {
        // Define the body trait
        $body_trait_vis trait $body_trait: $( $( $trait_bound + )* )? $( $impl_trait $(< $( $impl_type ),* >)? + )* {
            $(
                $crate::wrapper_enum!{@trait_method fn $method_name
                    ($self_decl $( ,$param_name : $param_type )* ) $( -> $return_type )? 
                    | { $( $crate::wrapper_enum!{@default_impl_body $( $default_impl )* } )? } }
            )*
            
            $(
                $(
                    $pass_through_impl
                )*
            )?
        }
        
        // Define the enum with appropriate where clauses
        $crate::wrapper_enum!{@gen_enum $(#[$enum_derives])* $enum_vis $enum_name $body_trait $( $default_type )? 
            | { $( $variant_name ( $( $variant_type )? ) )* } | {} | {} }
            
        // Implement the variant count and forwarding methods
        impl $enum_name {
            // Count the number of variants
            pub fn variant_count() -> usize {
                $crate::wrapper_enum!(@count $( $variant_name )* )
            }
            
            // Generate forwarding methods
            $crate::wrapper_enum!{@gen_forwards $enum_name
                | { $( $method_vis $method_name($self_decl $( ,$param_name : $param_type )* ) $( -> $return_type )? )|* |}
                | { $( $variant_name ( $( $variant_type )? ) )* }
                | { } }
                
            // Generate test methods for each variant with a body name
            $crate::wrapper_enum!{@gen_test $enum_name
                | { $( $variant_name ( $( |$variant_test| )? $( ||$variant_access|| )? ) )* }
                | { } }
                
            // Generate accessors for each variant with a body name
            $crate::wrapper_enum!{@gen_accessor $enum_name $( $default_type )?
                | { $( $variant_name ( $( ||$variant_access|| )? $( $variant_type )? ) )* }
                | { } }
                
            // Generate custom forwarding methods$crate::wrapper_enum!{@gen_custom_forwards $enum_name $( $default_type )?
            $crate::wrapper_enum!{@gen_custom_forwards $enum_name $( $default_type )?
                | { $( $( $forward_method_vis fn $forward_method_name
                    ( $( $forward_param_name : $forward_param_type ),* ) $( -> $forward_return_type )? {
                        $( @pre { $( $pre_code )* } )?
                        @match { $match_expr }
                        @method { $static_method_name ( $( $static_param ),* ) }
                        $( @post { |$result| $( $post_code )* } )?
                    } )* )? }
                | { $( $variant_name ( $( $variant_type )? ) )* }
                | { } }
        }
        
        // Implement additional traits for the enum
        $crate::wrapper_enum!{@gen_traits $enum_name
            | { $( $impl_trait $(< $( $impl_type ),* >)? ),* }
            | { $( [$( $impl_method_name($self_decl_impl $( ,$impl_param_name : $impl_param_type )* ) $( -> $impl_return_type )? )|*] )|* | }
            | { $( $variant_name ( $( $variant_type )? ) )* }
            | { } }
    };
    
    (@trait_method fn $method_name:ident
        ($self_decl:ty $( ,$param_name:ident : $param_type:ty )* $(,)? ) $( -> $return_type:ty )? | { } ) => {
            
            fn $method_name (self: $self_decl, $( $param_name : $param_type ),* ) $( -> $return_type )?;
    };
    
    (@trait_method fn $method_name:ident
        ($self_decl:ty $( ,$param_name:ident : $param_type:ty )* $(,)? ) $( -> $return_type:ty )? | { $( $default_impl:tt )* } ) => {
            
            fn $method_name (self: $self_decl, $( $param_name : $param_type ),* ) $( -> $return_type )? {
                $( $default_impl )*
            }
    };
    
    (@default_impl_body $( $default_impl:tt )* ) => {
        $( $default_impl )*
    };
    
    (@body_type $variant_type:ty, $( $default_type:ty )? ) => {
        $variant_type
    };
    
    (@body_type , $default_type:ty) => {
        $default_type
    };
    
    (@gen_enum $(#[$enum_derives:meta])* $enum_vis:vis $enum_name:ident $body_trait:ident $( $default_type:ty )?
        | { $variant_name:ident ( $( $type:ty )? ) $( $rest:ident ( $( $rest_type:ty )? ) )* }
        | { $( $bounds:tt )* }
        | { $( $variants:tt )* }) => {
            
            $crate::wrapper_enum!{@gen_enum $(#[$enum_derives])* $enum_vis $enum_name $body_trait $( $default_type )?
                | { $( $rest ( $( $rest_type )? ) )* }
                | { $( $bounds )* $crate::wrapper_enum!{@body_type $( $type )?, $( $default_type )?}: $body_trait, }
                | { $( $variants )* $variant_name( $crate::wrapper_enum!{@body_type $( $type )?, $( $default_type )?} ), } }
    };
    
    (@gen_enum $(#[$enum_derives:meta])* $enum_vis:vis $enum_name:ident $body_trait:ident $( $default_type:ty )?
        | {}
        | { $( $bounds:tt )* }
        | { $( $variants:tt )* }) => {
            
            $(#[$enum_derives])*
            $enum_vis enum $enum_name
            where
                $( $bounds )*
            {
                $( $variants )*
            }
    };
    
    (@gen_traits $enum_name:ident
        | { $trait_name:ident $(< $($trait_param:tt),* >)? $( ,$rest_trait:ident $(< $($rest_type_param:tt),* >)? )* }
        | { [$( $method_name:ident($self_decl:ty $( ,$param_name:ident : $param_type:ty )* ) $( -> $return_type:ty )? )|*]
            | $( [$( $rest_method_name:ident($self_decl_rest:ty $( ,$rest_param:ident : $rest_param_type:ty )* ) $( -> $rest_return:ty )? )|*] )|* $(|)? }
        | { $( $variant_name:ident ( $( $variant_type:ty )? ) )* }
        | { $( $traits:tt )* }) => {
            
            $crate::wrapper_enum!{@gen_traits $enum_name
                | { $( $rest_trait $(< $($rest_type_param),* >)? ),* }
                | { $( [$( $rest_method_name($self_decl_rest $( ,$rest_param : $rest_param_type )* ) $( -> $rest_return )? )|*] )|* | }
                | { $( $variant_name ( $( $variant_type )? ) )* }
                | {
                    $( $traits )*
                    
                    impl $trait_name $(< $($trait_param),* >)? for $enum_name {
                        $crate::wrapper_enum!{@gen_forwards $enum_name
                            | { $( $method_name($self_decl $( ,$param_name : $param_type )* ) $( -> $return_type )? )|* | }
                            | { $( $variant_name ( $( $variant_type )? ) )* }
                            | { } }
                    }
                } }
    };
    
    (@gen_traits $enum_name:ident
        | { $(|)?}
        | { $(|)?}
        | { $( $variant_name:ident ( $( $variant_type:ty )? ) )* }
        | { $( $traits:tt )* }) => {
            
            $( $traits )*
    };
    
    (@gen_forwards $enum_name:ident
        | { $method_vis:vis $method_name:ident($self_decl:ty $( ,$param_name:ident : $param_type:ty )* ) $( -> $return_type:ty )?
            | $( $rest_method_vis:vis $rest_method:ident($self_decl_rest:ty $( ,$rest_param:ident : $rest_param_type:ty )* ) $( -> $rest_return:ty )? )|* $(|)? }
        | { $( $variant_name:ident ( $( $variant_type:ty )? ) )* }
        | { $( $methods:tt )* }) => {
            
            $crate::wrapper_enum!{@gen_forwards $enum_name
                | { $( $rest_method_vis $rest_method($self_decl_rest $( ,$rest_param : $rest_param_type )* ) $( -> $rest_return )? )|* | }
                | { $( $variant_name ( $( $variant_type )? ) )* }
                | {
                    $( $methods )*
                    $crate::wrapper_enum!{@gen_forward $enum_name $method_vis $method_name($self_decl $( ,$param_name : $param_type )* ) $( -> $return_type )?
                        | { $( $variant_name ( $( $variant_type )? ) )* }
                        | { } }
                } }
    };
    
    (@gen_forwards $enum_name:ident
        | { $(|)? }
        | { $( $variant_name:ident ( $( $variant_type:ty )? ) )* }
        | { $( $methods:tt )* }) => {
            
            $( $methods )*
    };
    
    (@gen_forward $enum_name:ident $method_vis:vis $method_name:ident($self_decl:ty $( ,$param_name:ident : $param_type:ty )* ) $( -> $return_type:ty )?
        | { $variant_name:ident ( $( $variant_type:ty )? ) $( $rest:ident ( $( $rest_type:ty )? ) )* }
        | { $( $matches:tt )* }) => {
            
            $crate::wrapper_enum!{@gen_forward $enum_name $method_vis $method_name($self_decl $( ,$param_name : $param_type )* ) $( -> $return_type )?
                | { $( $rest ( $( $rest_type )? ) )* }
                | { $( $matches )* $enum_name::$variant_name(inner) => inner.$method_name( $( $param_name ),* ), } }
    };
    
    (@gen_forward $enum_name:ident $method_vis:vis $method_name:ident($self_decl:ty $( ,$param_name:ident : $param_type:ty )* ) $( -> $return_type:ty )?
        | {}
        | { $( $matches:tt )* }) => {
            
            $method_vis fn $method_name(self: $self_decl, $( $param_name : $param_type ),* ) $( -> $return_type )? {
                match self {
                    $( $matches )*
                }
            }
    };
    
    (@gen_test $enum_name:ident
        | { $variant_name:ident ( |$variant_test:ident| $( ||$variant_access:ident|| )? )
            $( $rest:ident ( $( |$rest_test:ident| )? $( ||$rest_access:ident|| )? ) )* }
        | { $( $tests:tt )* }) => {
            
            $crate::wrapper_enum!{@gen_test $enum_name
                | { $( $rest ( $( |$rest_test| )? $( ||$rest_access|| )? ) )* }
                | { $( $tests )*
                    paste::paste! {
                        pub fn [< is_ $variant_test >](&self) -> bool {
                            matches!(self, $enum_name::$variant_name(_))
                        }
                    }
                } }
    };
    
    (@gen_test $enum_name:ident
        | { $variant_name:ident ( ||$variant_access:ident|| )
            $( $rest:ident ( $( |$rest_test:ident| )? $( ||$rest_access:ident|| )? ) )* }
        | { $( $tests:tt )* }) => {
            
            $crate::wrapper_enum!{@gen_test $enum_name
                | { $( $rest ( $( |$rest_test| )? $( ||$rest_access|| )? ) )* }
                | { $( $tests )*
                    paste::paste! {
                        pub fn [< is_ $variant_access >](&self) -> bool {
                            matches!(self, $enum_name::$variant_name(_))
                        }
                    }
                } }
    };
    
    (@gen_test $enum_name:ident
        | { $variant_name:ident ( ) $( $rest:ident ( $( |$rest_test:ident| )? $( ||$rest_access:ident|| )? ) )* }
        | { $( $tests:tt )* }) => {
            
            $crate::wrapper_enum!{@gen_test $enum_name
                | { $( $rest ( $( |$rest_test| )? $( ||$rest_access|| )? ) )* }
                | { $( $tests )* } }
    };
    
    (@gen_test $enum_name:ident
        | {}
        | { $( $tests:tt )* }) => {
            
            $( $tests )*
    };
    
    (@gen_accessor $enum_name:ident $( $default_type:ty )?
        | { $variant_name:ident ( ||$variant_access:ident|| $( $variant_type:ty )? ) $( $rest:ident ( $( ||$rest_body:ident|| )? $( $rest_type:ty )? ) )* }
        | { $( $accessors:tt )* }) => {
            
            $crate::wrapper_enum!{@gen_accessor $enum_name $( $default_type )?
                | { $( $rest ( $( ||$rest_body|| )? $( $rest_type )? ) )* }
                | { $( $accessors )*
                    paste::paste! {
                        pub fn [< to_ $variant_access >](&self) -> Option<&$crate::wrapper_enum!{@body_type $( $variant_type )?, $( $default_type )?}> {
                            match self {
                                $enum_name::$variant_name(inner) => Some(inner),
                                _ => None,
                            }
                        }
                    }
                } }
    };
    
    (@gen_accessor $enum_name:ident $( $default_type:ty )?
        | { $variant_name:ident ( $( $variant_type:ty )? ) $( $rest:ident ( $( ||$rest_body:ident|| )? $( $rest_type:ty )? ) )* }
        | { $( $accessors:tt )* }) => {
            
            $crate::wrapper_enum!{@gen_accessor $enum_name $( $default_type )?
                | { $( $rest ( $( ||$rest_body|| )? $( $rest_type )? ) )* }
                | { $( $accessors )* } }
    };
    
    (@gen_accessor $enum_name:ident $( $default_type:ty )?
        | {}
        | { $( $accessors:tt )* }) => {
            
            $( $accessors )*
    };
    
    (@gen_custom_forwards $enum_name:ident $( $default_type:ty )?
        | { $forward_method_vis:vis fn $forward_method_name:ident
            ( $( $forward_param_name:ident : $forward_param_type:ty ),* $(,)? ) $( -> $forward_return_type:ty )? {
                $( @pre { $( $pre_code:tt )* } )?
                @match { $match_expr:expr }
                @method { $static_method_name:ident ( $( $static_param: expr ),* $(,)? ) }
                $( @post { |$result:ident| $( $post_code:tt )* } )?
            } $( $rest:tt )* }
        | { $( $variant_name:ident ( $( $variant_type:ty )? ) )* }
        | { $( $methods:tt )* }) => {
            
            $crate::wrapper_enum!{@gen_custom_forwards $enum_name $( $default_type )?
                | { $( $rest )* }
                | { $( $variant_name ( $( $variant_type )? ) )* }
                | { $( $methods )*
                    $crate::wrapper_enum!{@gen_custom_forward $enum_name $( $default_type )?
                        | $forward_method_vis fn $forward_method_name
                            ( $( $forward_param_name : $forward_param_type ),* ) $( -> $forward_return_type )? {
                                $( @pre { $( $pre_code )* } )?
                                @match { $match_expr }
                                @method { $static_method_name ( $( $static_param ),* ) }
                                $( @post { |$result| $( $post_code )* } )?
                            }
                        | { $( $variant_name ( $( $variant_type )? ) )* }
                        | { } }
                } }
    };
    
    (@gen_custom_forwards $enum_name:ident $( $default_type:ty )?
        | {}
        | { $( $variant_name:ident ( $( $variant_type:ty )? ) )* }
        | { $( $methods:tt )* }) => {
            
            $( $methods )*
    };
    
    (@gen_custom_forward $enum_name:ident $( $default_type:ty )?
        | $forward_method_vis:vis fn $forward_method_name:ident
            ( $( $forward_param_name:ident : $forward_param_type:ty ),* $(,)? ) $( -> $forward_return_type:ty )? {
                $( @pre { $( $pre_code:tt )* } )?
                @match { $match_expr:expr }
                @method { $static_method_name:ident ( $( $static_param: expr ),* $(,)? ) }
                $( @post { |$result:ident| $( $post_code:tt )* } )?
            }
        | { $variant_name:ident ( $( $variant_type:ty )? )
            $( $rest_variants:ident ( $( ||$rest_body:ident|| )? $( $rest_type:ty )? ) )* }
        | { $( $matches:tt )* }) => {
            
            $crate::wrapper_enum!{@gen_custom_forward $enum_name $( $default_type )?
                | $forward_method_vis fn $forward_method_name
                    ( $( $forward_param_name : $forward_param_type ),* ) $( -> $forward_return_type )? {
                        $( @pre { $( $pre_code )* } )?
                        @match { $match_expr }
                        @method { $static_method_name ( $( $static_param ),* ) }
                        $( @post { |$result| $( $post_code )* } )?
                    }
                | { $( $rest_variants ( $( ||$rest_body|| )? $( $rest_type )? ) )* }
                | { $( $matches )* $enum_name::$variant_name(inner) =>
                    <$crate::wrapper_enum!{@body_type $( $variant_type )?, $( $default_type )?}>::$static_method_name(inner, $( $static_param ),*), } }
    };
    
    (@gen_custom_forward $enum_name:ident $( $default_type:ty )?
        | $forward_method_vis:vis fn $forward_method_name:ident
            ( $( $forward_param_name:ident : $forward_param_type:ty ),* $(,)? ) $( -> $forward_return_type:ty )? {
                $( @pre { $( $pre_code:tt )* } )?
                @match { $match_expr:expr }
                @method { $static_method_name:ident ( $( $static_param: expr ),* $(,)? ) }
                $( @post { |$result:ident| $( $post_code:tt )* } )?
            }
        | {}
        | { $( $matches:tt )* }) => {
            
            $forward_method_vis fn $forward_method_name( $( $forward_param_name : $forward_param_type ),* ) $( -> $forward_return_type )? {
                $( $( $pre_code )* )?
                let $crate::wrapper_enum!(@result_name $( $result )? | default) = match $match_expr {
                    $( $matches )*
                };
                $( $( $post_code )* )?
                $crate::wrapper_enum!(@result_name $( $result )? | default)
            }
    };
    
    (@result_name $result:ident | $( $default:ident )?) => { $result };

    (@result_name | $default:ident) => { $default };

    (@count ) => { 0usize };
    (@count $head:ident $( $tail:ident )* ) => { 1usize + $crate::wrapper_enum!(@count $( $tail )*) };
}