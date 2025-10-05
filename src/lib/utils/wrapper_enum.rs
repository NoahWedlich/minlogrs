
/*
Example wrapper enum:

lib::wrapper_enum! {
    @default { DefaultType }
    pub trait BodyTrait: Debug {
        pub fn method1(params...) -> ReturnType {
            // default implementation
        }
        
        pub fn method2(params...) -> ReturnType
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum MyEnum {
        Variant1(Type1),
        Variant2(),
        ...
    }
} Expands to: {
    pub trait BodyTrait: Debug {
        pub fn method1(params...) -> ReturnType;
        pub fn method2(params...) -> ReturnType;    
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
        pub fn method1(&self, params...) -> ReturnType {
            match self {
                MyEnum::Variant1(inner) => inner.method1(params...),
                MyEnum::Variant2(inner) => inner.method1(params...),
                // ...
            }
        }
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
                    (&self $( ,$param_name:ident : $param_type:ty )* $(,)? ) $( -> $return_type:ty )? $({
                        $( $default_impl: tt )*
                    })?
            )*
        }
        
        $(#[$enum_derives:meta])*
        $enum_vis:vis enum $enum_name:ident {
            $(
                $variant_name:ident ( $( $variant_type:ty )? )
            ),* $(,)?
        }
    ) => {
        // Define the body trait
        $body_trait_vis trait $body_trait: $( $( $trait_bound + )* )? {
            $(
                lib::wrapper_enum!{@trait_method $method_vis fn $method_name
                    (&self $( ,$param_name : $param_type )* ) $( -> $return_type )? 
                    | { $( lib::wrapper_enum!(@default_impl_body $( $default_impl )* ) )? } }
            )*
        }
        
        // Define the enum with appropriate where clauses
        lib::wrapper_enum!{@gen_enum $(#[$enum_derives])* $enum_vis $enum_name $body_trait $( $default_type )? 
            | { $( $variant_name ( $( $variant_type )? ) )* } | {} | {} }
            
        // Implement the body trait for each explicit variant type
        impl $enum_name {
            pub fn variant_count() -> usize {
                lib::wrapper_enum!(@count $( $variant_name )* )
            }
            
            // Generate forwarding methods
            lib::wrapper_enum!{@gen_forwards $enum_name
                | { $( $method_name(&self $( ,$param_name : $param_type )* ) $( -> $return_type )? )|* }
                | { $( $variant_name ( $( $variant_type )? ) )* }
                | { } }
        }
    };
    
    (@trait_method $method_vis:vis fn $method_name:ident
        (&self $( ,$param_name:ident : $param_type:ty )* $(,)? ) $( -> $return_type:ty )? | { } ) => {
            
            $method_vis fn $method_name (&self, $( $param_name : $param_type ),* ) $( -> $return_type )?;
    };
    
    (@trait_method $method_vis:vis fn $method_name:ident
        (&self $( ,$param_name:ident : $param_type:ty )* $(,)? ) $( -> $return_type:ty )? | { $( $default_impl:tt )* } ) => {
            
            $method_vis fn $method_name (&self, $( $param_name : $param_type ),* ) $( -> $return_type )? {
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
            
            lib::wrapper_enum!{@gen_enum $(#[$enum_derives])* $enum_vis $enum_name $body_trait $( $default_type )?
                | { $( $rest ( $( $rest_type )? ) )* }
                | { $( $bounds )* lib::wrapper_enum!{@body_type $( $type )?, $( $default_type )?}: $body_trait, }
                | { $( $variants )* $variant_name( lib::wrapper_enum!{@body_type $( $type )?, $( $default_type )?} ), } }
    };
    
    (@gen_enum $(#[$enum_derives:meta])* $enum_vis:vis $enum_name:ident $body_trait:ident $default_type:ty
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
    
    (@gen_forwards $enum_name:ident
        | { $method_name:ident(&self $( ,$param_name:ident : $param_type:ty )* ) $( -> $return_type:ty )?
            | $( $rest_method:ident(&self $( ,$rest_param:ident : $rest_param_type:ty )* ) $( -> $rest_return:ty )? )|* }
        | { $( $variant_name:ident ( $( $variant_type:ty )? ) )* }
        | { $( $methods:tt )* }) => {
            
            lib::wrapper_enum!{@gen_forwards $enum_name
                | { $( $rest_method(&self $( ,$rest_param : $rest_param_type )* ) $( -> $rest_return )? )|* | }
                | { $( $variant_name ( $( $variant_type )? ) )* }
                | {
                    $( $methods )*
                    lib::wrapper_enum!{@gen_forward $enum_name $method_name(&self $( ,$param_name : $param_type )* ) $( -> $return_type )?
                        | { $( $variant_name ( $( $variant_type )? ) )* }
                        | { } }
                } }
    };
    
    (@gen_forwards $enum_name:ident
        | { | }
        | { $( $variant_name:ident ( $( $variant_type:ty )? ) )* }
        | { $( $methods:tt )* }) => {
            
            $( $methods )*
    };
    
    (@gen_forward $enum_name:ident $method_name:ident(&self $( ,$param_name:ident : $param_type:ty )* ) $( -> $return_type:ty )?
        | { $variant_name:ident ( $( $variant_type:ty )? ) $( $rest:ident ( $( $rest_type:ty )? ) )* }
        | { $( $matches:tt )* }) => {
            
            lib::wrapper_enum!{@gen_forward $enum_name $method_name(&self $( ,$param_name : $param_type )* ) $( -> $return_type )?
                | { $( $rest ( $( $rest_type )? ) )* }
                | { $( $matches )* $enum_name::$variant_name(inner) => inner.$method_name( $( $param_name ),* ), } }
    };
    
    (@gen_forward $enum_name:ident $method_name:ident(&self $( ,$param_name:ident : $param_type:ty )* ) $( -> $return_type:ty )?
        | {}
        | { $( $matches:tt )* }) => {
            
            pub fn $method_name(&self, $( $param_name : $param_type ),* ) $( -> $return_type )? {
                match self {
                    $( $matches )*
                }
            }
    };
    
    (@count ) => { 0usize };
    (@count $head:ident $( $tail:ident )* ) => { 1usize + lib::wrapper_enum!(@count $( $tail )*) };
}