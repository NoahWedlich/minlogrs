
use quote::format_ident;
use syn::*;
use syn::parse::*;
use syn::punctuated::*;

use quote::quote;

pub struct WrapperEnum {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub enum_token: Token![enum],
    pub name: Ident,
    pub generics: Generics,
    pub variants: Punctuated<WrapperEnumVariant, Token![,]>,
}

impl Parse for WrapperEnum {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis: Visibility = input.parse()?;
        let enum_token: Token![enum] = input.parse()?;
        let name: Ident = input.parse()?;
        let generics: Generics = input.parse()?;
        
        let content;
        let _ = braced!(content in input);
        let variants = content.parse_terminated(WrapperEnumVariant::parse, Token![,])?;
        
        Ok(WrapperEnum {
            attrs,
            vis,
            enum_token,
            name,
            generics,
            variants,
        })
    }
}

impl WrapperEnum {
    pub fn generate(&self) -> proc_macro2::TokenStream {
        let attrs = &self.attrs;
        let vis = &self.vis;
        let enum_token = &self.enum_token;
        let name = &self.name;
        let generics = &self.generics;
        let variants = self.variants.iter().map(|v| v.generate()).collect::<Vec<_>>();
        
        quote! {
            #(#attrs)*
            #vis #enum_token #name #generics {
                #(#variants),*
            }
        }
    }
    
    pub fn generate_impl(&self, wrapper_enum: &crate::WrapperEnumDecl) -> proc_macro2::TokenStream {
        let name = &self.name;
        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();
        
        let variant_count = self.generate_variant_count();
        let variant_tests = self.generate_variant_tests();
        let variant_accessors = self.generate_variant_accessors();
        
        let guards = wrapper_enum.f_traits.iter().filter_map(|f_trait| {
            f_trait.generate_guards(wrapper_enum)
        });
        
        let forwards = wrapper_enum.f_traits.iter().map(|f_trait| {
            f_trait.generate_forwards(wrapper_enum)
        }).collect::<Vec<_>>();
        
        quote! {
            impl #impl_generics #name #ty_generics #where_clause
            where
                #(#guards),*
            {
                pub fn variant_count() -> usize {
                    #variant_count
                }
                
                #variant_tests
                
                #variant_accessors
                
                #(#forwards)*
            }
        }
    }
    
    fn generate_variant_count(&self) -> usize {
        self.variants.len()
    }
    
    fn generate_variant_tests(&self) -> proc_macro2::TokenStream {
        let tests = self.variants.iter().map(|variant| {
            let variant_name = &variant.name;
            let test_fn_name = format_ident!("is_{}", Self::string_to_snake_case(&variant.field.name.to_string()));
            quote! {
                pub fn #test_fn_name(&self) -> bool {
                    matches!(self, Self::#variant_name(_))
                }
            }
        }).collect::<Vec<_>>();
        
        quote! {
            #(#tests)*
        }
    }
    
    fn generate_variant_accessors(&self) -> proc_macro2::TokenStream {
        let accessors = self.variants.iter().map(|variant| {
            let variant_name = &variant.name;
            let accessor_fn_name = format_ident!("to_{}", Self::string_to_snake_case(&variant.field.name.to_string()));
            let field_ty = &variant.field.ty;
            quote! {
                pub fn #accessor_fn_name(&self) -> Option<&#field_ty> {
                    if let Self::#variant_name(inner) = self {
                        Some(inner)
                    } else {
                        None
                    }
                }
            }
        }).collect::<Vec<_>>();
        
        quote! {
            #(#accessors)*
        }
    }
    
    fn string_to_snake_case(s: &str) -> String {
        let mut snake_case = String::new();
        for (i, ch) in s.chars().enumerate() {
            if ch.is_uppercase() {
                if i != 0 {
                    snake_case.push('_');
                }
                for lower_ch in ch.to_lowercase() {
                    snake_case.push(lower_ch);
                }
            } else {
                snake_case.push(ch);
            }
        }
        snake_case
    }
}

pub struct WrapperEnumVariant {
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub field: WrapperEnumVariantField,
    pub discriminant: Option<(Token![=], Expr)>,
}

impl Parse for WrapperEnumVariant {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let name: Ident = input.parse()?;
        let content;
        let _ = parenthesized!(content in input);
        let field: WrapperEnumVariantField = content.parse()?;
        let discriminant: Option<(Token![=], Expr)> = if input.peek(Token![=]) {
            let eq_token: Token![=] = input.parse()?;
            let expr: Expr = input.parse()?;
            Some((eq_token, expr))
        } else {
            None
        };
        
        Ok(WrapperEnumVariant {
            attrs,
            name,
            field,
            discriminant,
        })
    }
}

impl WrapperEnumVariant {
    pub fn generate(&self) -> proc_macro2::TokenStream {
        let attrs = &self.attrs;
        let name = &self.name;
        let field = self.field.generate();
        let discriminant = if let Some((eq_token, expr)) = &self.discriminant {
            quote! { #eq_token #expr }
        } else {
            quote! {}
        };
        
        quote! {
            #(#attrs)*
            #name ( #field ) #discriminant
        }
    }
}

pub struct WrapperEnumVariantField {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub name: Ident,
    pub ty: Type,
}

impl Parse for WrapperEnumVariantField {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis: Visibility = input.parse()?;
        let name: Ident = input.parse()?;
        let _: Token![:] = input.parse()?;
        let ty: Type = input.parse()?;
        
        Ok(WrapperEnumVariantField {
            attrs,
            vis,
            name,
            ty,
        })
    }
}

impl WrapperEnumVariantField {
    pub fn generate(&self) -> proc_macro2::TokenStream {
        let attrs = &self.attrs;
        let vis = &self.vis;
        let ty = &self.ty;
        
        quote! {
            #(#attrs)*
            #vis #ty
        }
    }
}