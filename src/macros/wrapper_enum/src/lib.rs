
extern crate proc_macro;
use proc_macro::TokenStream;
use syn::*;
use syn::parse::*;

mod forwarded_trait;
mod wrapper_enum;

#[proc_macro]
pub fn wrapper_enum(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as WrapperEnumDecl);
    
    parsed.generate()
}

struct WrapperEnumDecl {
    w_enum: wrapper_enum::WrapperEnum,
    f_traits: Vec<forwarded_trait::ForwardedTrait>,
}

impl Parse for WrapperEnumDecl {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut w_enum = None;
        let mut f_traits = Vec::new();
        
        loop {
            if input.is_empty() {
                break;
            }
            
            let tokens = input.fork();
            let _attrs = tokens.call(Attribute::parse_outer)?;
            let _vis: Visibility = tokens.parse()?;
            
            if tokens.peek(Token![enum]) {
                if w_enum.is_some() {
                    return Err(tokens.error("only one wrapper_enum is allowed per invocation of the macro."));
                } else {
                    let parsed_enum = input.parse::<wrapper_enum::WrapperEnum>()?;
                    w_enum = Some(parsed_enum);
                }
            } else if tokens.peek(forwarded_trait::keyword::fwd) {
                let parsed_trait = input.parse::<forwarded_trait::ForwardedTrait>()?;
                f_traits.push(parsed_trait);
            } else if tokens.peek(forwarded_trait::keyword::ext) {
                let parsed_trait = input.parse::<forwarded_trait::ForwardedTrait>()?;
                f_traits.push(parsed_trait);
            } else {
                return Err(tokens.error("expected either a wrapper_enum or a forwarded_trait."))
            }
        }
    
        if w_enum.is_none() {
            return Err(input.error("a wrapper_enum must be provided."));
        }
        
        if f_traits.is_empty() {
            return Err(input.error("at least one forwarded_trait must be provided."));
        }
        
        Ok(WrapperEnumDecl {
            w_enum: w_enum.unwrap(),
            f_traits,
        })
    }
}

impl WrapperEnumDecl {
    fn generate(&self) -> TokenStream {
        let mut tokens = TokenStream::new();
        
        tokens.extend(self.generate_forwarded_traits());
        tokens.extend(self.generate_wrapper_enum());
        tokens.extend(self.generate_wrapper_enum_impl());
        tokens.extend(self.generate_ext_forwards());
        
        tokens
    }
    
    fn generate_forwarded_traits(&self) -> TokenStream {
        let mut tokens = proc_macro2::TokenStream::new();
        
        for f_trait in &self.f_traits {
            tokens.extend(f_trait.generate());
        }
        
        tokens.into()
    }
    
    fn generate_wrapper_enum(&self) -> TokenStream {
        self.w_enum.generate().into()
    }
    
    fn generate_wrapper_enum_impl(&self) -> TokenStream {
        self.w_enum.generate_impl(&self).into()
    }
    
    fn generate_ext_forwards(&self) -> TokenStream {
        let impls = self.f_traits.iter().map(|f_trait| {
            f_trait.generate_ext_forwards(&self)
        });
        
        quote::quote! {
            #(#impls)*
        }.into()
    }
}