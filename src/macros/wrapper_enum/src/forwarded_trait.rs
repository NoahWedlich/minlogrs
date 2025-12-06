
use quote::ToTokens;
use syn::*;
use syn::parse::*;
use syn::punctuated::*;
use syn::spanned::Spanned;

use quote::{quote, quote_spanned};

enum FwdTraitKind {
    Forwarded,
    External,
}

pub struct ForwardedTrait {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    kind: FwdTraitKind,
    pub bound: Option<keyword::bnd>,
    pub name: Ident,
    pub generics: Generics,
    pub supertraits: Punctuated<TypeParamBound, Token![+]>,
    pub items: Vec<MaybeForwardedItem>,
}

impl Parse for ForwardedTrait {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis: Visibility = input.parse()?;
        
        let kind = if input.peek(keyword::fwd) {
            input.parse::<keyword::fwd>()?;
            FwdTraitKind::Forwarded
        } else if input.peek(keyword::ext) {
            input.parse::<keyword::ext>()?;
            FwdTraitKind::External
        } else {
            return Err(input.error("expected 'fwd' or 'ext' keyword for forwarded trait declaration."));
        };
        
        let bound: Option<keyword::bnd> = if input.peek(keyword::bnd) {
            Some(input.parse()?)
        } else {
            None
        };
        
        let _ = input.parse::<Token![trait]>()?;
        let name: Ident = input.parse()?;
        let generics: Generics = input.parse()?;
        
        let mut supertraits = Punctuated::new();
        if input.peek(Token![:]) {
            let colon: Token![:] = input.parse()?;
            loop {
                if input.peek(Token![where]) || input.peek(token::Brace) {
                    break;
                }
                
                supertraits.push_value(input.parse()?);
                
                if input.peek(Token![where]) || input.peek(token::Brace) {
                    break;
                }
                
                supertraits.push_punct(input.parse()?);
            }
            
            Some(colon)
        } else {
            None
        };
        
        let content;
        let _ = braced!(content in input);
        
        let mut items = Vec::new();
        loop {
            if content.is_empty() {
                break;
            }
            
            items.push(content.parse::<MaybeForwardedItem>()?);
        }
        
        Ok(ForwardedTrait {
            attrs,
            vis,
            kind,
            bound,
            name,
            generics,
            supertraits,
            items,
        })
    }
}

impl ForwardedTrait {
    pub fn generate(&self) -> proc_macro2::TokenStream {
        if let FwdTraitKind::External = self.kind {
            quote! {}
        } else {
            let items = self.items.iter().map(|item| {
                item.generate_for_trait()
            }).collect::<Vec<_>>();
            
            let attrs = &self.attrs;
            let vis = &self.vis;
            let name = &self.name;
            let generics = &self.generics;
            
            let supertraits = if !self.supertraits.is_empty() {
                let bounds = &self.supertraits;
                quote! {
                    : #bounds
                }
            } else {
                quote! {}
            };
            
            quote! {
                #(#attrs)*
                #vis trait #name #generics #supertraits {
                    #(#items)*
                }
            }
        }
    }
    
    pub fn generate_forwards(&self, wrapper_enum: &crate::WrapperEnumDecl) -> proc_macro2::TokenStream {
        if let FwdTraitKind::External = self.kind {
            quote! {}
        } else {
            let forwards = self.items.iter().map(|item| {
                item.generate_forwards(wrapper_enum, true)
            }).collect::<Vec<_>>();
            
            quote! {
                #(#forwards)*
            }
        }
    }
    
    pub fn generate_ext_forwards(&self, wrapper_enum: &crate::WrapperEnumDecl) -> proc_macro2::TokenStream {
        if let FwdTraitKind::External = self.kind {
            let forwards = self.items.iter().map(|item| {
                item.generate_forwards(wrapper_enum, false)
            }).collect::<Vec<_>>();
            
            let trait_name = &self.name;
            let (impl_generics, ty_generics, where_clause) = wrapper_enum.w_enum.generics.split_for_impl();
            let (_, trait_generics, _ ) = self.generics.split_for_impl();
            let enum_name = &wrapper_enum.w_enum.name;
            
            quote! {
                impl #impl_generics #trait_name #trait_generics for #enum_name #ty_generics #where_clause {
                    #(#forwards)*
                }
            }
        } else {
            quote! {}
        }
    }
    
    pub fn generate_guards(&self, wrapper_enum: &crate::WrapperEnumDecl) -> proc_macro2::TokenStream {
        if let Some(_) = &self.bound {
            let guards = wrapper_enum.w_enum.variants.iter().map(|variant| {
                let name = &self.name;
                let (_, generics, _) = self.generics.split_for_impl();
                let ty = &variant.field.ty;
                
                quote_spanned! {ty.span()=>
                    #ty: #name #generics
                }
            });
            
            quote! {
                #(#guards),*
            }
        } else {
            return quote! {};
        }
    }
}

pub enum MaybeForwardedItem {
    Item(TraitItem),
    ForwardedMethod(ForwardedMethod),
}

impl Parse for MaybeForwardedItem {
    fn parse(input: ParseStream) -> Result<Self> {
        let tokens = input.fork();
        let _attrs = tokens.call(Attribute::parse_outer)?;
        let _vis: Visibility = tokens.parse()?;
        
        if tokens.peek(keyword::fwd) {
            input.parse::<ForwardedMethod>()
                .map(MaybeForwardedItem::ForwardedMethod)
        } else {
            input.parse::<TraitItem>()
                .map(MaybeForwardedItem::Item)
        }
    }
}

impl MaybeForwardedItem {
    pub fn generate_for_trait(&self) -> proc_macro2::TokenStream {
        match self {
            MaybeForwardedItem::Item(item) => quote! { #item },
            MaybeForwardedItem::ForwardedMethod(fwd_method) => fwd_method.generate_for_trait(),
        }
    }
    
    pub fn generate_forwards(&self, wrapper_enum: &crate::WrapperEnumDecl, add_vis: bool) -> proc_macro2::TokenStream {
        match self {
            MaybeForwardedItem::Item(_) => quote! {},
            MaybeForwardedItem::ForwardedMethod(fwd_method) => fwd_method.generate_forwards(wrapper_enum, add_vis),
        }
    }
}

pub struct ForwardedMethod {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub sig: Signature,
    pub body: Option<Block>,
}

impl Parse for ForwardedMethod {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis: Visibility = input.parse()?;
        let _: keyword::fwd = input.parse()?;
        let sig: Signature = input.parse()?;
        let body: Option<Block> = if input.peek(token::Brace) {
            Some(input.parse()?)
        } else {
            None
        };
        
        Ok(ForwardedMethod {
            attrs,
            vis,
            sig,
            body,
        })
    }
}

impl ForwardedMethod {
    pub fn generate_for_trait(&self) -> proc_macro2::TokenStream {
        let attrs = &self.attrs;
        let sig = &self.sig;
        let body = if let Some(body) = &self.body {
            quote! { #body }
        } else {
            quote! { ; }
        };
        
        quote! {
            #(#attrs)*
            #sig #body
        }
    }
    
    pub fn generate_forwards(&self, wrapper_enum: &crate::WrapperEnumDecl, add_vis: bool) -> proc_macro2::TokenStream {
        let enum_name = &wrapper_enum.w_enum.name;
        
        let vis = if add_vis {
            let svis = &self.vis;
            quote! { #svis }
        } else {
            quote! {}
        };
        
        let sig = &self.sig;
        let method_name = &sig.ident;
        
        let trait_generics = if !sig.generics.params.is_empty() {
            let (_, gens, _) = sig.generics.split_for_impl();
            quote! { :: #gens }
        } else {
            quote! {}
        };
        
        let ty_generics = if !wrapper_enum.w_enum.generics.params.is_empty() {
            let (_, gens, _) = wrapper_enum.w_enum.generics.split_for_impl();
            quote! { :: #gens }
        } else {
            quote! {}
        };
        
        let forward_arms = wrapper_enum.w_enum.variants.iter().map(|variant| {
            let variant_name = &variant.name;
            let inner_name = &variant.field.name;
            let call_args = sig.inputs.iter().filter_map(|arg| {
                if let FnArg::Typed(pat_type) = arg {
                    Some(pat_type.pat.to_token_stream())
                } else {
                    None
                }
            });
            
            quote! {
                #enum_name #ty_generics :: #variant_name ( #inner_name ) => #inner_name #trait_generics . #method_name ( #(#call_args),* ),
            }
        });
        
        quote! {
            #vis #sig {
                match self {
                    #(#forward_arms)*
                }
            }
        }
    }
}

pub mod keyword {
    syn::custom_keyword!(fwd);
    syn::custom_keyword!(ext);
    syn::custom_keyword!(bnd);
}