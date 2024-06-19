#![allow(dead_code, clippy::manual_flatten, clippy::for_kv_map)]

use paste::paste;
use rustdoc_types::{
    Abi, Constant, Deprecation, Discriminant, DynTrait, Enum, ExternalCrate, FnDecl, Function,
    FunctionPointer, GenericArg, GenericArgs, GenericBound, GenericParamDef, GenericParamDefKind,
    Generics, Header, Id, Impl, Import, Item, ItemEnum, Module, OpaqueTy, Path, PolyTrait,
    Primitive, ProcMacro, Static, Struct, StructKind, Term, Trait, TraitAlias, Type, TypeAlias,
    TypeBinding, TypeBindingKind, Union, Variant, VariantKind, Visibility, WherePredicate
};
use crate::{docs::{FnArg, Infer, Lifetime}, utils::{Result, OK}};

macro_rules! func {
    {
        $(for $ty:ident : |$v:pat_param, $x:pat_param| -> $body:stmt)+
    } => {
        paste! {
            $(
                pub fn [<visit_ $ty:snake>]($v: &mut impl Visitor, $x: &$ty) -> Result {
                    $body
                    OK
                }

                pub fn [<visit_ $ty:snake _mut>]($v: &mut impl VisitorMut, $x: &mut $ty) -> Result {
                    $body
                    OK
                }
            )+
        }

        pub trait Visitor: Sized {
            paste! {
                $(
                    fn [<visit_ $ty:snake>](&mut self, x: &$ty) -> Result {
                        [<visit_ $ty:snake>](self, x)
                    }
                )+
            }
        }

        pub trait VisitorMut: Sized {
            paste! {
                $(
                    fn [<visit_ $ty:snake>](&mut self, x: &mut $ty) -> Result {
                        [<visit_ $ty:snake _mut>](self, x)
                    }
                )+
            }
        }
    };
}

func! {
    for Abi: |_, _| -> {}

    for Constant: |_, _| -> {}

    for Deprecation: |_, _| -> {}

    for Discriminant: |_, _| -> {}

    for DynTrait: |v, DynTrait { traits, .. }| -> for x in traits {
        v.visit_poly_trait(x)?;
    }

    for Enum: |v, Enum { generics, variants, impls, .. }| -> {
        v.visit_generics(generics)?;
        for x in variants {
            v.visit_id(x)?;
        }
        for x in impls {
            v.visit_id(x)?;
        }
    }

    for ExternalCrate: |_, _| -> {}

    for FnArg: |v, (_, x)| -> v.visit_type(x)?

    for FnDecl: |v, FnDecl { inputs, output, .. }| -> {
        for x in inputs {
            v.visit_fn_arg(x)?;
        }
        if let Some(x) = output {
            v.visit_type(x)?;
        }
    }

    for Function: |v, Function { decl, generics, header, .. }| -> {
        v.visit_fn_decl(decl)?;
        v.visit_generics(generics)?;
        v.visit_header(header)?;
    }

    for FunctionPointer: |v, FunctionPointer { decl, generic_params, header }| -> {
        for x in generic_params {
            v.visit_generic_param_def(x)?;
        }
        v.visit_header(header)?;
        v.visit_fn_decl(decl)?;
    }

    for GenericArg: |v, x| -> match x {
        GenericArg::Lifetime(lifetime) => v.visit_lifetime(lifetime)?,
        GenericArg::Infer => v.visit_infer(Infer.into())?,
        GenericArg::Type(r#type) => v.visit_type(r#type)?,
        GenericArg::Const(r#const) => v.visit_constant(r#const)?,
    }

    for GenericArgs: |v, x| -> match x {
        GenericArgs::AngleBracketed { args, bindings } => {
            for x in args {
                v.visit_generic_arg(x)?;
            }
            for x in bindings {
                v.visit_type_binding(x)?;
            }
        }
        GenericArgs::Parenthesized { inputs, output } => {
            for x in inputs {
                v.visit_type(x)?;
            }
            if let Some(x) = output {
                v.visit_type(x)?;
            }
        }
    }

    for GenericBound: |v, x| -> match x {
        GenericBound::TraitBound { trait_, generic_params, .. } => {
            v.visit_path(trait_)?;
            for x in generic_params {
                v.visit_generic_param_def(x)?;
            }
        }
        GenericBound::Outlives(lifetime) => v.visit_lifetime(lifetime)?,
    }

    for GenericParamDef: |v, GenericParamDef { kind, .. }| -> match kind {
        GenericParamDefKind::Lifetime { .. } => {}
        GenericParamDefKind::Type { bounds, default, .. } => {
            for x in bounds {
                v.visit_generic_bound(x)?;
            }
            if let Some(x) = default {
                v.visit_type(x)?;
            }
        }
        GenericParamDefKind::Const { type_, .. } => v.visit_type(type_)?,
    }

    for Generics: |v, Generics { params, where_predicates }| -> {
        for x in params {
            v.visit_generic_param_def(x)?;
        }
        for x in where_predicates {
            v.visit_where_predicate(x)?;
        }
    }

    for Header: |v, Header { abi, .. }| -> v.visit_abi(abi)?

    for Id: |_, _| -> {}

    for Infer: |_, _| -> {}

    for Impl: |v, Impl { generics, trait_, for_, items, blanket_impl, .. }| -> {
        v.visit_generics(generics)?;
        if let Some(x) = trait_ {
            v.visit_path(x)?;
        }
        v.visit_type(for_)?;
        for x in items {
            v.visit_id(x)?;
        }
        if let Some(x) = blanket_impl {
            v.visit_type(x)?;
        }
    }

    for Import: |v, Import { id, .. }| -> if let Some(x) = id {
        v.visit_id(x)?;
    }

    for Item: |v, Item { id, visibility, links, deprecation, inner, .. }| -> {
        v.visit_id(id)?;
        v.visit_visibility(visibility)?;
        for (_, x) in links {
            v.visit_id(x)?;
        }
        if let Some(x) = deprecation {
            v.visit_deprecation(x)?;
        }
        match inner {
            | ItemEnum::ExternCrate { .. }
            | ItemEnum::ForeignType
            | ItemEnum::Macro(_) => {}
            ItemEnum::Module(x) => v.visit_module(x)?,
            ItemEnum::Import(x) => v.visit_import(x)?,
            ItemEnum::Union(x) => v.visit_union(x)?,
            ItemEnum::Struct(x) => v.visit_struct(x)?,
            ItemEnum::StructField(x) => v.visit_type(x)?,
            ItemEnum::Enum(x) => v.visit_enum(x)?,
            ItemEnum::Variant(x) => v.visit_variant(x)?,
            ItemEnum::Function(x) => v.visit_function(x)?,
            ItemEnum::Trait(x) => v.visit_trait(x)?,
            ItemEnum::TraitAlias(x) => v.visit_trait_alias(x)?,
            ItemEnum::Impl(x) => v.visit_impl(x)?,
            ItemEnum::TypeAlias(x) => v.visit_type_alias(x)?,
            ItemEnum::OpaqueTy(x) => v.visit_opaque_ty(x)?,
            ItemEnum::Constant { type_, const_ } => {
                v.visit_type(type_)?;
                v.visit_constant(const_)?;
            }
            ItemEnum::Static(x) => v.visit_static(x)?,
            ItemEnum::ProcMacro(x) => v.visit_proc_macro(x)?,
            ItemEnum::Primitive(x) => v.visit_primitive(x)?,
            ItemEnum::AssocConst { type_, .. } => v.visit_type(type_)?,
            ItemEnum::AssocType { generics, bounds, default } => {
                v.visit_generics(generics)?;
                for x in bounds {
                    v.visit_generic_bound(x)?;
                }
                if let Some(x) = default {
                    v.visit_type(x)?;
                }
            }
        }
    }

    for Lifetime: |_, _| -> {}

    for Module: |v, Module { items, .. }| -> for x in items {
        v.visit_id(x)?;
    }

    for OpaqueTy: |v, OpaqueTy { bounds, generics }| -> {
        for x in bounds {
            v.visit_generic_bound(x)?;
        }
        v.visit_generics(generics)?;
    }

    for Path: |v, Path { id, args, .. }| -> {
        v.visit_id(id)?;
        if let Some(x) = args {
            v.visit_generic_args(x)?;
        }
    }

    for PolyTrait: |v, PolyTrait { trait_, generic_params }| -> {
        v.visit_path(trait_)?;
        for x in generic_params {
            v.visit_generic_param_def(x)?;
        }
    }

    for Primitive: |v, Primitive { impls, .. }| -> {
        for x in impls {
            v.visit_id(x)?;
        }
    }

    for ProcMacro: |_, _| -> {}

    for Static: |v, Static { type_, .. }| -> v.visit_type(type_)?

    for Struct: |v, Struct { kind, generics, impls }| -> {
        match kind {
            StructKind::Unit => {}
            StructKind::Tuple(fields) => for x in fields {
                if let Some(x) = x {
                    v.visit_id(x)?;
                }
            }
            StructKind::Plain { fields, .. } => for x in fields {
                v.visit_id(x)?;
            }
        }
        v.visit_generics(generics)?;
        for x in impls {
            v.visit_id(x)?;
        }
    }

    for Term: |v, x| -> match x {
        Term::Type(r#type) => v.visit_type(r#type)?,
        Term::Constant(r#const) => v.visit_constant(r#const)?,
    }

    for Trait: |v, Trait { items, generics, bounds, implementations, .. }| -> {
        for x in items {
            v.visit_id(x)?;
        }
        v.visit_generics(generics)?;
        for x in bounds {
            v.visit_generic_bound(x)?;
        }
        for x in implementations {
            v.visit_id(x)?;
        }
    }

    for TraitAlias: |v, TraitAlias { generics, params }| -> {
        v.visit_generics(generics)?;
        for x in params {
            v.visit_generic_bound(x)?;
        }
    }

    for Type: |v, x| -> match x {
        Type::Generic(_) | Type::Primitive(_) => {}
        Type::Infer => v.visit_infer(Infer.into())?,
        Type::ResolvedPath(path) => v.visit_path(path)?,
        Type::DynTrait(trait_) => v.visit_dyn_trait(trait_)?,
        Type::FunctionPointer(ptr) => v.visit_function_pointer(ptr)?,
        Type::Tuple(fields) => for x in fields {
            v.visit_type(x)?;
        }
        Type::Slice(type_) => v.visit_type(type_)?,
        Type::Array { type_, .. } => v.visit_type(type_)?,
        Type::Pat { type_, .. } => v.visit_type(type_)?,
        Type::ImplTrait(bounds) => for x in bounds {
            v.visit_generic_bound(x)?;
        }
        Type::RawPointer { type_, .. } => v.visit_type(type_)?,
        Type::BorrowedRef { type_, .. } => v.visit_type(type_)?,
        Type::QualifiedPath { args, self_type, trait_, .. } => {
            v.visit_generic_args(args)?;
            v.visit_type(self_type)?;
            if let Some(x) = trait_ {
                v.visit_path(x)?;
            }
        }
    }

    for TypeAlias: |v, TypeAlias { type_, generics }| -> {
        v.visit_type(type_)?;
        v.visit_generics(generics)?;
    }

    for TypeBinding: |v, TypeBinding { args, binding, .. }| -> {
        v.visit_generic_args(args)?;
        match binding {
            TypeBindingKind::Equality(term) => v.visit_term(term)?,
            TypeBindingKind::Constraint(bounds) => for x in bounds {
                v.visit_generic_bound(x)?;
            }
        }
    }

    for Union: |v, Union { generics, fields, impls, .. }| -> {
        v.visit_generics(generics)?;
        for x in fields {
            v.visit_id(x)?;
        }
        for x in impls {
            v.visit_id(x)?;
        }
    }

    for Variant: |v, Variant { kind, discriminant }| -> {
        match kind {
            VariantKind::Plain => {}
            VariantKind::Tuple(fields) => for x in fields {
                if let Some(x) = x {
                    v.visit_id(x)?;
                }
            }
            VariantKind::Struct { fields, .. } => for x in fields {
                v.visit_id(x)?;
            }
        }
        if let Some(x) = discriminant {
            v.visit_discriminant(x)?;
        }
    }

    for Visibility: |v, x| -> match x {
        Visibility::Public | Visibility::Default | Visibility::Crate => {}
        Visibility::Restricted { parent, .. } => v.visit_id(parent)?,
    }

    for WherePredicate: |v, x| -> match x {
        WherePredicate::BoundPredicate { type_, bounds, generic_params } => {
            v.visit_type(type_)?;
            for x in bounds {
                v.visit_generic_bound(x)?;
            }
            for x in generic_params {
                v.visit_generic_param_def(x)?;
            }
        }
        WherePredicate::RegionPredicate { bounds, .. } => for x in bounds {
            v.visit_generic_bound(x)?;
        }
        WherePredicate::EqPredicate { lhs, rhs } => {
            v.visit_type(lhs)?;
            v.visit_term(rhs)?;
        }
    }
}
