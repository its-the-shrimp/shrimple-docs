use std::{fmt::Display, io::Write};

use anyhow::Context;
use rustdoc_types::{Abi, Constant, Discriminant, DynTrait, Enum, FnDecl, Function, FunctionPointer, GenericArg, GenericArgs, GenericBound, GenericParamDef, GenericParamDefKind, Generics, Header, Impl, Import, Item, ItemEnum, MacroKind, Path, PolyTrait, ProcMacro, Static, Struct, StructKind, Term, Trait, TraitAlias, TraitBoundModifier, Type, TypeAlias, TypeBinding, TypeBindingKind, Union, Variant, VariantKind, WherePredicate};

use crate::utils::{BoolExt, Result, OK};

pub trait RustFormat {
    fn print(&self, out: &mut impl Write) -> Result;
}

impl<T: RustFormat> RustFormat for Box<T> {
    fn print(&self, out: &mut impl Write) -> Result {
        T::print(self, out)
    }
}

fn maybe_print(
    prefix: impl Display,
    item: &Option<impl RustFormat>,
    postfix: impl Display,
    out: &mut impl Write,
) -> Result {
    if let Some(item) = item {
        write!(out, "{prefix}")?;
        item.print(out)?;
        write!(out, "{postfix}")?;
    }
    OK
}

fn print_with_sep<'item, T: RustFormat + 'item>(
    prefix: impl Display,
    items: impl IntoIterator<Item = &'item T>,
    sep: impl Display,
    postfix: impl Display,
    out: &mut impl Write,
)
    -> Result
{
    let mut items = items.into_iter();
    let Some(first) = items.next() else { return OK };
    write!(out, "{prefix}")?;
    first.print(out)?;
    for item in items {
        write!(out, "{sep}")?;
        item.print(out)?;
    }
    write!(out, "{postfix}")?;
    OK
}

/// Unlike the `RustFormat` impl on `Generics`, this allows to do anything between printing the
/// parameters and the `where` predicates.
fn print_generics<Out: Write, E: Into<anyhow::Error>>(
    item: &Generics,
    f: impl FnOnce(&mut Out) -> Result<(), E>,
    postfix: impl Display,
    out: &mut Out,
) -> Result {
    let params = item.params.iter().filter(|i| match i.kind {
        GenericParamDefKind::Type { synthetic, .. } => !synthetic,
        _ => true,
    });
    print_with_sep("<", params, ", ", ">", out)?;
    f(out).map_err(E::into)?;
    print_with_sep(
        "\nwhere\n    ",
        &item.where_predicates,
        ",\n    ",
        postfix,
        out,
    )?;
    OK
}

impl RustFormat for PolyTrait {
    fn print(&self, out: &mut impl Write) -> Result {
        print_with_sep("for<", &self.generic_params, ", ", "> ", out)?;
        self.trait_.print(out)
    }
}

impl RustFormat for DynTrait {
    fn print(&self, out: &mut impl Write) -> Result {
        write!(out, "dyn ")?;
        print_with_sep("", &self.traits, ", ", "", out)?;
        if let Some(lifetime) = &self.lifetime {
            write!(out, " + '{lifetime}")?;
        }
        OK
    }
}

impl RustFormat for Header {
    fn print(&self, out: &mut impl Write) -> Result {
        if self.async_ {
            write!(out, "async ")?;
        }
        if self.const_ {
            write!(out, "const ")?;
        }
        if self.unsafe_ {
            write!(out, "unsafe ")?;
        }
        match &self.abi {
            Abi::Rust => {},
            Abi::C { .. } => write!(out, "extern \"C\" ")?,
            Abi::Other(name) => write!(out, "extern {name:?} ")?,
            _ => write!(out, "extern ... ")?,
        }
        OK
    }
}

/// A function argument declaration
impl RustFormat for (String, Type) {
    fn print(&self, out: &mut impl Write) -> Result {
        if !self.0.is_empty() {
            write!(out, "{}: ", self.0)?;
        }
        self.1.print(out)
    }
}

impl RustFormat for FnDecl {
    fn print(&self, out: &mut impl Write) -> Result {
        write!(out, "(")?;
        print_with_sep("", &self.inputs, ", ", "", out)?;
        if self.c_variadic {
            write!(out, "{}...", self.inputs.is_empty().pick("", ", "))?;
        }
        write!(out, ")")?;
        maybe_print(" -> ", &self.output, "", out)?;
        OK
    }
}

impl RustFormat for FunctionPointer {
    fn print(&self, out: &mut impl Write) -> Result {
        print_with_sep("for<", &self.generic_params, ", ", "> ", out)?;
        self.header.print(out)?;
        self.decl.print(out)?;
        OK
    }
}

impl RustFormat for Type {
    fn print(&self, out: &mut impl Write) -> Result {
        Ok(match self {
            Self::ResolvedPath(path) => path.print(out)?,

            Self::DynTrait(r#dyn) => r#dyn.print(out)?,

            Self::Generic(t) | Self::Primitive(t) => write!(out, "{t}")?,

            Self::FunctionPointer(r#fn) => r#fn.print(out)?,

            Self::Tuple(types) => {
                write!(out, "(")?;
                print_with_sep("", types, ", ", "", out)?;
                write!(out, "{})", (types.len() == 1).pick(",", ""))?;
            }

            Self::Slice(t) => {
                write!(out, "[")?;
                t.print(out)?;
                write!(out, "]")?;
            }

            Self::Array { type_, len } => {
                write!(out, "[")?;
                type_.print(out)?;
                write!(out, "; {len}]")?;
            }

            Self::Pat { type_, .. } => {
                type_.print(out)?;
                write!(out, " is ...")?;
            }

            Self::ImplTrait(r#impl) => print_with_sep("impl ", r#impl, " + ", "", out)?,

            Self::Infer => write!(out, "_")?,

            Self::RawPointer { mutable, type_ } => {
                write!(out, "*{} ", mutable.pick("mut", "const"))?;
                type_.print(out)?;
            }

            Self::BorrowedRef { lifetime, mutable, type_ } => {
                write!(out, "&")?;
                if let Some(lifetime) = lifetime {
                    write!(out, "'{lifetime} ")?;
                }
                if *mutable {
                    write!(out, "mut ")?;
                }
                type_.print(out)?;
            }

            Self::QualifiedPath { name, args, self_type, trait_ } => {
                let angles_needed = trait_.is_some()
                    || !matches!(&**self_type, Self::QualifiedPath {..} | Self::ResolvedPath(..));
                let [open, close] = angles_needed.pick(["<", ">"], ["", ""]);
                write!(out, "{open}")?;
                self_type.print(out)?;
                maybe_print(" as ", trait_, "", out)?;
                write!(out, "{close}::{name}")?;
                args.print(out)?;
            }
        })
    }
}

impl RustFormat for Constant {
    fn print(&self, out: &mut impl Write) -> Result {
        let to_print = self.value.as_deref()
            .unwrap_or_else(|| self.is_literal.pick(&*self.expr, "const { ... }"));
        write!(out, "{to_print}")?;
        OK
    }
}

impl RustFormat for GenericArg {
    fn print(&self, out: &mut impl Write) -> Result {
        Ok(match self {
            Self::Lifetime(name) => write!(out, "'{name}")?,
            Self::Type(t) => t.print(out)?,
            Self::Const(c) => c.print(out)?,
            Self::Infer => write!(out, "_")?,
        })
    }
}

impl RustFormat for TypeBinding {
    fn print(&self, out: &mut impl Write) -> Result {
        write!(out, "{}", self.name)?;
        self.args.print(out)?;
        match &self.binding {
            TypeBindingKind::Equality(term) => {
                write!(out, " = ")?;
                term.print(out)
            }
            TypeBindingKind::Constraint(bounds) => print_with_sep(": ", bounds, " + ", "", out),
        }
    }
}

impl RustFormat for GenericArgs {
    fn print(&self, out: &mut impl Write) -> Result {
        Ok(match self {
            Self::AngleBracketed { args, bindings } => {
                print_with_sep("<", args, ", ", "", out)?;
                print_with_sep(args.is_empty().pick("<", ", "), bindings, ", ", "", out)?;
                if !bindings.is_empty() || !args.is_empty() {
                    write!(out, ">")?;
                }
            }

            Self::Parenthesized { inputs, output } => {
                write!(out, "(")?;
                print_with_sep("", inputs, ", ", "", out)?;
                write!(out, ")")?;
                maybe_print(" -> ", output, "", out)?;
            }
        })
    }
}

impl RustFormat for Path {
    fn print(&self, out: &mut impl Write) -> Result {
        write!(out, "{}", self.name)?;
        maybe_print("", &self.args, "", out)?;
        OK
    }
}

impl RustFormat for GenericBound {
    fn print(&self, out: &mut impl Write) -> Result {
        Ok(match self {
            Self::TraitBound { trait_, generic_params, modifier } => {
                print_with_sep("for<", generic_params, ", ", "> ", out)?;
                match modifier {
                    TraitBoundModifier::None => {}
                    TraitBoundModifier::Maybe => write!(out, "?")?,
                    TraitBoundModifier::MaybeConst => write!(out, "~const ")?,
                }
                trait_.print(out)?;
            }
            Self::Outlives(lifetime) => write!(out, "'{lifetime}")?,
        })
    }
}

impl RustFormat for GenericParamDef {
    fn print(&self, out: &mut impl Write) -> Result {
        let Self { name, kind } = self;
        Ok(match kind {
            GenericParamDefKind::Lifetime { outlives } => {
                write!(out, "'{name}: ")?;
                if let [first, rest @ ..] = &**outlives {
                    write!(out, "'{first}")?;
                    for lifetime in rest {
                        write!(out, " + '{lifetime}")?;
                    }
                }
            }
            GenericParamDefKind::Type { bounds, default, .. } => {
                write!(out, "{name}: ")?;
                print_with_sep("", bounds, " + ", "", out)?;
                maybe_print(" = ", default, "", out)?;

            }
            GenericParamDefKind::Const { type_, default } => {
                write!(out, "const {name}: ")?;
                type_.print(out)?;
                if default.is_some() {
                    write!(out, " = ...")?;
                }
            }
        })
    }
}

impl RustFormat for Term {
    fn print(&self, out: &mut impl Write) -> Result {
        match self {
            Self::Type(t) => t.print(out),
            Self::Constant(c) => c.print(out),
        }
    }
}

impl RustFormat for WherePredicate {
    fn print(&self, out: &mut impl Write) -> Result {
        Ok(match self {
            Self::BoundPredicate { type_, bounds, generic_params } => {
                print_with_sep("for<", generic_params, ", ", "> ", out)?;
                type_.print(out)?;
                print_with_sep(": ", bounds, " + ", "", out)?;
            }

            Self::RegionPredicate { lifetime, bounds } => {
                write!(out, "'{lifetime}")?;
                print_with_sep(": ", bounds, " + ", "", out)?;
            }

            Self::EqPredicate { lhs, rhs } => {
                lhs.print(out)?;
                write!(out, " = ")?;
                rhs.print(out)?;
            }
        })
    }
}

impl RustFormat for Generics {
    fn print(&self, out: &mut impl Write) -> Result {
        print_generics(self, |_| OK, ",\n", out)
    }
}

impl RustFormat for Item {
    #[allow(clippy::too_many_lines)]
    fn print(&self, out: &mut impl Write) -> Result {
        match &self.inner {
            ItemEnum::Module(_) => {
                let name = self.name.as_ref().context("no module name")?;
                write!(out, "mod {name};")?;
            }

            ItemEnum::ExternCrate { name, rename } => {
                write!(out, "extern crate {name}")?;
                if let Some(rename) = rename {
                    write!(out, "as {rename};")?;
                }
                write!(out, ";")?;
            }

            ItemEnum::Import(Import { source, name, glob, .. }) => {
                write!(out, "use {source}")?;
                if *glob {
                    write!(out, "::*")?;
                }
                if !source.ends_with(name) {
                    write!(out, " as {name}")?;
                }
                write!(out, ";")?;
            }

            ItemEnum::Union(Union { generics, .. }) => {
                let name = self.name.as_ref().context("no union name")?;
                write!(out, "union {name}")?;
                generics.print(out)?;
                // TODO
                write!(out, "{}{{ ... }}", generics.where_predicates.is_empty().pick(" ", ""))?;
            }

            ItemEnum::Struct(Struct { kind, generics, .. }) => {
                let name = self.name.as_ref().context("no struct name")?;
                write!(out, "struct {name}")?;
                match kind {
                    StructKind::Unit => {
                        generics.print(out)?;
                        write!(out, ";")?;
                    }

                    StructKind::Tuple(_) => {
                        print_generics(generics, |out| write!(out, "(...)"), "", out)?;
                        write!(out, ";")?;
                    }

                    StructKind::Plain { .. } => {
                        generics.print(out)?;
                        // TODO
                        write!(out, "{}{{ ... }}",
                            generics.where_predicates.is_empty().pick(" ", ""))?;
                    }
                }
            }

            ItemEnum::StructField(field) => {
                let name = self.name.as_ref().context("no struct field name")?;
                writeln!(out, "struct ... {{")?;
                write!(out,  "    {name}: ")?;
                field.print(out)?;
                write!(out, ",\n}}")?;
            }

            ItemEnum::Enum(Enum { generics, .. }) => {
                let name = self.name.as_ref().context("no enum name")?;
                write!(out, "enum {name}")?;
                generics.print(out)?;
                // TODO
                write!(out, "{}{{ ... }}", generics.where_predicates.is_empty().pick(" ", ""))?;
            }

            ItemEnum::Variant(Variant { kind, discriminant }) => {
                let name = self.name.as_ref().context("no enum variant name")?;
                writeln!(out, "enum ... {{")?;
                write!(out,  "    {name}")?;
                match kind {
                    VariantKind::Plain => {}
                    // TODO
                    VariantKind::Tuple(_) => write!(out, "(...)")?,
                    // TODO
                    VariantKind::Struct { .. } => write!(out, "{{ ... }}")?,
                }
                if let Some(Discriminant { value, .. }) = discriminant {
                    write!(out, " = {value}")?;
                }
                write!(out, ",\n}}")?;
            }

            ItemEnum::Function(Function { decl, generics, header, has_body }) => {
                let name = self.name.as_ref().context("no function name")?;
                header.print(out)?;
                write!(out, "fn {name}")?;
                let [postfix, end] = has_body.pick([",\n", "{ ... }"], ["", ";"]);
                print_generics(generics, |out| decl.print(out), postfix, out)?;
                write!(out, "{end}")?;
            }

            ItemEnum::Trait(Trait { is_auto, is_unsafe, generics, bounds, .. }) => {
                let name = self.name.as_ref().context("no trait name")?;
                if *is_auto {
                    write!(out, "auto ")?;
                }
                if *is_unsafe {
                    write!(out, "unsafe ")?;
                }
                write!(out, "trait {name}")?;
                print_generics(
                    generics,
                    |out| print_with_sep(": ", bounds, " + ", "", out),
                    ",\n",
                    out,
                )?;
                write!(out, "{}{{ ... }}", generics.where_predicates.is_empty().pick(" ", ""))?;
            }

            ItemEnum::TraitAlias(TraitAlias { generics, params }) => {
                let name = self.name.as_ref().context("no trait alias name")?;
                write!(out, "trait {name}")?;
                print_generics(
                    generics,
                    |out| print_with_sep(": ", params, " + ", "", out),
                    "",
                    out,
                )?;
                write!(out, ";")?;
            }

            ItemEnum::Impl(Impl { is_unsafe, generics, trait_, for_, negative, .. }) => {
                if *is_unsafe {
                    write!(out, "unsafe ")?;
                }
                write!(out, "impl")?;
                print_generics(
                    generics,
                    |out| {
                        maybe_print(negative.pick(" !", " "), trait_, " for", out)?;
                        write!(out, " ")?;
                        for_.print(out)?;
                        OK
                    },
                    ",\n",
                    out,
                )?;
                write!(out, "{}{{ ... }}", generics.where_predicates.is_empty().pick(" ", ""))?;
            }

            ItemEnum::TypeAlias(TypeAlias { type_, generics }) => {
                let name = self.name.as_ref().context("no type alias name")?;
                write!(out, "type {name}")?;
                print_generics(generics, |out| {write!(out, " = ")?; type_.print(out)}, "", out)?;
                write!(out, ";")?;
            }

            ItemEnum::OpaqueTy(_) => todo!(),

            ItemEnum::Constant(r#const) => {
                let name = self.name.as_ref().context("no constant name")?;
                write!(out, "const {name}: ")?;
                r#const.type_.print(out)?;
                write!(out, " = ")?;
                r#const.print(out)?;
            }

            ItemEnum::Static(Static { type_, mutable, .. }) => {
                let name = self.name.as_ref().context("no static item name")?;
                write!(out, "static {} {name}: ", mutable.pick("mut", ""))?;
                type_.print(out)?;
                write!(out, " = ...;")?;
            }

            ItemEnum::ForeignType => {
                let name = self.name.as_ref().context("no foreign type name")?;
                write!(out, "extern type {name};")?;
            }

            ItemEnum::Macro(name) => write!(out, "macro_rules! {name}")?,

            ItemEnum::ProcMacro(ProcMacro { kind, helpers }) => {
                let name = self.name.as_ref().context("no proc macro name")?;
                match kind {
                    MacroKind::Bang => {
                        writeln!(out, "#[proc_macro]")?;
                        write!(out, "pub fn {name}(tokens: TokenStream) -> TokenStream {{ ... }}")?;
                    }

                    MacroKind::Attr => {
                        writeln!(out, "#[proc_macro_attribute]")?;
                        write!(out, "pub fn {name}(item: TokenStream, attr: TokenStream) \
                                     -> TokenStream {{ ... }}")?;
                    }

                    MacroKind::Derive => {
                        write!(out, "#[proc_macro_derive({name}]")?;
                        if let [first, rest @ ..] = &**helpers {
                            write!(out, ", attributes({first}")?;
                            for helper in rest {
                                write!(out, ", {helper}")?;
                            }
                        }
                        writeln!(out, ")]")?;
                        write!(out, "pub fn f(item: TokenStream) -> TokenStream {{ ... }}")?;
                    }
                }
            }

            ItemEnum::Primitive(_) => todo!(),

            ItemEnum::AssocConst { type_, default } => {
                let name = self.name.as_ref().context("no associated constant name")?;
                write!(out, "const {name}: ")?;
                type_.print(out)?;
                if default.is_some() {
                    write!(out, " = ...")?;
                }
                write!(out, ";")?;
            }

            ItemEnum::AssocType { generics, bounds, default } => {
                let name = self.name.as_ref().context("no associated type name")?;
                write!(out, "type {name}")?;
                print_generics(
                    generics,
                    |out| {
                        print_with_sep(": ", bounds, " + ", "", out)?;
                        maybe_print(" = ", default, "", out)?;
                        OK
                    },
                    "",
                    out,
                )?;
                write!(out, ";")?;
            }
        }

        write!(out, "\n{}", self.docs.as_deref().unwrap_or(""))?;
        OK
    }
}
