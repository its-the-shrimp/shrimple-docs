use {
    std::{fmt::{Display, Write}, mem::take},
    anyhow::{bail, Context},
    rustdoc_types::{
        Abi,
        Constant,
		DynTrait,
		Enum,
		FnDecl,
		Function,
		FunctionPointer,
		GenericArgs,
		GenericBound,
		GenericParamDef,
		GenericParamDefKind,
		Generics,
		Header,
		Import,
		Item,
		ItemEnum,
		MacroKind,
		Path,
		PolyTrait,
		Primitive,
		ProcMacro,
		Static,
		Struct,
		StructKind,
		Trait,
		TraitAlias,
		TraitBoundModifier,
		Type,
		TypeAlias,
		TypeBinding,
		TypeBindingKind,
		Union,
		VariantKind,
		Visibility,
		WherePredicate,
    },
    crate::{
        docs::{Docs, Infer},
        item_visitor::{visit_type, Visitor},
        utils::{BoolExt, Result, OK, BOLD, NOSTYLE},
    },
};

struct Formatter<W>(W);

impl<W: Write> Formatter<W> {
    fn maybe_print<'item, T: 'item>(
        &mut self,
        method: impl Fn(&mut Self, &'item T) -> Result,
        prefix: impl Display,
        item: &'item Option<T>,
        postfix: impl Display,
    ) -> Result {
        if let Some(item) = item {
            write!(self.0, "{prefix}")?;
            method(self, item)?;
            write!(self.0, "{postfix}")?;
        }
        OK
    }

    fn print_with_sep<'item, T: 'item>(
        &mut self,
        method: impl Fn(&mut Self, &'item T) -> Result,
        prefix: impl Display,
        items: impl IntoIterator<Item = &'item T>,
        sep: impl Display,
        postfix: impl Display,
    )
        -> Result
    {
        let mut items = items.into_iter();
        let Some(first) = items.next() else { return OK };
        write!(self.0, "{prefix}")?;
        method(self, first)?;
        for item in items {
            write!(self.0, "{sep}")?;
            method(self, item)?;
        }
        write!(self.0, "{postfix}")?;
        OK
    }

    fn print_generics<E: Into<anyhow::Error>>(
        &mut self,
        item: &Generics,
        postfix: impl Display,
        f: impl FnOnce(&mut Self) -> Result<(), E>,
    ) -> Result {
        let params = item.params.iter().filter(|i| match i.kind {
            GenericParamDefKind::Type { synthetic, .. } => !synthetic,
            _ => true,
        });
        self.print_with_sep(Self::visit_generic_param_def, "<", params, ", ", ">")?;
        f(self).map_err(E::into)?;
        self.print_with_sep(
            Self::visit_where_predicate,
            "\nwhere\n    ",
            &item.where_predicates,
            ",\n    ",
            postfix,
        )?;
        OK
    }
}

impl<W: Write> Visitor for Formatter<W> {
    fn visit_lifetime(&mut self, x: &String) -> Result {
        write!(self.0, "{x}").map_err(Into::into)
    }

    fn visit_infer(&mut self, _: &Infer) -> Result {
        write!(self.0, "_").map_err(Into::into)
    }

    fn visit_poly_trait(&mut self, x: &PolyTrait) -> Result {
        self.print_with_sep(Self::visit_generic_param_def, "for<", &x.generic_params, ", ", "> ")?;
        self.visit_path(&x.trait_)?;
        OK
    }

    fn visit_dyn_trait(&mut self, x: &DynTrait) -> Result {
        write!(self.0, "dyn ")?;
        self.print_with_sep(Self::visit_poly_trait, "", &x.traits, ", ", "")?;
        self.maybe_print(Self::visit_lifetime, " + ", &x.lifetime, "")?;
        OK
    }

    fn visit_header(&mut self, x: &Header) -> Result {
        if x.async_ {
            write!(self.0, "async ")?;
        }
        if x.const_ {
            write!(self.0, "const ")?;
        }
        if x.unsafe_ {
            write!(self.0, "unsafe ")?;
        }
        match &x.abi {
            Abi::Rust => {},
            Abi::C { .. } => write!(self.0, "extern \"C\" ")?,
            Abi::Other(name) => write!(self.0, "extern {name:?} ")?,
            _ => write!(self.0, "extern ... ")?,
        }
        OK
    }

    fn visit_fn_arg(&mut self, x: &(String, Type)) -> Result {
        if !x.0.is_empty() {
            write!(self.0, "{}: ", x.0)?;
        }
        self.visit_type(&x.1)?;
        OK
    }

    fn visit_fn_decl(&mut self, x: &FnDecl) -> Result {
        write!(self.0, "(")?;
        self.print_with_sep(Self::visit_fn_arg, "", &x.inputs, ", ", "")?;
        if x.c_variadic {
            write!(self.0, "{}...", x.inputs.is_empty().pick("", ", "))?;
        }
        write!(self.0, ")")?;
        self.maybe_print(Self::visit_type, " -> ", &x.output, "")?;
        OK
    }

    fn visit_function_pointer(&mut self, x: &FunctionPointer) -> Result {
        self.print_with_sep(Self::visit_generic_param_def, "for<", &x.generic_params, ", ", "> ")?;
        self.visit_header(&x.header)?;
        self.visit_fn_decl(&x.decl)?;
        OK
    }

    fn visit_type(&mut self, x: &Type) -> Result {
        match x {
            Type::ResolvedPath(_) | Type::DynTrait(_) | Type::FunctionPointer(_) => {
                visit_type(self, x)?;
            }

            Type::Primitive(t) if t == "never" => self.0.write_char('!')?,
            Type::Generic(t) | Type::Primitive(t) => write!(self.0, "{t}")?,

            Type::Tuple(types) => {
                write!(self.0, "(")?;
                self.print_with_sep(Self::visit_type, "", types, ", ", "")?;
                write!(self.0, "{})", (types.len() == 1).pick(",", ""))?;
            }

            Type::Slice(t) => {
                write!(self.0, "[")?;
                self.visit_type(t)?;
                write!(self.0, "]")?;
            }

            Type::Array { type_, len } => {
                write!(self.0, "[")?;
                self.visit_type(type_)?;
                write!(self.0, "; {len}]")?;
            }

            Type::Pat { type_, .. } => {
                self.visit_type(type_)?;
                write!(self.0, " is ...")?;
            }

            Type::ImplTrait(r#impl) => {
                self.print_with_sep(Self::visit_generic_bound, "impl ", r#impl, " + ", "")?;
            }

            Type::Infer => write!(self.0, "_")?,

            Type::RawPointer { mutable, type_ } => {
                write!(self.0, "*{} ", mutable.pick("mut", "const"))?;
                self.visit_type(type_)?;
            }

            Type::BorrowedRef { lifetime, mutable, type_ } => {
                write!(self.0, "&")?;
                if let Some(lifetime) = lifetime {
                    write!(self.0, "{lifetime} ")?;
                }
                if *mutable {
                    write!(self.0, "mut ")?;
                }
                self.visit_type(type_)?;
            }

            Type::QualifiedPath { name, args, self_type, trait_ } => {
                let angles_needed = trait_.is_some()
                    || !matches!(&**self_type, Type::QualifiedPath {..} | Type::ResolvedPath(..));
                let [open, close] = angles_needed.pick(["<", ">"], ["", ""]);
                write!(self.0, "{open}")?;
                self.visit_type(self_type)?;
                self.maybe_print(Self::visit_path, " as ", trait_, "")?;
                write!(self.0, "{close}::{name}")?;
                self.visit_generic_args(args)?;
            }
        }
        OK
    }

    fn visit_constant(&mut self, x: &Constant) -> Result {
        let to_print = x.value.as_deref()
            .unwrap_or_else(|| x.is_literal.pick(&*x.expr, "const { ... }"));
        write!(self.0, "{to_print}")?;
        OK
    }

    fn visit_type_binding(&mut self, x: &TypeBinding) -> Result {
        write!(self.0, "{}", x.name)?;
        self.visit_generic_args(&x.args)?;
        match &x.binding {
            TypeBindingKind::Equality(term) => {
                write!(self.0, " = ")?;
                self.visit_term(term)
            }
            TypeBindingKind::Constraint(bounds) => {
                self.print_with_sep(Self::visit_generic_bound, ": ", bounds, " + ", "")
            }
        }
    }

    fn visit_generic_args(&mut self, x: &GenericArgs) -> Result {
        match x {
            GenericArgs::AngleBracketed { args, bindings } => {
                self.print_with_sep(Self::visit_generic_arg, "<", args, ", ", "")?;
                self.print_with_sep(
                    Self::visit_type_binding,
                    args.is_empty().pick("<", ", "),
                    bindings,
                    ", ",
                    "",
                )?;
                if !bindings.is_empty() || !args.is_empty() {
                    write!(self.0, ">")?;
                }
                OK
            }

            GenericArgs::Parenthesized { inputs, output } => {
                write!(self.0, "(")?;
                self.print_with_sep(Self::visit_type, "", inputs, ", ", "")?;
                write!(self.0, ")")?;
                self.maybe_print(Self::visit_type, " -> ", output, "")
            }
        }
    }

    fn visit_path(&mut self, x: &Path) -> Result {
        write!(self.0, "{}", x.name)?;
        self.maybe_print(|v, x| v.visit_generic_args(x), "", &x.args, "")?;
        OK
    }

    fn visit_generic_bound(&mut self, x: &GenericBound) -> Result {
        match x {
            GenericBound::TraitBound { trait_, generic_params, modifier } => {
                self.print_with_sep(
                    Self::visit_generic_param_def,
                    "for<", generic_params, ", ", "> ",
                )?;
                match modifier {
                    TraitBoundModifier::None => {}
                    TraitBoundModifier::Maybe => write!(self.0, "?")?,
                    TraitBoundModifier::MaybeConst => write!(self.0, "~const ")?,
                }
                self.visit_path(trait_)?;
            }
            GenericBound::Outlives(lifetime) => self.visit_lifetime(lifetime)?,
        }
        OK
    }

    fn visit_generic_param_def(&mut self, x: &GenericParamDef) -> Result {
        match &x.kind {
            GenericParamDefKind::Lifetime { outlives } => {
                self.visit_lifetime(&x.name)?;
                self.print_with_sep(Self::visit_lifetime, ": ", outlives, " + ", "")?;
            }
            GenericParamDefKind::Type { bounds, default, .. } => {
                write!(self.0, "{}", x.name)?;
                self.print_with_sep(Self::visit_generic_bound, ": ", bounds, " + ", "")?;
                self.maybe_print(Self::visit_type, " = ", default, "")?;
            }
            GenericParamDefKind::Const { type_, default } => {
                write!(self.0, "const {}: ", x.name)?;
                self.visit_type(type_)?;
                if default.is_some() {
                    write!(self.0, " = ...")?;
                }
            }
        }
        OK
    }

    fn visit_where_predicate(&mut self, x: &WherePredicate) -> Result {
        match x {
            WherePredicate::BoundPredicate { type_, bounds, generic_params } => {
                self.print_with_sep(
                    Self::visit_generic_param_def,
                    "for<", generic_params, ", ", "> ",
                )?;
                self.visit_type(type_)?;
                self.print_with_sep(Self::visit_generic_bound, ": ", bounds, " + ", "")?;
            }

            WherePredicate::RegionPredicate { lifetime, bounds } => {
                self.visit_lifetime(lifetime)?;
                self.print_with_sep(Self::visit_generic_bound, ": ", bounds, " + ", "")?;
            }

            WherePredicate::EqPredicate { lhs, rhs } => {
                self.visit_type(lhs)?;
                write!(self.0, " = ")?;
                self.visit_term(rhs)?;
            }
        }
        OK
    }

    fn visit_generics(&mut self, x: &Generics) -> Result {
        self.print_generics(x, ",\n", |_| OK)
    }

    fn visit_visibility(&mut self, x: &Visibility) -> Result {
        match x {
            Visibility::Public => write!(self.0, "pub ")?,
            Visibility::Default => {}
            Visibility::Crate => write!(self.0, "pub(crate) ")?,
            Visibility::Restricted { path, .. } => if !path.starts_with("::") {
                write!(self.0, "pub(in {path}) ")?;
            }
        }
        OK
    }
}

#[allow(clippy::too_many_lines)]
pub fn print_item(item: &Item, docs: &Docs, out: &mut impl Write) -> Result {
    let mut fmt = Formatter(out);

    match &item.inner {
        ItemEnum::Impl(_) | ItemEnum::Variant(_) | ItemEnum::StructField(_) => {}

        ItemEnum::Module(_) => {
            let name = item.name.as_ref().context("no module name")?;
            write!(fmt.0, "mod {name};")?;
        }

        ItemEnum::ExternCrate { name, rename } => {
            write!(fmt.0, "extern crate {name}")?;
            if let Some(rename) = rename {
                write!(fmt.0, "as {rename};")?;
            }
            write!(fmt.0, ";")?;
        }

        ItemEnum::Import(Import { source, name, glob, .. }) => {
            write!(fmt.0, "use {source}")?;
            if *glob {
                write!(fmt.0, "::*")?;
            }
            if !source.ends_with(name) {
                write!(fmt.0, " as {name}")?;
            }
            write!(fmt.0, ";")?;
        }

        ItemEnum::Union(Union { generics, fields, fields_stripped, .. }) => {
            let name = item.name.as_ref().context("no union name")?;
            write!(fmt.0, "union {name}")?;
            fmt.visit_generics(generics)?;
            write!(fmt.0, "{}{{", generics.where_predicates.is_empty().pick(" ", ""))?;
            if !fields.is_empty() || *fields_stripped {
                writeln!(fmt.0)?;
            }
            for field_id in fields {
                let Some(field) = docs.index.get(field_id) else {
                    bail!("no union field found");
                };
                let Some(field_name) = &field.name else {
                    bail!("no union field name found");
                };
                let ItemEnum::StructField(field_ty) = &field.inner else {
                    bail!("no union field type found");
                };
                write!(fmt.0, "    ")?;
                fmt.visit_visibility(&field.visibility)?;
                write!(fmt.0, "{field_name}: ")?;
                fmt.visit_type(field_ty)?;
                writeln!(fmt.0, ",")?;
            }
            if *fields_stripped {
                writeln!(fmt.0, "    ...")?;
            }
            write!(fmt.0, "}}")?;
        }

        ItemEnum::Struct(Struct { kind, generics, .. }) => {
            let name = item.name.as_ref().context("no struct name")?;
            write!(fmt.0, "struct {name}")?;
            match kind {
                StructKind::Unit => write!(fmt.0, ";")?,

                StructKind::Tuple(fields) => {
                    fmt.print_generics(generics, "", |fmt| {
                        write!(fmt.0, "(")?;
                        let [mut fields_stripped, mut first] = [false, true];
                        for field_id in fields {
                            let Some(field_id) = field_id else {
                                fields_stripped = true;
                                continue;
                            };
                            let Some(field) = docs.index.get(field_id) else {
                                bail!("no struct field found");
                            };
                            let ItemEnum::StructField(field_ty) = &field.inner else {
                                bail!("no struct field type found");
                            };
                            if !take(&mut first) {
                                write!(fmt.0, ", ")?;
                            }
                            fmt.visit_visibility(&field.visibility)?;
                            fmt.visit_type(field_ty)?;
                        }
                        if fields_stripped {
                            write!(fmt.0, "{}...", first.pick("", ", "))?;
                        }
                        write!(fmt.0, ");")?;
                        OK
                    })?;
                }

                StructKind::Plain { fields, fields_stripped } => {
                    fmt.visit_generics(generics)?;
                    write!(fmt.0, "{}{{", generics.where_predicates.is_empty().pick(" ", ""))?;
                    if !fields.is_empty() || *fields_stripped {
                        writeln!(fmt.0)?;
                    }
                    for field_id in fields {
                        let Some(field) = docs.index.get(field_id) else {
                            bail!("no struct field found");
                        };
                        let Some(field_name) = &field.name else {
                            bail!("no struct field name found");
                        };
                        let ItemEnum::StructField(field_ty) = &field.inner else {
                            bail!("no struct field type found");
                        };
                        write!(fmt.0, "    ")?;
                        fmt.visit_visibility(&field.visibility)?;
                        write!(fmt.0, "{field_name}: ")?;
                        fmt.visit_type(field_ty)?;
                        writeln!(fmt.0, ",")?;
                    }
                    if *fields_stripped {
                        writeln!(fmt.0, "    ...")?;
                    }
                    write!(fmt.0, "}}")?;
                }
            }
        }

        ItemEnum::Enum(Enum { generics, variants, variants_stripped, .. }) => {
            let name = item.name.as_ref().context("no enum name")?;
            write!(fmt.0, "enum {name}")?;
            fmt.visit_generics(generics)?;
            write!(fmt.0, "{}{{", generics.where_predicates.is_empty().pick(" ", ""))?;
            if !variants.is_empty() || *variants_stripped {
                writeln!(fmt.0)?;
            }

            for variant_id in variants {
                let Some(variant) = docs.index.get(variant_id) else {
                    bail!("no enum variant found");
                };
                let Some(variant_name) = &variant.name else {
                    bail!("no enum variant name found");
                };
                let ItemEnum::Variant(variant) = &variant.inner else {
                    bail!("enum variant is not an enum variant");
                };
                write!(fmt.0, "    ")?;
                write!(fmt.0, "{variant_name}")?;
                match &variant.kind {
                    VariantKind::Plain => {}

                    VariantKind::Tuple(fields) => {
                        write!(fmt.0, "(")?;
                        let [mut fields_stripped, mut first] = [false, true];
                        for field_id in fields {
                            let Some(field_id) = field_id else {
                                fields_stripped = true;
                                continue;
                            };
                            let Some(field) = docs.index.get(field_id) else {
                                bail!("no enum variant field found");
                            };
                            let ItemEnum::StructField(field_ty) = &field.inner else {
                                bail!("no enum variant field type found");
                            };
                            if !take(&mut first) {
                                write!(fmt.0, ", ")?;
                            }
                            fmt.visit_visibility(&field.visibility)?;
                            fmt.visit_type(field_ty)?;
                        }
                        if fields_stripped {
                            write!(fmt.0, "{}...", first.pick("", ", "))?;
                        }
                        write!(fmt.0, ")")?;
                    }

                    VariantKind::Struct { fields, fields_stripped } => {
                        write!(fmt.0, " {{")?;
                        if !fields.is_empty() || *fields_stripped {
                            writeln!(fmt.0)?;
                        }
                        for field_id in fields {
                            let Some(field) = docs.index.get(field_id) else {
                                bail!("no enum variant field found");
                            };
                            let Some(field_name) = &field.name else {
                                bail!("no enum variant field name found");
                            };
                            let ItemEnum::StructField(field_ty) = &field.inner else {
                                bail!("no enum variant field type found");
                            };
                            write!(fmt.0, "        ")?;
                            fmt.visit_visibility(&field.visibility)?;
                            write!(fmt.0, "{field_name}: ")?;
                            fmt.visit_type(field_ty)?;
                            writeln!(fmt.0, ",")?;
                        }
                        if *fields_stripped {
                            writeln!(fmt.0, "        ...")?;
                        }
                        write!(fmt.0, "    }}")?;
                    }
                }
                if let Some(discriminant) = &variant.discriminant {
                    write!(fmt.0, " = {}", discriminant.value)?;
                }
                writeln!(fmt.0, ",")?;
            }

            if *variants_stripped {
                writeln!(fmt.0, "    ...")?;
            }
            write!(fmt.0, "}}")?;
        }

        ItemEnum::Function(Function { decl, generics, header, has_body }) => {
            let name = item.name.as_ref().context("no function name")?;
            fmt.visit_header(header)?;
            write!(fmt.0, "fn {name}")?;
            let [postfix, end] = has_body.pick([",\n", " { ... }"], ["", ";"]);
            fmt.print_generics(generics, postfix, |fmt| fmt.visit_fn_decl(decl))?;
            write!(fmt.0, "{end}")?;
        }

        ItemEnum::Trait(Trait { is_auto, is_unsafe, generics, bounds, .. }) => {
            let name = item.name.as_ref().context("no trait name")?;
            if *is_auto {
                write!(fmt.0, "auto ")?;
            }
            if *is_unsafe {
                write!(fmt.0, "unsafe ")?;
            }
            write!(fmt.0, "trait {name}")?;
            fmt.print_generics(
                generics,
                ",\n",
                |fmt| fmt.print_with_sep(Formatter::visit_generic_bound, ": ", bounds, " + ", ""),
            )?;
            write!(fmt.0, "{}{{ ... }}", generics.where_predicates.is_empty().pick(" ", ""))?;
        }

        ItemEnum::TraitAlias(TraitAlias { generics, params }) => {
            let name = item.name.as_ref().context("no trait alias name")?;
            write!(fmt.0, "trait {name}")?;
            fmt.print_generics(
                generics,
                "",
                |fmt| fmt.print_with_sep(Formatter::visit_generic_bound, ": ", params, " + ", ""),
            )?;
            write!(fmt.0, ";")?;
        }

        ItemEnum::TypeAlias(TypeAlias { type_, generics }) => {
            let name = item.name.as_ref().context("no type alias name")?;
            write!(fmt.0, "type {name}")?;
            fmt.print_generics(generics, "", |fmt| {
                write!(fmt.0, " = ")?;
                fmt.visit_type(type_)
            })?;
            write!(fmt.0, ";")?;
        }

        ItemEnum::OpaqueTy(_) => todo!(),

        ItemEnum::Constant { type_, const_ } => {
            let name = item.name.as_ref().context("no constant name")?;
            write!(fmt.0, "const {name}: ")?;
            fmt.visit_type(type_)?;
            write!(fmt.0, " = ")?;
            fmt.visit_constant(const_)?;
        }

        ItemEnum::Static(Static { type_, mutable, .. }) => {
            let name = item.name.as_ref().context("no static item name")?;
            write!(fmt.0, "static {} {name}: ", mutable.pick("mut", ""))?;
            fmt.visit_type(type_)?;
            write!(fmt.0, " = ...;")?;
        }

        ItemEnum::ForeignType => {
            let name = item.name.as_ref().context("no foreign type name")?;
            write!(fmt.0, "extern type {name};")?;
        }

        ItemEnum::Macro(body) => write!(fmt.0, "{body}")?,

        ItemEnum::ProcMacro(ProcMacro { kind, helpers }) => {
            let name = item.name.as_ref().context("no proc macro name")?;
            match kind {
                MacroKind::Bang => {
                    writeln!(fmt.0, "#[proc_macro]")?;
                    write!(fmt.0, "pub fn {name}(tokens: TokenStream) -> TokenStream {{ ... }}")?;
                }

                MacroKind::Attr => {
                    writeln!(fmt.0, "#[proc_macro_attribute]")?;
                    write!(fmt.0, "pub fn {name}(item: TokenStream, attr: TokenStream) \
                                 -> TokenStream {{ ... }}")?;
                }

                MacroKind::Derive => {
                    write!(fmt.0, "#[proc_macro_derive({name}]")?;
                    if let [first, rest @ ..] = &**helpers {
                        write!(fmt.0, ", attributes({first}")?;
                        for helper in rest {
                            write!(fmt.0, ", {helper}")?;
                        }
                    }
                    writeln!(fmt.0, ")]")?;
                    write!(fmt.0, "pub fn f(item: TokenStream) -> TokenStream {{ ... }}")?;
                }
            }
        }

        ItemEnum::Primitive(Primitive { name, .. }) => write!(fmt.0, "{BOLD}{name}{NOSTYLE}")?,

        ItemEnum::AssocConst { type_, default } => {
            let name = item.name.as_ref().context("no associated constant name")?;
            write!(fmt.0, "const {name}: ")?;
            fmt.visit_type(type_)?;
            if default.is_some() {
                write!(fmt.0, " = ...")?;
            }
            write!(fmt.0, ";")?;
        }

        ItemEnum::AssocType { generics, bounds, default } => {
            let name = item.name.as_ref().context("no associated type name")?;
            write!(fmt.0, "type {name}")?;
            fmt.print_generics(
                generics,
                "",
                |fmt| {
                    fmt.print_with_sep(Formatter::visit_generic_bound, ": ", bounds, " + ", "")?;
                    fmt.maybe_print(Formatter::visit_type, " = ", default, "")?;
                    OK
                }
            )?;
            write!(fmt.0, ";")?;
        }
    }

    write!(fmt.0, "\n{}", item.docs.as_deref().unwrap_or(""))?;
    OK
}
