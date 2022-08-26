//! ZIR - Zinc-IR

use crate::util::index::StringInterningVec;
pub use api::*;

mod api {
    use crate::util::index::{self, IndexVec, InterningIndexVec, StringInterningVec, StringSymbol};
    use std::num::NonZeroU8;

    index::define_idx! { pub struct TyId: u32 }
    index::define_idx! { pub struct TyFuncId: u32 }

    index::define_idx! { pub struct FuncId: u32 }
    index::define_idx! { pub struct BlockId: u32 }
    index::define_idx! { pub struct BlockLocalInstId: u32 }

    #[derive(Debug, Default)]
    pub struct Context {
        strings: StringInterningVec,
        blocks: IndexVec<BlockId, Block>,
        funcs: IndexVec<FuncId, Func>,

        tys: InterningIndexVec<TyId, Ty>,
        ty_funcs: InterningIndexVec<TyFuncId, TyFunc>,
    }

    impl Context {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn with_strings(strings: StringInterningVec) -> Self {
            Self {
                strings,
                ..Self::default()
            }
        }

        pub fn get_inst(&self, id: InstId) -> &Inst {
            &self.blocks[id.parent].insts[id.local]
        }

        pub fn get_inst_ty(&self, id: InstId) -> &TyId {
            &self.blocks[id.parent].inst_tys[id.local]
        }

        pub fn new_block(&mut self, label: StringSymbol, func: FuncId) -> BlockId {
            let blk = self.blocks.push(Block {
                label,
                func,
                insts: Default::default(),
                inst_tys: Default::default(),
            });
            self.funcs.get_mut(func).unwrap().blocks.push(blk);
            blk
        }

        pub fn get_str(&mut self, str: &str) -> StringSymbol {
            if self.strings.is_str_interned(str) {
                self.strings.get_from_str_value(str).unwrap()
            } else {
                self.strings.intern(str.to_string())
            }
        }

        pub fn get_string(&mut self, string: String) -> StringSymbol {
            self.strings.get_or_intern(string)
        }

        pub fn get_ty(&mut self, ty: Ty) -> TyId {
            self.tys.get_or_intern(ty)
        }

        pub fn get_ty_func(&mut self, ty: TyFunc) -> TyFuncId {
            self.ty_funcs.get_or_intern(ty)
        }

        pub fn new_func(&mut self, name: StringSymbol, ty: TyFuncId) -> FuncId {
            self.funcs.push(Func::new(name, ty))
        }

        pub fn strings(&self) -> &StringInterningVec {
            &self.strings
        }

        pub fn blocks(&self) -> &IndexVec<BlockId, Block> {
            &self.blocks
        }

        pub fn funcs(&self) -> &IndexVec<FuncId, Func> {
            &self.funcs
        }

        pub fn tys(&self) -> &InterningIndexVec<TyId, Ty> {
            &self.tys
        }

        pub fn ty_funcs(&self) -> &InterningIndexVec<TyFuncId, TyFunc> {
            &self.ty_funcs
        }
    }

    #[derive(Debug, Clone)]
    pub struct Func {
        name: StringSymbol,
        ty: TyFuncId,
        blocks: Vec<BlockId>,
    }

    impl Func {
        pub fn new(name: StringSymbol, ty: TyFuncId) -> Self {
            Self {
                name,
                ty,
                blocks: vec![],
            }
        }

        pub fn name(&self) -> StringSymbol {
            self.name
        }

        pub fn ty(&self) -> TyFuncId {
            self.ty
        }

        pub fn blocks(&self) -> &[BlockId] {
            self.blocks.as_ref()
        }
    }

    #[derive(Debug)]
    pub struct Block {
        label: StringSymbol,
        func: FuncId,

        insts: IndexVec<BlockLocalInstId, Inst>,
        inst_tys: IndexVec<BlockLocalInstId, TyId>,
    }

    impl Block {
        pub fn label(&self) -> StringSymbol {
            self.label
        }

        pub fn insts(&self) -> &IndexVec<BlockLocalInstId, Inst> {
            &self.insts
        }

        pub fn inst_tys(&self) -> &IndexVec<BlockLocalInstId, TyId> {
            &self.inst_tys
        }
    }

    #[derive(Debug)]
    pub enum Inst {
        Arg(u8),
        Constant(ConstantValue),
        Add(InstId, InstId),
        Call(InstId, Vec<InstId>),
        Ret(InstId),
        RetVoid,
    }

    #[derive(Debug)]
    pub enum ConstantValue {
        Void,
        Integer(u64),
        String(StringSymbol),
        Func(FuncId),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Ty {
        Void,
        Int(TyInt),
        Func(TyFuncId),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct TyInt {
        pub signed: bool,
        pub size: Option<NonZeroU8>, // None -> ptr sized
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct TyFunc {
        pub params: Vec<TyId>,
        pub ret: TyId,
    }

    /// Reference to block, and an inst local to that block
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct InstId {
        parent: BlockId,
        local: BlockLocalInstId,
    }

    impl InstId {
        pub fn new(parent: BlockId, local: BlockLocalInstId) -> Self {
            Self { parent, local }
        }
    }

    impl std::fmt::Display for InstId {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "%{}.{}", self.parent.0, self.local.0)
        }
    }

    #[derive(Debug)]
    pub struct InstBuilder {
        blk: BlockId,
    }

    impl InstBuilder {
        pub fn new(blk: BlockId) -> Self {
            Self { blk }
        }

        pub fn set_target(&mut self, blk: BlockId) {
            self.blk = blk;
        }

        fn mk_inst_id(&self, local: BlockLocalInstId) -> InstId {
            InstId {
                parent: self.blk,
                local,
            }
        }

        pub fn build_arg(&self, ctx: &mut Context, n: u8) -> InstId {
            let blk = ctx.blocks.get_mut(self.blk).unwrap();
            let func = blk.func;
            let func_ty = ctx.funcs[func].ty;
            let func_ty = &ctx.ty_funcs[func_ty];
            let param_ty = func_ty.params[n as usize];

            let local = blk.insts.push(Inst::Arg(n));
            blk.inst_tys.push(param_ty);

            self.mk_inst_id(local)
        }

        pub fn build_ret_void(&self, ctx: &mut Context) -> InstId {
            let ty_void = ctx.tys.get_or_intern(Ty::Void);
            let blk = ctx.blocks.get_mut(self.blk).unwrap();

            let local = blk.insts.push(Inst::RetVoid);
            blk.inst_tys.push(ty_void);

            self.mk_inst_id(local)
        }

        pub fn build_ret(&self, ctx: &mut Context, id: InstId) -> InstId {
            let ty = *ctx.get_inst_ty(id);
            let blk = ctx.blocks.get_mut(self.blk).unwrap();

            let local = blk.insts.push(Inst::Ret(id));
            blk.inst_tys.push(ty);

            self.mk_inst_id(local)
        }

        pub fn build_add(&self, ctx: &mut Context, lhs: InstId, rhs: InstId) -> InstId {
            let lhs_ty = *ctx.get_inst_ty(lhs);
            let rhs_ty = *ctx.get_inst_ty(rhs);
            assert_eq!(lhs_ty, rhs_ty);

            let blk = ctx.blocks.get_mut(self.blk).unwrap();

            let local = blk.insts.push(Inst::Add(lhs, rhs));
            blk.inst_tys.push(lhs_ty);

            self.mk_inst_id(local)
        }

        pub fn build_const(&self, ctx: &mut Context, ty: TyId, value: ConstantValue) -> InstId {
            let blk = ctx.blocks.get_mut(self.blk).unwrap();

            let local = blk.insts.push(Inst::Constant(value));
            blk.inst_tys.push(ty);

            self.mk_inst_id(local)
        }
    }
}

pub mod codegen {
    use std::collections::HashMap;

    use crate::zir;
    use llvm;

    pub fn codegen(
        ctx: &zir::Context,
    ) -> (
        llvm::Context,
        llvm::Module,
        Vec<llvm::Function>,
        Vec<llvm::BasicBlock>,
    ) {
        let mut codegen = CodeGen::new(ctx);
        codegen.codegen();
        (
            codegen.llvm_ctx,
            codegen.llvm_mod,
            codegen.llvm_funcs,
            codegen.llvm_blocks,
        )
    }

    pub struct CodeGen<'ctx> {
        ctx: &'ctx zir::Context,
        llvm_ctx: llvm::Context,
        llvm_mod: llvm::Module,
        // llvm_builder: llvm::IRBuilder,
        //
        llvm_funcs: Vec<llvm::Function>,
        llvm_blocks: Vec<llvm::BasicBlock>,

        ty_cache: HashMap<zir::TyId, llvm::Type>,
        ty_func_cache: HashMap<zir::TyFuncId, llvm::FunctionType>,
    }

    impl<'ctx> CodeGen<'ctx> {
        pub fn new(ctx: &'ctx zir::Context) -> Self {
            let llvm_ctx = llvm::Context::new();
            Self {
                ctx,
                llvm_mod: llvm::Module::new("MainModule", Some(&llvm_ctx)),
                // llvm_builder: llvm::IRBuilder::new(&llvm_ctx),
                llvm_ctx,

                llvm_funcs: Default::default(),
                llvm_blocks: Default::default(),

                ty_cache: Default::default(),
                ty_func_cache: Default::default(),
            }
        }

        pub fn codegen(&mut self) {
            for func in self.ctx.funcs().raw().iter() {
                self.gen_func(func);
            }
        }

        fn get_llvm_type(&mut self, id: zir::TyId) -> llvm::Type {
            if let Some(ty) = self.ty_cache.get(&id) {
                return *ty;
            }

            let zir_ty = &self.ctx.tys()[id];
            let llvm_ty = match zir_ty {
                zir::Ty::Void => llvm::Type::get_void_ty(&self.llvm_ctx),
                zir::Ty::Int(ty_int) => {
                    // @FIXME: Use actual target ptr size
                    let size = ty_int.size.map_or(64, |u8| u8.get() as u32);
                    llvm::IntegerType::get(&self.llvm_ctx, size).into()
                }
                zir::Ty::Func(id) => self.get_llvm_func_type(*id).into(),
            };
            self.ty_cache.insert(id, llvm_ty);
            llvm_ty
        }

        fn get_llvm_func_type(&mut self, id: zir::TyFuncId) -> llvm::FunctionType {
            if let Some(ty) = self.ty_func_cache.get(&id) {
                return *ty;
            }

            let func = &self.ctx.ty_funcs()[id];
            let params = func
                .params
                .iter()
                .map(|&id| self.get_llvm_type(id))
                .collect::<Vec<_>>();
            let ret = self.get_llvm_type(func.ret);
            llvm::FunctionType::new(&ret, &params, false)
        }

        fn gen_func(&mut self, func: &zir::Func) {
            let func_ty = self.get_llvm_func_type(func.ty());
            let name = &self.ctx.strings()[func.name()];

            let llvm_func = llvm::Function::new(
                &func_ty,
                llvm::LinkageType::ExternalLinkage,
                0,
                name,
                Some(&self.llvm_mod),
            );

            let mut values_map: HashMap<zir::InstId, llvm::Value> = Default::default();

            let mut builder = llvm::IRBuilder::new(&self.llvm_ctx);

            // for blk_id in func.blocks
            func.blocks().iter().for_each(|&blk_id| {
                let blk = &self.ctx.blocks()[blk_id];

                let llvm_blk = llvm::BasicBlock::new(
                    &self.llvm_ctx,
                    self.ctx.strings().get(blk.label()).unwrap(),
                    &llvm_func,
                    None,
                );
                builder.set_insertion_point(&llvm_blk);

                // for local_inst in blk.insts
                blk.insts().indices().iter().for_each(|&local_inst| {
                    let inst = &blk.insts()[local_inst];

                    let value = match inst {
                        zir::Inst::Arg(i) => llvm_func.get_arg(*i as u32),

                        zir::Inst::Constant(k) => match k {
                            zir::ConstantValue::Void => {
                                let ty = self.get_llvm_type(blk.inst_tys()[local_inst]);
                                llvm::Constant::new_null_value(&ty).into()
                            }
                            zir::ConstantValue::Integer(int) => {
                                let ty = self.get_llvm_type(blk.inst_tys()[local_inst]);
                                llvm::Constant::new_integer_value(&ty.try_into().unwrap(), *int)
                                    .into()
                            }
                            zir::ConstantValue::String(_) => todo!(),
                            zir::ConstantValue::Func(_) => todo!(),
                        },

                        zir::Inst::Add(lhs, rhs) => {
                            let lhs = values_map.get(lhs).unwrap();
                            let rhs = values_map.get(rhs).unwrap();
                            builder.create_add(lhs, rhs, "tmpadd")
                        }

                        zir::Inst::Call(_, _) => todo!(),

                        zir::Inst::Ret(id) => {
                            let val = values_map.get(id).unwrap();
                            builder.create_ret(Some(val))
                        }

                        zir::Inst::RetVoid => builder.create_ret_void(),
                    };

                    values_map.insert(zir::InstId::new(blk_id, local_inst), value);
                });

                self.llvm_blocks.push(llvm_blk);
            });

            self.llvm_funcs.push(llvm_func);
        }
    }
}

pub mod print {
    use crate::util::{index::Idx, AutoIndentingWriter};
    use crate::zir;
    use std::io::{self, Write};

    pub fn dump<W: Write>(ctx: &zir::Context, writer: &mut W) -> io::Result<()> {
        ZirPrinter::new(ctx, writer).dump()
    }

    pub struct ZirPrinter<'s, W: Write> {
        ctx: &'s zir::Context,
        f: AutoIndentingWriter<'s, W>,
    }

    impl<'s, W: Write> ZirPrinter<'s, W> {
        pub fn new(ctx: &'s zir::Context, writer: &'s mut W) -> Self {
            Self {
                ctx,
                f: AutoIndentingWriter::new(writer, 4),
            }
        }

        pub fn dump(&mut self) -> io::Result<()> {
            for func in self.ctx.funcs().iter() {
                let name = &self.ctx.strings()[func.name()];
                write!(self.f, "\n{:?} :: ", name)?;
                self.write_ty_func(func.ty())?;

                writeln!(self.f, " {{")?;
                self.f.push_indent();

                for (blk_i, blk) in func.blocks().iter().map(|&id| (id, &self.ctx.blocks()[id])) {
                    let label = &self.ctx.strings()[blk.label()];
                    self.f.pop_indent();
                    writeln!(self.f, "#{} {}:", blk_i.index(), label)?;
                    self.f.push_indent();

                    for local_inst in blk.insts().indices() {
                        let inst = &blk.insts()[local_inst];

                        write!(self.f, "%{} : ", local_inst.index())?;
                        let ty = blk.inst_tys()[local_inst];
                        self.write_ty(ty)?;
                        write!(self.f, " = ")?;

                        match inst {
                            zir::Inst::Arg(n) => write!(self.f, "arg {}", n)?,
                            zir::Inst::Constant(val) => {
                                write!(self.f, "const ",)?;
                                match val {
                                    zir::ConstantValue::Void => write!(self.f, "void")?,
                                    zir::ConstantValue::Integer(int) => write!(self.f, "{}", int)?,
                                    zir::ConstantValue::String(str) => write!(self.f, "{:?}", str)?,
                                    zir::ConstantValue::Func(_) => write!(self.f, "fn")?,
                                }
                            }
                            zir::Inst::Add(lhs, rhs) => {
                                write!(self.f, "add {}, {}", lhs, rhs)?;
                            }
                            zir::Inst::Call(_, _) => todo!(),
                            zir::Inst::Ret(id) => {
                                write!(self.f, "ret {}", id)?;
                            }
                            zir::Inst::RetVoid => write!(self.f, "ret void")?,
                        }

                        writeln!(self.f)?;
                    }
                }

                self.f.pop_indent();
                writeln!(self.f, "}}")?;
            }

            self.f.flush()
        }

        fn write_ty(&mut self, id: zir::TyId) -> io::Result<()> {
            let ty = &self.ctx.tys()[id];

            match ty {
                zir::Ty::Void => write!(self.f, "void")?,
                zir::Ty::Int(ty_int) => write!(
                    self.f,
                    "{}{}",
                    if ty_int.signed { "s" } else { "u" },
                    ty_int
                        .size
                        .map_or("intptr".to_string(), |n| format!("{}", n))
                )?,
                zir::Ty::Func(id) => {
                    self.write_ty_func(*id)?;
                }
            }

            self.f.flush()
        }

        fn write_ty_func(&mut self, id: zir::TyFuncId) -> io::Result<()> {
            let func = &self.ctx.ty_funcs()[id];
            write!(self.f, "fn(")?;
            for (i, ty_id) in func.params.iter().enumerate() {
                self.write_ty(*ty_id)?;

                if i != func.params.len() - 1 {
                    write!(self.f, ", ")?;
                }
            }
            write!(self.f, "): ")?;
            self.write_ty(func.ret)?;
            self.f.flush()
        }
    }
}

pub fn gen(
    nr: &crate::nameres::NameResolutionMap,
    ty_map: &crate::typer::TypeMap,
    strings: StringInterningVec,
) -> Context {
    let mut gen = gen::ZirGen::new(nr, ty_map, strings);
    gen.gen();
    gen.ctx
}

mod gen {
    use fnv::FnvHashMap as HashMap;

    use crate::{nameres as nr, typer, util::index::StringInterningVec, zir};

    pub(super) struct ZirGen<'s> {
        nr: &'s nr::NameResolutionMap,
        ty_map: &'s typer::TypeMap,

        ty_cache: HashMap<typer::TyId, zir::TyId>,

        pub(super) ctx: zir::Context,
    }

    impl<'s> ZirGen<'s> {
        pub fn new(
            nr: &'s nr::NameResolutionMap,
            ty_map: &'s typer::TypeMap,
            strings: StringInterningVec,
        ) -> Self {
            Self {
                nr,
                ty_map,
                ty_cache: Default::default(),
                ctx: zir::Context::with_strings(strings),
            }
        }
    }

    impl ZirGen<'_> {
        pub fn gen(&mut self) {
            let root_scope = self.nr.root_scope();

            for id in root_scope.decls.indices() {
                let name = root_scope.decls_name_map.get_by_right(&id).unwrap();
                let kind = &self.nr.decls[&id];
                match kind {
                    nr::DeclKind::Func(id) => {
                        let body = self.nr.decl_funcs[*id].body;
                        let ty = self.ty_map.decl_func_tys[id];
                        let ty = self.get_ty_func(ty);

                        let func = self.ctx.new_func(*name, ty);

                        let blk_name = self.ctx.get_str("entry");
                        let blk = self.ctx.new_block(blk_name, func);
                        let builder = zir::InstBuilder::new(blk);

                        self.gen_expr(body, &builder);
                        // builder.build_ret_void(&mut self.ctx);
                    }
                }
            }
        }

        fn gen_expr(&mut self, expr_id: nr::ExprId, builder: &zir::InstBuilder) -> zir::InstId {
            let expr = &self.nr.exprs[expr_id];
            match &expr.kind {
                nr::ExprKind::Res(_) => todo!(),
                nr::ExprKind::Block(id) => {
                    let block = &self.nr.blocks[*id];
                    block
                        .stmts
                        .iter()
                        .for_each(|stmt| self.gen_stmt(*stmt, builder));

                    let ty = self.ctx.get_ty(zir::Ty::Void);
                    builder.build_const(&mut self.ctx, ty, zir::ConstantValue::Void)
                }
                nr::ExprKind::Literal(lit) => match lit {
                    nr::ExprLiteral::String(_) => todo!(),
                    nr::ExprLiteral::Integer(int) => {
                        let ty = self.ty_map.exprs[&expr_id];
                        let ty = self.get_ty(ty);
                        builder.build_const(&mut self.ctx, ty, zir::ConstantValue::Integer(*int))
                    }
                    nr::ExprLiteral::Float(_) => todo!(),
                    nr::ExprLiteral::Bool(_) => todo!(),
                },
                nr::ExprKind::Infix(infix) => {
                    let lhs = self.gen_expr(infix.lhs, builder);
                    let rhs = self.gen_expr(infix.rhs, builder);
                    builder.build_add(&mut self.ctx, lhs, rhs)
                }
                nr::ExprKind::Call(_) => todo!(),
                nr::ExprKind::Return(id) => {
                    if let Some(id) = id {
                        let inst = self.gen_expr(*id, builder);
                        builder.build_ret(&mut self.ctx, inst)
                    } else {
                        builder.build_ret_void(&mut self.ctx)
                    }
                }
            }
        }

        fn gen_stmt(&mut self, stmt_id: nr::StmtId, builder: &zir::InstBuilder) {
            let stmt = &self.nr.stmts[stmt_id];
            match &stmt.kind {
                nr::StmtKind::Let(_) => todo!(),
                nr::StmtKind::Expr(expr) => {
                    let _ = self.gen_expr(*expr, builder);
                }
                nr::StmtKind::Decl(_) => todo!(),
            }
        }

        fn get_ty(&mut self, ty_id: typer::TyId) -> zir::TyId {
            if let Some(ty) = self.ty_cache.get(&ty_id) {
                return *ty;
            }

            let ty = &self.ty_map.tys[ty_id];
            let ty = match ty {
                typer::Ty::Prim(prim) => match prim {
                    typer::TyPrim::Integer { signed, size } => zir::Ty::Int(zir::TyInt {
                        signed: *signed,
                        size: *size,
                    }),
                    typer::TyPrim::Bool => todo!(),
                    typer::TyPrim::Void => zir::Ty::Void,
                    typer::TyPrim::Never => todo!(),
                },
                typer::Ty::Func(id) => {
                    let id = self.get_ty_func(*id);
                    zir::Ty::Func(id)
                }
                typer::Ty::Slice(_) => todo!(),
                typer::Ty::Nullable(_) => todo!(),
            };
            let id = self.ctx.get_ty(ty);
            self.ty_cache.insert(ty_id, id);
            id
        }

        fn get_ty_func(&mut self, ty_id: typer::TyFuncId) -> zir::TyFuncId {
            let func = &self.ty_map.func_tys[ty_id];

            let params = func
                .params
                .iter()
                .map(|ty_id| self.get_ty(*ty_id))
                .collect();
            let ret = self.get_ty(func.ret);

            let func = zir::TyFunc { params, ret };
            self.ctx.get_ty_func(func)
        }
    }
}

pub mod test {
    use crate::zir;

    pub fn create_test_funcs() -> zir::Context {
        let mut ctx = zir::Context::new();

        // let ty_void = ctx.tys.get_or_intern(zir::Ty::Void);
        let ty_sintptr = ctx.get_ty(zir::Ty::Int(zir::TyInt {
            signed: true,
            size: None,
        }));

        {
            let func_name = ctx.get_str("three");
            let func_ty = ctx.get_ty_func(zir::TyFunc {
                params: vec![],
                ret: ty_sintptr,
            });
            let func_id = ctx.new_func(func_name, func_ty);

            let blk_name = ctx.get_str("entry");
            let blk = ctx.new_block(blk_name, func_id);

            {
                let builder = zir::InstBuilder::new(blk);
                let k1 = builder.build_const(&mut ctx, ty_sintptr, zir::ConstantValue::Integer(1));
                let k2 = builder.build_const(&mut ctx, ty_sintptr, zir::ConstantValue::Integer(2));
                let res = builder.build_add(&mut ctx, k1, k2);
                builder.build_ret(&mut ctx, res);
            }
        }

        {
            let func_name = ctx.get_str("sum");
            let func_ty = ctx.get_ty_func(zir::TyFunc {
                params: vec![ty_sintptr, ty_sintptr, ty_sintptr],
                ret: ty_sintptr,
            });
            let func_id = ctx.new_func(func_name, func_ty);

            let blk_name = ctx.get_str("");
            let blk = ctx.new_block(blk_name, func_id);

            {
                let builder = zir::InstBuilder::new(blk);
                let arg0 = builder.build_arg(&mut ctx, 0);
                let arg1 = builder.build_arg(&mut ctx, 1);
                let arg2 = builder.build_arg(&mut ctx, 2);
                let add0 = builder.build_add(&mut ctx, arg0, arg1);
                let add1 = builder.build_add(&mut ctx, add0, arg2);
                builder.build_ret(&mut ctx, add1);
            }
        }

        ctx
    }
}
