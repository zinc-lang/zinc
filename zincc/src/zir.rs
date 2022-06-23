use crate::{
    parse::ast::StrSym,
    util::index::{self, IndexVec, InterningIndexVec},
};

#[derive(Debug, Default)]
pub struct Context {
    strings: InterningIndexVec<String, StrSym>,
    tys: InterningIndexVec<Ty, TyId>,
    blocks: IndexVec<Block, BlockId>,
    funcs: IndexVec<Func, FuncId>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_inst(&self, id: InstId) -> &Inst {
        self.blocks
            .get(id.parent)
            .unwrap()
            .insts
            .get(id.local)
            .unwrap()
    }

    pub fn get_inst_ty(&self, id: InstId) -> &TyId {
        self.blocks
            .get(id.parent)
            .unwrap()
            .inst_tys
            .get(id.local)
            .unwrap()
    }

    pub fn new_block(&mut self, label: StrSym, func: FuncId) -> BlockId {
        let blk = self.blocks.push(Block {
            label,
            func,
            insts: Default::default(),
            inst_tys: Default::default(),
        });
        self.funcs.get_mut(func).unwrap().blocks.push(blk);
        blk
    }
}

#[derive(Debug, Clone)]
pub struct Func {
    name: StrSym,
    ty: TyId,
    blocks: Vec<BlockId>,
}

impl Func {
    pub fn new(name: StrSym, ty: TyId) -> Self {
        Self {
            name,
            ty,
            blocks: vec![],
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncId(u32);

impl index::Idx for FuncId {
    fn new(idx: usize) -> Self {
        Self(idx.try_into().unwrap())
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug)]
pub struct Block {
    label: StrSym,
    func: FuncId,

    insts: IndexVec<Inst, BlockLocalInstId>,
    // inst_nodes: Vec<NodeId>,
    inst_tys: IndexVec<TyId, BlockLocalInstId>,
    // ty_nodes: Vec<NodeId>,
    // names: Vec<Option<String>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(u32);

impl index::Idx for BlockId {
    fn new(idx: usize) -> Self {
        Self(idx.try_into().unwrap())
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

/// Reference to block, and an inst local to that block
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstId {
    parent: BlockId,
    local: BlockLocalInstId,
}

impl std::fmt::Display for InstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}.{}", self.parent.0, self.local.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockLocalInstId(u32);

impl index::Idx for BlockLocalInstId {
    fn new(idx: usize) -> Self {
        Self(idx.try_into().unwrap())
    }

    fn index(self) -> usize {
        self.0 as usize
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
    Integer(u64),
    String(StrSym),
    Func(FuncId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Void,
    Int(TyInt),
    Func(TyFunc),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyInt {
    signed: bool,
    size: TyIntSize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyIntSize {
    PtrSized,
    BitSized(u8),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyFunc {
    params: Vec<TyId>,
    ret: TyId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyId(u32);

impl index::Idx for TyId {
    fn new(idx: usize) -> Self {
        Self(idx.try_into().unwrap())
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

pub struct InstBuilder<'ctx> {
    ctx: &'ctx mut Context,
    blk: BlockId,
}

impl<'ctx> InstBuilder<'ctx> {
    pub fn new(ctx: &'ctx mut Context, blk: BlockId) -> Self {
        Self { ctx, blk }
    }

    pub fn set_target(&mut self, blk: BlockId) {
        self.blk = blk;
    }

    fn get_blk_mut(&mut self) -> &mut Block {
        self.ctx.blocks.get_mut(self.blk).unwrap()
    }

    fn mk_inst_id(&self, local: BlockLocalInstId) -> InstId {
        InstId {
            parent: self.blk,
            local,
        }
    }

    pub fn build_arg(&mut self, n: u8) -> InstId {
        let blk = self.get_blk_mut();
        let func = blk.func;
        let func_ty = self.ctx.funcs.get(func).unwrap().ty;
        let func_param_ty = *match self.ctx.tys.get(func_ty).unwrap() {
            Ty::Func(TyFunc { params, .. }) => params.get(n as usize).unwrap(),
            _ => panic!("Not a function type"),
        };

        let blk = self.get_blk_mut();

        let local = blk.insts.push(Inst::Arg(n));
        blk.inst_tys.push(func_param_ty);

        self.mk_inst_id(local)
    }

    pub fn build_ret_void(&mut self) -> InstId {
        let ty_void = self.ctx.tys.get_or_intern(Ty::Void);
        let blk = self.get_blk_mut();

        let local = blk.insts.push(Inst::RetVoid);
        blk.inst_tys.push(ty_void);

        self.mk_inst_id(local)
    }

    pub fn build_ret(&mut self, id: InstId) -> InstId {
        let ty = *self.ctx.get_inst_ty(id);
        let blk = self.get_blk_mut();

        let local = blk.insts.push(Inst::Ret(id));
        blk.inst_tys.push(ty);

        self.mk_inst_id(local)
    }

    pub fn build_add(&mut self, lhs: InstId, rhs: InstId) -> InstId {
        let lhs_ty = *self.ctx.get_inst_ty(lhs);
        let rhs_ty = *self.ctx.get_inst_ty(rhs);
        assert_eq!(lhs_ty, rhs_ty);

        let blk = self.get_blk_mut();

        let local = blk.insts.push(Inst::Add(lhs, rhs));
        blk.inst_tys.push(lhs_ty);

        self.mk_inst_id(local)
    }

    pub fn build_const(&mut self, ty: TyId, value: ConstantValue) -> InstId {
        let blk = self.get_blk_mut();

        let local = blk.insts.push(Inst::Constant(value));
        blk.inst_tys.push(ty);

        self.mk_inst_id(local)
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
        llvm_tys: HashMap<zir::TyId, llvm::Type>,
        llvm_funcs: Vec<llvm::Function>,
        llvm_blocks: Vec<llvm::BasicBlock>,
    }

    impl<'ctx> CodeGen<'ctx> {
        pub fn new(ctx: &'ctx zir::Context) -> Self {
            let llvm_ctx = llvm::Context::new();
            Self {
                ctx,
                llvm_mod: llvm::Module::new("MainModule", Some(&llvm_ctx)),
                // llvm_builder: llvm::IRBuilder::new(&llvm_ctx),
                llvm_ctx,

                llvm_tys: Default::default(),
                llvm_funcs: Default::default(),
                llvm_blocks: Default::default(),
            }
        }

        pub fn codegen(&mut self) {
            for func in self.ctx.funcs.raw.iter() {
                self.gen_func(func);
            }
        }

        fn get_llvm_type(&mut self, id: zir::TyId) -> llvm::Type {
            self.llvm_tys.get(&id).cloned().unwrap_or_else(|| {
                let zir_ty = self.ctx.tys.get(id).cloned().unwrap();
                let llvm_ty = self.make_llvm_type(&zir_ty);
                self.llvm_tys.insert(id, llvm_ty);
                llvm_ty
            })
        }

        fn make_llvm_type(&mut self, ty: &zir::Ty) -> llvm::Type {
            match ty {
                zir::Ty::Void => llvm::Type::get_void_ty(&self.llvm_ctx),
                zir::Ty::Int(ty_int) => {
                    match ty_int.size {
                        // @FIXME: Use actual target ptr size
                        zir::TyIntSize::PtrSized => {
                            llvm::IntegerType::get(&self.llvm_ctx, 64).into()
                        }
                        zir::TyIntSize::BitSized(n) => {
                            llvm::IntegerType::get(&self.llvm_ctx, n as u32).into()
                        }
                    }
                }
                zir::Ty::Func(ty_func) => {
                    let params = ty_func
                        .params
                        .iter()
                        .map(|&id| self.get_llvm_type(id))
                        .collect::<Vec<_>>();
                    let ret = self.get_llvm_type(ty_func.ret);
                    llvm::FunctionType::new(&ret, &params, false).into()
                }
            }
        }

        fn gen_func(&mut self, func: &zir::Func) {
            let func_ty = self.get_llvm_type(func.ty).try_into().unwrap();
            let name = self.ctx.strings.get(func.name).unwrap();

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
            func.blocks.iter().for_each(|&blk_id| {
                let blk = self.ctx.blocks.get(blk_id).unwrap();

                let llvm_blk = llvm::BasicBlock::new(
                    &self.llvm_ctx,
                    self.ctx.strings.get(blk.label).unwrap(),
                    &llvm_func,
                    None,
                );
                builder.set_insertion_point(&llvm_blk);

                // for local_inst in blk.insts
                blk.insts.indices().iter().for_each(|&local_inst| {
                    let inst = blk.insts.get(local_inst).unwrap();

                    let value = match inst {
                        zir::Inst::Arg(i) => llvm_func.get_arg(*i as u32),

                        zir::Inst::Constant(k) => match k {
                            zir::ConstantValue::Integer(int) => {
                                let ty = self.get_llvm_type(*blk.inst_tys.get(local_inst).unwrap());
                                llvm::Constant::new_integer_value(&ty, *int).into()
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

                    values_map.insert(
                        zir::InstId {
                            parent: blk_id,
                            local: local_inst,
                        },
                        value,
                    );
                });

                self.llvm_blocks.push(llvm_blk);
            });

            self.llvm_funcs.push(llvm_func);
        }
    }
}

pub mod print {
    use crate::util::AutoIndentingWriter;
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
            for func in self.ctx.funcs.raw.iter() {
                let name = self.ctx.strings.get(func.name).unwrap();
                write!(self.f, "\n{:?} :: ", name)?;
                self.write_ty(func.ty)?;

                writeln!(self.f, " {{")?;
                self.f.push_indent();

                for (blk_i, blk) in func
                    .blocks
                    .iter()
                    .map(|&id| self.ctx.blocks.get(id).unwrap())
                    .enumerate()
                {
                    let label = self.ctx.strings.get(blk.label).unwrap();
                    self.f.pop_indent();
                    writeln!(self.f, "#{} {}:", blk_i, label)?;
                    self.f.push_indent();

                    for local_inst in blk.insts.indices() {
                        let inst = blk.insts.get(local_inst).unwrap();

                        write!(self.f, "%{} : ", local_inst.0)?;
                        let ty = blk.inst_tys.get(local_inst).unwrap();
                        self.write_ty(*ty)?;
                        write!(self.f, " = ")?;

                        match inst {
                            zir::Inst::Arg(n) => write!(self.f, "arg {}", n)?,
                            zir::Inst::Constant(val) => {
                                write!(self.f, "const ",)?;
                                match val {
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
            let ty = self.ctx.tys.get(id).unwrap();

            match ty {
                zir::Ty::Void => write!(self.f, "void")?,
                zir::Ty::Int(ty_int) => write!(
                    self.f,
                    "s{}",
                    match ty_int.size {
                        zir::TyIntSize::PtrSized => "intptr".to_string(),
                        zir::TyIntSize::BitSized(n) => format!("{}", n),
                    }
                )?,
                zir::Ty::Func(func) => {
                    write!(self.f, "fn(")?;
                    for (i, ty_id) in func.params.iter().enumerate() {
                        self.write_ty(*ty_id)?;

                        if i != func.params.len() - 1 {
                            write!(self.f, ", ")?;
                        }
                    }
                    write!(self.f, ") -> ")?;
                    self.write_ty(func.ret)?;
                }
            }

            self.f.flush()
        }
    }
}

pub mod test {
    use crate::zir;

    pub fn do_test() {
        let mut ctx = zir::Context::new();

        // let ty_void = ctx.tys.get_or_intern(zir::Ty::Void);
        let ty_sintptr = ctx.tys.get_or_intern(zir::Ty::Int(zir::TyInt {
            signed: true,
            size: zir::TyIntSize::PtrSized,
        }));

        {
            let func_id = ctx.funcs.push(zir::Func::new(
                ctx.strings.get_or_intern("three".to_string()),
                ctx.tys.get_or_intern(zir::Ty::Func(zir::TyFunc {
                    params: vec![],
                    ret: ty_sintptr,
                })),
            ));

            let blk_name = ctx.strings.get_or_intern("entry".to_string());
            let blk = ctx.new_block(blk_name, func_id);

            {
                let mut builder = zir::InstBuilder::new(&mut ctx, blk);
                let k1 = builder.build_const(ty_sintptr, zir::ConstantValue::Integer(1));
                let k2 = builder.build_const(ty_sintptr, zir::ConstantValue::Integer(2));
                let res = builder.build_add(k1, k2);
                builder.build_ret(res);
            }
        }

        {
            let func_id = ctx.funcs.push(zir::Func::new(
                ctx.strings.get_or_intern("sum".to_string()),
                ctx.tys.get_or_intern(zir::Ty::Func(zir::TyFunc {
                    params: vec![ty_sintptr, ty_sintptr, ty_sintptr],
                    ret: ty_sintptr,
                })),
            ));

            let blk_name = ctx.strings.get_or_intern("".to_string());
            let blk = ctx.new_block(blk_name, func_id);

            {
                let mut builder = zir::InstBuilder::new(&mut ctx, blk);
                let arg0 = builder.build_arg(0);
                let arg1 = builder.build_arg(1);
                let arg2 = builder.build_arg(2);
                let add0 = builder.build_add(arg0, arg1);
                let add1 = builder.build_add(add0, arg2);
                builder.build_ret(add1);
            }
        }

        eprintln!("\n=-=-=  ZIR Dump  =-=-=");
        zir::print::dump(&ctx, &mut std::io::stderr()).unwrap();
        // dbg!(&ctx);

        let (_llvm_ctx, llvm_mod, _llvm_funcs, _llvm_blocks) = zir::codegen::codegen(&ctx);
        use std::os::unix::prelude::FromRawFd;
        llvm::verify_module(&llvm_mod, &mut unsafe { std::fs::File::from_raw_fd(1) });
        eprintln!("\n=-=-=  LLVM Dump  =-=-=");
        llvm_mod.dump();
        std::process::exit(0);
    }
}
