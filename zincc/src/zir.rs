use crate::{
    parse::{ast::StrSym, cst::NodeId},
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
}

#[derive(Debug, Clone)]
pub struct Func {
    name: StrSym,
    blocks: Vec<BlockId>,
    ty: TyId,
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

pub mod codegen {
    use std::collections::HashMap;

    use crate::zir;
    use llvm;

    pub fn codegen(ctx: &zir::Context) -> (llvm::Context, llvm::Module, Vec<llvm::Function>) {
        let mut codegen = CodeGen::new(ctx);
        codegen.codegen();
        (codegen.llvm_ctx, codegen.llvm_mod, codegen.llvm_funcs)
    }

    pub struct CodeGen<'ctx> {
        ctx: &'ctx zir::Context,
        llvm_ctx: llvm::Context,
        llvm_mod: llvm::Module,
        llvm_builder: llvm::IRBuilder,

        llvm_tys: HashMap<zir::TyId, llvm::Type>,
        llvm_funcs: Vec<llvm::Function>,
    }

    impl<'ctx> CodeGen<'ctx> {
        pub fn new(ctx: &'ctx zir::Context) -> Self {
            let llvm_ctx = llvm::Context::new();
            Self {
                ctx,
                llvm_mod: llvm::Module::new("MainModule", Some(&llvm_ctx)),
                llvm_builder: llvm::IRBuilder::new(&llvm_ctx),
                llvm_ctx,

                llvm_tys: Default::default(),
                llvm_funcs: Default::default(),
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
            let ty: llvm::FunctionType = self.get_llvm_type(func.ty).try_into().unwrap();
            let name = self.ctx.strings.get(func.name).unwrap();

            let llvm_func = llvm::Function::new(
                &ty,
                llvm::LinkageType::ExternalLinkage,
                0,
                name,
                Some(&self.llvm_mod),
            );

            let mut values_map: HashMap<zir::InstId, llvm::Value> = Default::default();

            // for blk_id in func.blocks
            func.blocks.iter().for_each(|&blk_id| {
                let blk = self.ctx.blocks.get(blk_id).unwrap();

                let llvm_blk = llvm::BasicBlock::new(
                    &self.llvm_ctx,
                    self.ctx.strings.get(blk.label).unwrap(),
                    &llvm_func,
                    None,
                );
                self.llvm_builder.set_insertion_point(&llvm_blk);

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
                            self.llvm_builder.create_add(lhs, rhs, "tmpadd")
                        }

                        zir::Inst::Call(_, _) => todo!(),

                        zir::Inst::Ret(id) => {
                            let val = values_map.get(id).unwrap();
                            self.llvm_builder.create_ret(Some(val))
                        }

                        zir::Inst::RetVoid => self.llvm_builder.create_ret_void(),
                    };

                    values_map.insert(
                        zir::InstId {
                            parent: blk_id,
                            local: local_inst,
                        },
                        value,
                    );
                });
            });

            // llvm::verify_function(&llvm_func, &mut unsafe { std::fs::File::from_raw_fd(1) });
            self.llvm_funcs.push(llvm_func);
        }
    }
    // use std::os::unix::prelude::FromRawFd;
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
                write!(self.f, "{:?} ", name)?;
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
                        let ty = blk.inst_tys.get(local_inst).unwrap();
                        write!(self.f, "%{} = ", local_inst.0)?;
                        match inst {
                            zir::Inst::Arg(_) => todo!(),
                            zir::Inst::Constant(val) => {
                                write!(self.f, "const ",)?;
                                self.write_ty(*ty)?;
                                write!(self.f, " ",)?;
                                match val {
                                    zir::ConstantValue::Integer(int) => write!(self.f, "{}", int)?,
                                    zir::ConstantValue::String(str) => write!(self.f, "{:?}", str)?,
                                    zir::ConstantValue::Func(_) => write!(self.f, "fn")?,
                                }
                            }
                            zir::Inst::Add(lhs, rhs) => {
                                write!(self.f, "add ")?;
                                self.write_ty(*ty)?;
                                write!(self.f, " {} {}", lhs, rhs)?;
                            }
                            zir::Inst::Call(_, _) => todo!(),
                            zir::Inst::Ret(_) => todo!(),
                            zir::Inst::RetVoid => write!(self.f, "ret void")?,
                        }
                    }
                }

                self.f.pop_indent();
                writeln!(self.f, "\n}}")?;
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
                        zir::TyIntSize::PtrSized => "int_size".to_string(),
                        zir::TyIntSize::BitSized(n) => format!("{}", n),
                    }
                )?,
                zir::Ty::Func(func) => {
                    write!(self.f, "fn(")?;
                    for ty_id in func.params.iter() {
                        self.write_ty(*ty_id)?;
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
    use crate::{util::index::IndexVec, zir};
    use std::os::unix::prelude::FromRawFd;

    pub fn do_test() {
        let mut ctx = zir::Context::new();

        let ty_void = ctx.tys.get_or_intern(zir::Ty::Void);

        let _id = ctx.funcs.push(zir::Func {
            name: ctx.strings.get_or_intern("foo".to_string()),
            ty: ctx.tys.get_or_intern(zir::Ty::Func(zir::TyFunc {
                params: vec![],
                ret: ty_void,
            })),
            blocks: vec![ctx.blocks.push(zir::Block {
                label: ctx.strings.get_or_intern("entry".to_string()),
                insts: IndexVec::from_raw(vec![zir::Inst::RetVoid]),
                inst_tys: IndexVec::from_raw(vec![ty_void]),
            })],
        });

        zir::print::dump(&ctx, &mut std::io::stderr()).unwrap();

        let (_llvm_ctx, llvm_mod, _llvm_funcs) = zir::codegen::codegen(&ctx);
        llvm::verify_module(&llvm_mod, &mut unsafe { std::fs::File::from_raw_fd(1) });
        llvm_mod.dump();
        std::process::exit(0);
    }
}
