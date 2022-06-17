// use crate::llvm;

#[allow(unused)]
mod zir {
    use crate::util::arena::{Arena, Ref};
    use fnv::FnvHashMap as HashMap;

    #[derive(Debug, Default)]
    pub struct Context {
        pub strings: Arena<String>,
        pub funcs: Arena<Func>,
        pub insts: Arena<Inst>,
        pub tys: Arena<Ty>,
        pub func_tys: Arena<FuncTy>,
    }

    impl Context {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn dump(&self) -> std::io::Result<()> {
            printer::Printer::dump(self, &mut std::io::stderr())
        }
    }

    #[derive(Debug, Clone, Eq, PartialEq)]
    pub struct Func {
        pub name: Ref<String>,
        pub ty: Ref<FuncTy>,
        pub values: Arena<Value>,
        pub value_types: HashMap<Ref<Value>, Ref<Ty>>,
        pub value_names: HashMap<Ref<Value>, Option<Ref<String>>>,
    }

    impl Func {
        pub fn new(name: Ref<String>, ty: Ref<FuncTy>) -> Self {
            Self {
                name,
                ty,
                values: Arena::default(),
                value_types: HashMap::default(),
                value_names: HashMap::default(),
            }
        }
    }

    #[derive(Debug, Clone, Eq, PartialEq)]
    pub enum Value {
        Integer(u64),
        String(Ref<String>),
        Inst(Ref<Inst>),
        Func(Ref<Func>),
    }

    #[derive(Debug, Clone, Eq, PartialEq)]
    pub enum Inst {
        Add(Ref<Value>, Ref<Value>),
        Ret(Option<Ref<Value>>),
    }

    #[derive(Debug, Clone, Eq, PartialEq)]
    pub enum Ty {
        Sint(u32),
        Uint(u32),
        SintSize,
        UintSize,
        Function(Ref<FuncTy>),
    }

    #[derive(Debug, Clone, Eq, PartialEq)]
    pub struct FuncTy {
        pub args: Vec<Ref<Ty>>,
        pub ret: Ref<Ty>,
    }

    pub mod printer {
        use super::*;
        use crate::util::AutoIndentingWriter;
        use std::{
            fmt,
            io::{Result, Write},
        };

        pub struct Printer<'ctx, 'w, W: Write> {
            ctx: &'ctx Context,
            f: AutoIndentingWriter<'w, W>,
        }

        impl<'ctx, 'w, W: Write> Printer<'ctx, 'w, W> {
            pub fn new(ctx: &'ctx Context, writer: &'w mut W) -> Self {
                Self {
                    ctx,
                    f: AutoIndentingWriter::new(writer, 4),
                }
            }

            pub fn dump(ctx: &'ctx Context, out: &'w mut W) -> Result<()> {
                Self::new(ctx, out).print_all()
            }

            pub fn print_all(&mut self) -> Result<()> {
                self.ctx
                    .funcs
                    .refs()
                    .into_iter()
                    .map(|s| self.write_func(s))
                    .collect::<Result<()>>()?;

                self.f.flush()
            }

            pub fn write_func(&mut self, func: Ref<Func>) -> Result<()> {
                let func = self.ctx.funcs.get(func).unwrap();
                let name = self.ctx.strings.get(func.name).unwrap();

                write!(self.f, "decl '{}' ", name)?;
                self.write_func_ty(func.ty)?;

                writeln!(self.f, " {{")?;
                self.f.push_indent();

                self.f.pop_indent();
                writeln!(self.f, "}}");

                self.f.flush()
            }

            pub fn write_func_ty(&mut self, func_ty: Ref<FuncTy>) -> Result<()> {
                let ty = self.ctx.func_tys.get(func_ty).unwrap();

                write!(self.f, "fn(");

                for (i, arg) in ty.args.iter().enumerate() {
                    self.write_ty(*arg)?;

                    if i != ty.args.len() - 1 {
                        write!(self.f, ", ")?;
                    }
                }

                write!(self.f, ") ")?;
                self.write_ty(ty.ret)?;

                self.f.flush()
            }

            pub fn write_ty(&mut self, ty: Ref<Ty>) -> Result<()> {
                let ty = self.ctx.tys.get(ty).unwrap();
                match ty {
                    Ty::Sint(bits) => write!(self.f, "s{}", bits),
                    Ty::Uint(bits) => write!(self.f, "u{}", bits),
                    Ty::SintSize => write!(self.f, "sint_size"),
                    Ty::UintSize => write!(self.f, "uint_size"),
                    Ty::Function(ty) => self.write_func_ty(*ty),
                }?;
                self.f.flush()
            }
        }
    }
}

pub fn zir_test() {
    assert_eq!(
        std::mem::size_of::<crate::util::arena::Ref<zir::Context>>(),
        std::mem::size_of::<Option<crate::util::arena::Ref<zir::Context>>>()
    );

    let mut ctx = zir::Context::new();

    let ty_s32 = ctx.tys.alloc(zir::Ty::Sint(32));

    let _ = ctx.funcs.alloc(zir::Func::new(
        ctx.strings.alloc("sum".to_string()),
        ctx.func_tys.alloc(zir::FuncTy {
            ret: ty_s32,
            args: vec![ty_s32, ty_s32, ty_s32],
        }),
    ));

    ctx.dump().unwrap();
    // zir::printer::Printer::dump(&ctx, &mut std::io::stderr()).unwrap();

    // let ctx = llvm::Context::new();
    // let module = llvm::Module::new("some module", Some(&ctx));

    // let mut builder = llvm::IRBuilder::new(&ctx);

    // let ty_u32: llvm::Type = llvm::IntegerType::get(&ctx, 32).into();
    // let ty_void = llvm::Type::get_void_ty(&ctx);

    // let func_ty = llvm::FunctionType::new(&ty_void, &[ty_u32, ty_u32, ty_u32], false);

    // let func = llvm::Function::new(
    //     &func_ty,
    //     llvm::LinkageType::ExternalLinkage,
    //     0,
    //     "sum",
    //     Some(&module),
    // );

    // let bb = llvm::BasicBlock::new(&ctx, "entry", &func, None);

    // builder.set_insertion_point(&bb);

    // let arg0 = func.get_arg(0);
    // let arg1 = func.get_arg(1);
    // let arg2 = func.get_arg(2);

    // let add0 = builder.create_add(&arg0, &arg1, "add_tmp");
    // let add1 = builder.create_add(&add0, &arg2, "add_tmp");
    // let _ = builder.create_ret(Some(&add1));

    // println!();
    // module.dump();

    std::process::exit(0);
}
