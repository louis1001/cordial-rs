extern crate pest;
#[macro_use]
extern crate pest_derive;

extern crate inkwell;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::{Linkage, Module};
use inkwell::targets::{FileType, InitializationConfig, Target, TargetMachine};
use inkwell::types::FunctionType;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use pest::iterators::Pair;
use pest::Parser;
use rand::distributions::Alphanumeric;
use rand::Rng;
use std::error::Error;
use std::fs;
use std::path::Path;
use std::process::Command;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    block: BasicBlock<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    fn new(named: &str, context: &'ctx Context) -> Self {
        let module = context.create_module("main");
        let builder = context.create_builder();
        let i64_type = context.i64_type();
        let main_sign = i64_type.fn_type(&[], false);
        let main_func = module.add_function("main", main_sign, None);
        let block = context.append_basic_block(main_func, "entry");

        match block.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(block),
        }

        let inst = builder.build_return(Some(&i64_type.const_zero()));
        builder.position_before(&inst);

        CodeGen {
            context: context,
            module,
            builder,
            block: block,
        }
    }

    #[inline]
    fn function_value(&self, named: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(named)
    }

    fn from_ast(&mut self, node: Box<cordial::Ast>) {
        use cordial::Ast::*;
        match *node {
            Programa(contenido) => self.from_ast(contenido),
            Bloque(sentencias) => {
                for i in sentencias {
                    self.from_ast(i);
                }
            }
            Di(texto) => {
                let i32_type = self.context.i32_type();
                let str_type = self.context.i8_type().ptr_type(AddressSpace::Generic);
                let printf_type = i32_type.fn_type(
                    &[inkwell::types::BasicMetadataTypeEnum::PointerType(str_type)],
                    true,
                );
                let printf;
                if let Some(fun) = self.module.get_function("printf") {
                    printf = fun;
                } else {
                    let fun = self.module.add_function("printf", printf_type, None);
                    printf = fun;
                }

                let value = self.context.metadata_string(texto.as_str());

                self.builder.build_call(printf, &[value.into()], "muestra");
            }
            _ => {}
        }
    }
}

mod cordial {
    #[derive(Parser)]
    #[grammar = "parser/cordial.pest"]
    pub struct Parser;

    #[derive(Debug)]
    pub enum Operator {
        Mas,
        Menos,
        Por,
        Entre,
    }

    impl From<&str> for Operator {
        fn from(op: &str) -> Self {
            match op {
                "mas" => Operator::Mas,
                "menos" => Operator::Menos,
                "por" => Operator::Por,
                "entre" => Operator::Entre,
                _ => {
                    println!("{}", op);
                    panic!()
                }
            }
        }
    }
    impl From<String> for Operator {
        fn from(op: String) -> Self {
            Self::from(op.as_str())
        }
    }

    #[derive(Debug)]
    pub enum Ast {
        Programa(Box<Ast>),
        Numero(u64),
        Texto(String),
        Muestra(Box<Ast>),
        Di(String),
        Bloque(Vec<Box<Ast>>),
        OpBin(Operator, Box<Ast>, Box<Ast>),
        NoOp,
    }
}

fn build_tree(pair: Pair<cordial::Rule>) -> Result<Box<cordial::Ast>, String> {
    use cordial::Ast::*;
    match pair.as_rule() {
        cordial::Rule::programa => {
            let inner = pair.into_inner().next().unwrap();

            Ok(Box::new(Programa(build_tree(inner)?)))
        }
        cordial::Rule::lista_sentencias => {
            let inner = pair.into_inner().map(|x| build_tree(x).unwrap());

            Ok(Box::new(Bloque(inner.collect())))
        }
        cordial::Rule::peticion => {
            let inner = pair.into_inner().next().unwrap();

            build_tree(inner)
        }
        cordial::Rule::sentencia_bloque => todo!(),
        cordial::Rule::expresion | cordial::Rule::producto => {
            let mut product_parts = pair.into_inner();
            let lhs = build_tree(product_parts.next().unwrap())?;
            let mut result: Box<cordial::Ast> = lhs;
            while let Some(operator) = product_parts.next() {
                let op = operator.as_str();
                let rhs = build_tree(product_parts.next().unwrap())?;
                result = Box::new(OpBin(op.into(), result, rhs));
            }
            Ok(result)
        }
        cordial::Rule::numero => {
            // Se que es valido por las reglas de gramática
            Ok(Box::new(Numero(pair.as_str().parse().unwrap())))
        }
        cordial::Rule::verdad => todo!(),
        cordial::Rule::texto => {
            // let child = pairs.into_inner().next().unwrap();
            let str = pair.as_str();
            Ok(Box::new(Texto(str[1..(str.len() - 1)].to_string())))
        }
        cordial::Rule::texto_contenido => todo!(),
        cordial::Rule::di => {
            let child = pair.into_inner().next().unwrap();
            Ok(Box::new(Di(child.as_str().to_string())))
        }
        cordial::Rule::muestra => {
            let child = pair.into_inner().next().unwrap();
            Ok(Box::new(Muestra(build_tree(child)?)))
        }
        cordial::Rule::baja => Ok(Box::new(Muestra(Box::new(Texto("\n".to_string()))))),
        _ => Err(format!("Token inesperado: `{}`", pair.as_str())),
    }
}

fn indent(n: usize) -> String {
    let mut result = String::new();
    for _ in 0..n * 4 {
        result += " ";
    }

    result.to_string()
}

pub fn print_tree(pair: Pair<cordial::Rule>, level: usize) -> String {
    match pair.as_rule() {
        cordial::Rule::programa => todo!(),
        cordial::Rule::lista_sentencias => {
            let children = pair.into_inner();
            let mut result = "".to_string();
            for child in children {
                result += &print_tree(child, level);
            }
            result
        }
        cordial::Rule::sentencia_bloque => {
            let child = pair.into_inner().next().unwrap();
            print_tree(child, level + 1)
        }
        cordial::Rule::expresion | cordial::Rule::producto => {
            let mut product_parts = pair.into_inner();
            let lhs = print_tree(product_parts.next().unwrap(), level + 1);
            if let Some(operator) = product_parts.next() {
                let op = operator.as_str();
                let rhs = print_tree(product_parts.next().unwrap(), level + 1);
                op.to_string() + ":\n" + &lhs + &rhs
            } else {
                format!("{}\n", lhs)
            }

            // let mut elements = pair.into_inner();
            // print_tree(elements.clone().next().unwrap(), level);
            // elements.next().unwrap();

            // let mut result: Box<cordial::Ast> = lhs;

            // while let Some(operacion) = elements.next() {
            //     let operator = operacion.as_str().to_string();
            //     elements.next().unwrap();
            //     let rhs = build_tree(elements.next().unwrap())?;
            //     println!("{} -> {:?}", operator, rhs);

            //     result = Box::new(OpBin(operator.to_string().into(), result, rhs));
            // }
        }
        cordial::Rule::numero => {
            indent(level);
            format!("numero:({})\n", pair.as_str())
        }
        cordial::Rule::verdad => {
            indent(level);
            format!("verdad({})\n", pair.as_str())
        }
        cordial::Rule::texto => {
            indent(level);
            format!("texto: (\"{}\")\n", pair.as_str())
        }
        cordial::Rule::di => {
            let child = pair.into_inner().next().unwrap();
            indent(level) + "di: {\n" + &print_tree(child, level + 1) + &indent(level) + "}\n"
        }
        cordial::Rule::muestra => {
            let child = pair.into_inner().next().unwrap();
            indent(level) + "muestra: {\n" + &print_tree(child, level + 1) + &indent(level) + "}\n"
        }
        cordial::Rule::baja => indent(level) + "baja\n",
        _ => "".to_string(),
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    Target::initialize_native(&InitializationConfig::default())?;

    let path = "ejemplos/prueba.cord";
    let file_content = fs::read_to_string(path).unwrap();
    let mut file = cordial::Parser::parse(cordial::Rule::programa, &file_content)
        .expect("Error while parsing");

    let context = Context::create();
    let mut codegen = CodeGen::new("main", &context);

    let default_triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&default_triple)?;

    let machine = target
        .create_target_machine(
            &default_triple,
            TargetMachine::get_host_cpu_name().to_str()?,
            TargetMachine::get_host_cpu_features().to_str()?,
            OptimizationLevel::Default,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .expect("Couldn't initialize the host machine");

    let object_name = "./output.out";
    let path = Path::new(object_name);
    let result = machine.write_to_file(&codegen.module, FileType::Object, path);

    match result {
        Ok(_) => println!("Success!"),
        Err(err) => println!("Error :(\n{}", err),
    }

    let tree = build_tree(file.next().unwrap())?;

    codegen.from_ast(tree);
    codegen.module.verify().expect("Modulo invalido");

    // println!("Tree:\n{}", tree)
    // for token in file {
    //     let tree = build_tree(token);
    //     println!("{:?}", tree.unwrap())
    // }
    let mut args = vec!["-o", "output", "-e", "_main"];
    let mut program = vec![object_name];
    if machine.get_triple().to_string().contains("apple-darwin") {
        program.append(&mut vec![
            "-lSystem",
            "-macosx_version_min",
            "11.0",
            "-L",
            "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib",
        ]);
    }
    program.append(&mut args);

    let mut linker = Command::new("ld");
    linker.args(program);
    let output = linker.output().expect("Couldn't link file");

    if !output.stderr.is_empty() {
        println!("{}", String::from_utf8_lossy(&output.stderr));
    } else {
        println!("{}", String::from_utf8_lossy(&output.stdout));
    }

    Ok(())
}
