extern crate pest;
#[macro_use]
extern crate pest_derive;

extern crate inkwell;

use core::panic;
use std::borrow::Borrow;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::{Linkage, Module};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{FileType, InitializationConfig, Target, TargetMachine};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue,
};
use inkwell::FloatPredicate::ONE;
use inkwell::IntPredicate::EQ;
use inkwell::OptimizationLevel;
use inkwell::{AddressSpace, IntPredicate};
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
    main: Option<FunctionValue<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    fn new(named: &str, context: &'ctx Context) -> Self {
        let module = context.create_module(named);
        let builder = context.create_builder();

        CodeGen {
            context,
            module,
            builder,
            main: None,
        }
    }

    #[inline]
    fn function_value(&self, named: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(named)
    }

    /// Creates a new stack allocation instruction in the entry block of the function.
    /// https://github.com/TheDan64/inkwell/blob/master/examples/kaleidoscope/main.rs#L863
    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.main.unwrap().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.context.i64_type(), name)
    }

    fn generate_from_ast(&mut self, node: Box<cordial::Ast>) {
        use cordial::Ast::*;
        match *node {
            Programa(contenido) => {
                let i64_type = self.context.i64_type();
                let main_sign = i64_type.fn_type(&[], false);
                let main_func = self.module.add_function("main", main_sign, None);
                let block = self.context.append_basic_block(main_func, "entry");

                self.builder.position_at_end(block);

                self.main = Some(main_func);
                self.generate_from_ast(contenido);

                let block = self.main.unwrap().get_last_basic_block().unwrap();

                self.builder.position_at_end(block);
                self.builder.build_return(Some(&i64_type.const_zero()));
            }
            Bloque(sentencias) => {
                for i in sentencias {
                    self.generate_from_ast(i);
                }
            }
            Di(texto) => self.decir(texto),
            Muestra(nodo, is_bool) => self.mostrar(nodo, is_bool),
            Repetir(cuenta, cuerpo) => {
                let counter_name = "loop_counter";
                let start_alloca = self.create_entry_block_alloca(counter_name);
                let cuenta = self.construir_expresion(*cuenta);

                let start = self.context.i64_type().const_int(0, false);
                self.builder.build_store(start_alloca, start);

                let loop_block = self
                    .context
                    .append_basic_block(self.main.unwrap(), "repetir");

                self.builder.build_unconditional_branch(loop_block); // main -> repetir
                self.builder.position_at_end(loop_block);

                let curr_var = self.builder.build_load(start_alloca, counter_name);

                let end_cond = self.builder.build_int_compare(
                    inkwell::IntPredicate::SLT,
                    curr_var.into_int_value(),
                    cuenta.into_int_value(),
                    "loop_compare",
                );

                let loop_body = self
                    .context
                    .append_basic_block(self.main.unwrap(), "cuerporepetir");
                self.builder.position_at_end(loop_body);

                self.generate_from_ast(cuerpo);

                let step = self.context.i64_type().const_int(1, false);

                let next_var =
                    self.builder
                        .build_int_add(curr_var.into_int_value(), step, "nextvar");

                self.builder.build_store(start_alloca, next_var);

                self.builder.build_unconditional_branch(loop_block);

                let after_loop = self
                    .context
                    .append_basic_block(self.main.unwrap(), "luegorepetir");

                // if loop_counter < cuenta|-> cuerpo_repetir
                //                   else  |-> after_loop
                self.builder.position_at_end(loop_block);
                // let bool_temp = self.builder.build_unsigned_int_to_float(
                //     end_cond,
                //     self.context.f64_type(),
                //     "booltemp",
                // );
                // let bool_cond = self.builder.build_float_compare(
                //     ONE,
                //     bool_temp,
                //     self.context.f64_type().const_float(0.0),
                //     "loopcond",
                // );
                self.builder
                    .build_conditional_branch(end_cond, loop_body, after_loop);

                self.builder.position_at_end(after_loop);
            }
            _ => todo!(),
        }
    }

    fn generar_printf(&mut self) -> FunctionValue<'ctx> {
        if let Some(existing) = self.function_value("printf") {
            existing
        } else {
            let f64_type = self.context.f64_type();
            let str_type = self
                .context
                .i8_type()
                .ptr_type(inkwell::AddressSpace::Generic);

            let printf_args_type =
                vec![inkwell::types::BasicMetadataTypeEnum::PointerType(str_type)];
            let printf_type = f64_type.fn_type(printf_args_type.as_slice(), true);
            self.module.add_function("printf", printf_type, None)
        }
    }

    fn mostrar(&mut self, nodo: Box<cordial::Ast>, is_bool: bool) {
        let mut is_true = false;
        // TODO: Soportar operaciones aparte de literales de verdad.
        if let cordial::Ast::Verdad(v) = nodo.borrow() { is_true = *v; }
        let value = self.construir_expresion(*nodo);

        let printf = self.generar_printf();

        let format_string = self.builder.build_global_string_ptr(if is_bool { "%s" } else { "%d" }, "format_string"); // No lo uso como format aún.

        let meta: BasicMetadataValueEnum =
            if is_bool {
                let metadata_string = unsafe {
                    self.builder
                        .build_global_string(if is_true { "cierto" } else { "falso" }, "valor")
                        .as_basic_value_enum()
                };

                metadata_string.into()
            } else {
                value.into()
            };
        self.builder
            .build_call(
                printf,
                &[format_string.as_pointer_value().into(), meta],
                "muestra",
            )
            .try_as_basic_value()
            .expect_left("Invalid");
    }

    fn decir(&mut self, texto: String) {
        let printf = self.generar_printf();

        let format_string = self.builder.build_global_string_ptr("%s", "format_string"); // No lo uso como format aún.
        let metadata_string = unsafe {
            self.builder
                .build_global_string(&texto, "valor")
                .as_basic_value_enum()
        };

        self.builder
            .build_call(
                printf,
                &[
                    format_string.as_pointer_value().into(),
                    metadata_string.into(),
                ],
                "di",
            )
            .try_as_basic_value()
            .expect_left("Invalid");
    }

    fn construir_expresion(&mut self, node: cordial::Ast) -> BasicMetadataValueEnum<'ctx> {
        match node {
            cordial::Ast::OpBin(op, lhs, rhs) => {
                let lhs = self.construir_expresion(*lhs).into_int_value();
                let rhs = self.construir_expresion(*rhs).into_int_value();
                let operator;
                match op {
                    cordial::Operator::Mas => {
                        operator = self.builder.build_int_add(lhs, rhs, "tempadd");
                    }
                    cordial::Operator::Menos => {
                        operator = self.builder.build_int_sub(lhs, rhs, "tempsub");
                    }
                    cordial::Operator::Por => {
                        operator = self.builder.build_int_mul(lhs, rhs, "tempmul");
                    }
                    cordial::Operator::Entre => {
                        operator = self.builder.build_int_signed_div(lhs, rhs, "tempdiv");
                    }
                }

                operator.as_basic_value_enum().into()
            }
            cordial::Ast::Numero(num) => self.context.i64_type().const_int(num, false).into(),
            cordial::Ast::Verdad(val) => self.context.i64_type().const_int(if val { 1 } else { 0 }, false).into(),
            _ => panic!("Expresion invalida! No genera un número."),
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
        Verdad(bool),
        Muestra(Box<Ast>, bool),
        Di(String),
        Bloque(Vec<Box<Ast>>),
        OpBin(Operator, Box<Ast>, Box<Ast>),
        Repetir(Box<Ast>, Box<Ast>),
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
        cordial::Rule::verdad => Ok(Box::new(Verdad(pair.as_str() == "cierto"))),
        cordial::Rule::texto => {
            // let child = pairs.into_inner().next().unwrap();
            let str = pair.as_str();
            Ok(Box::new(Texto(str[1..(str.len() - 1)].to_string())))
        }
        cordial::Rule::texto_contenido => todo!(),
        cordial::Rule::di => {
            let child = pair.into_inner().next().unwrap();
            let str = child.as_str();
            Ok(Box::new(Di(str[1..(str.len() - 1)]
                .to_string()
                .to_string())))
        }
        cordial::Rule::muestra => {
            let child = pair.into_inner().next().unwrap();
            let child_value = build_tree(child)?;
            let mut is_bool = false;
            if let Verdad(_) = *child_value { is_bool = true; }
            Ok(Box::new(Muestra(child_value, is_bool)))
        }
        cordial::Rule::baja => Ok(Box::new(Di("\n".to_string()))),
        cordial::Rule::repite_veces => {
            let mut child = pair.into_inner();
            let expr = build_tree(child.next().unwrap())?;

            let body = build_tree(child.next().unwrap())?;

            Ok(Box::new(Repetir(expr, body)))
        }
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
    let mut file =
        cordial::Parser::parse(cordial::Rule::programa, &file_content).expect("Error al procesar");

    let context = Context::create();
    let mut codegen = CodeGen::new("main", &context);

    let default_triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&default_triple)?;

    let machine = target
        .create_target_machine(
            &default_triple,
            TargetMachine::get_host_cpu_name().to_str()?,
            TargetMachine::get_host_cpu_features().to_str()?,
            OptimizationLevel::None,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .expect("No se pudo inicializar la máquina objetivo");

    let tree = build_tree(file.next().unwrap())?;

    codegen.generate_from_ast(tree);
    if let Err(error) = codegen.module.verify() {
        panic!("{}", error.to_string());
    }

    let object_name = "./target/output.o";
    let path = Path::new(object_name);
    let result = machine.write_to_file(&codegen.module, FileType::Object, path);
    match result {
        Ok(_) => {}
        Err(err) => println!("Error :(\n{}", err),
    }

    // println!("Tree:\n{}", tree)
    // for token in file {
    //     let tree = build_tree(token);
    //     println!("{:?}", tree.unwrap())
    // }
    let mut args = vec!["-o", "target/output", "-e", "_main"];
    let mut program = vec![object_name];
    if machine.get_triple().to_string().contains("apple-darwin") {
        program.append(&mut vec![
            "-lSystem",
            "-L",
            "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib",
        ]);
    }
    program.append(&mut args);

    println!("Linking:\nld {}", program.join(" "));
    let mut linker = Command::new("ld");
    linker.args(program);
    let output = linker
        .output()
        .expect("No se pudo hacer link en el archivo");

    if !output.stderr.is_empty() {
        println!("{}", String::from_utf8_lossy(&output.stderr));
    } else {
        println!("{}", String::from_utf8_lossy(&output.stdout));
    }

    println!("Running:");
    let mut run = Command::new("./target/output");
    let output = run.output().expect("No se pudo correr el programa");
    let stderr = output.stderr;
    let stdout = output.stdout;

    print!("{}", String::from_utf8_lossy(&stdout));
    print!("{}", String::from_utf8_lossy(&stderr));

    Ok(())
}
