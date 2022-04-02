extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::fs;

use pest::iterators::Pair;
use pest::Parser;

macro_rules! node {
    ( $i:expr ) => {
        Box::new($i)
    };
}

macro_rules! node {
    ( $i:expr ) => {
        Ok(Box::new($i))
    };
}

mod cordial {
    #[derive(Parser)]
    #[grammar = "parser/cordial.pest"]
    pub struct Parser;

    #[derive(Debug)]
    pub enum Ast {
        Numero(u64),
        Texto(String),
        Muestra(Box<Ast>),
        Di(Box<Ast>),
    }
}

fn build_tree(pairs: Pair<cordial::Rule>) -> Result<Box<cordial::Ast>, String> {
    use cordial::Ast::*;
    match pairs.as_rule() {
        cordial::Rule::programa => todo!(),
        cordial::Rule::lista_sentencias => todo!(),
        cordial::Rule::peticion => todo!(),
        cordial::Rule::sentencia_bloque => todo!(),
        cordial::Rule::mas => todo!(),
        cordial::Rule::menos => todo!(),
        cordial::Rule::por => todo!(),
        cordial::Rule::entre => todo!(),
        cordial::Rule::producto => todo!(),
        cordial::Rule::expresion => todo!(),
        cordial::Rule::numero => {
            // Se que es valido por las reglas de gramática
            node!(Numero(pairs.as_str().parse().unwrap()))
        }
        cordial::Rule::verdad => todo!(),
        cordial::Rule::texto => {
            // let child = pairs.into_inner().next().unwrap();
            let str = pairs.as_str();
            node!(Texto(str[1..(str.len()-1)].to_string()))
        }
        cordial::Rule::texto_contenido => todo!(),
        cordial::Rule::di => {
            let child = pairs.into_inner().next().unwrap();
            node!(Di(build_tree(child)?))
        }
        cordial::Rule::muestra => {
            let child = pairs.into_inner().next().unwrap();
            node!(Muestra(build_tree(child)?))
        }
        cordial::Rule::baja => node!(Muestra(Box::new(Texto("\n".to_string())))),
        _ => Err(format!("Token inesperado: `{}`", pairs.as_str())),
    }
}

fn print_tree(pairs: Pair<cordial::Rule>, level: usize) {
    match pairs.as_rule() {
        cordial::Rule::programa => todo!(),
        cordial::Rule::lista_sentencias => {
            let children = pairs.into_inner();
            for child in children {
                print_tree(child, level + 1);
            }
        }
        cordial::Rule::sentencia_bloque => {
            let child = pairs.into_inner().next().unwrap();
            print_tree(child, level + 1);
        }
        cordial::Rule::mas => {
            println!("{: <0$}mas", level);
        }
        cordial::Rule::menos => {
            println!("{: <0$}menos", level);
        }
        cordial::Rule::por => {
            println!("{: <0$}por", level);
        }
        cordial::Rule::entre => {
            println!("{: <0$}entre", level);
        }
        cordial::Rule::numero => println!("numero: {}", pairs.as_str()),
        cordial::Rule::verdad => println!("verdad: {}", pairs.as_str()),
        cordial::Rule::texto_contenido => println!("texto: \"{}\"", pairs.as_str()),
        cordial::Rule::di => {
            let child = pairs.into_inner().next().unwrap();
            print!("di: ");
            print_tree(child, level + 1);
        }
        cordial::Rule::muestra => {
            let child = pairs.into_inner().next().unwrap();
            print!("muestra: ");
            print_tree(child, level + 1);
        }
        cordial::Rule::baja => println!("baja"),
        _ => println!("{}", pairs.as_str()),
    }
}

fn main() {
    let path = "ejemplos/prueba.cord";
    let file_content = fs::read_to_string(path).unwrap();
    let file: Vec<_> = cordial::Parser::parse(cordial::Rule::programa, &file_content)
        .expect("Error while parsing")
        .collect();
    for token in file {
        // print_tree(token, 0)
        let tree = build_tree(token);
        println!("{:?}", tree.unwrap())
        // let result = match token.as_rule() {
        //     pest::Token::Start(val) => format!("muestra {}", ),
        //     _ => ""
        // }
    }
}
