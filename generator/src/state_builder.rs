#![allow(dead_code)]
use std::{cmp, collections::HashMap, ops::Range};

use proc_macro2::{Ident, Span, TokenStream};

use quote::{quote, ToTokens, TokenStreamExt};

use crate::expr::Expr;

pub type StateId = u64;

#[derive(Clone, Debug)]
enum GeneratedExpr {
    Inline(TokenStream),
    State(StateId),
}

impl ToTokens for GeneratedExpr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            GeneratedExpr::Inline(inline) => tokens.append_all(inline.clone()),
            GeneratedExpr::State(id) => {
                let state = Ident::new(&format!("state{id}"), Span::call_site());

                tokens.append_all(quote!(return self.#state()));
            }
        }
    }
}

#[derive(Debug)]
pub struct StateBuilder<'r> {
    rules: &'r HashMap<String, Expr>,
    states: Vec<TokenStream>,
    count: StateId,
}

impl<'r> StateBuilder<'r> {
    pub fn new(rules: &'r HashMap<String, Expr>) -> Self {
        Self {
            rules,
            states: vec![],
            count: 0,
        }
    }

    fn next_id(&mut self) -> StateId {
        let id = self.count;

        self.count += 1;

        id
    }

    pub fn build(mut self, rule: &str) -> Option<TokenStream> {
        let accept = State::new(self.next_id(), |_, _, _| quote!());
        let reject = State::new(self.next_id(), |_, _, _| {
            quote! {
                self.result = false;
            }
        });

        let accept_id = accept.id;
        let reject_id = reject.id;

        let accept = accept.accept(0).reject(0).gen(&mut self);
        let reject = reject.accept(0).reject(0).gen(&mut self);

        self.states.push(accept);
        self.states.push(reject);

        let expr = self.rules.get(rule)?;
        let enter_state = State::new(self.next_id(), |builder, accept, reject| {
            builder.gen_expr(expr, accept, reject).into_token_stream()
        });

        let enter = Ident::new(&format!("state{}", enter_state.id), Span::call_site());

        let enter_state = enter_state
            .accept(accept_id)
            .reject(reject_id)
            .gen(&mut self);

        self.states.push(enter_state);

        let states = self.states;

        Some(quote! {
            pub fn parse(&mut self) -> bool {
                self.#enter();
                self.result
            }

            #(#states)*
        })
    }

    fn gen_expr(&mut self, expr: &Expr, accept_id: StateId, reject_id: StateId) -> GeneratedExpr {
        let accept = Ident::new(&format!("state{accept_id}"), Span::call_site());
        let reject = Ident::new(&format!("state{reject_id}"), Span::call_site());

        match expr {
            Expr::Range(Range { start, end }) => match (start.len_utf8(), end.len_utf8()) {
                (1, 1) => {
                    let mut slice = [0u8];

                    start.encode_utf8(&mut slice);
                    let start = slice[0];

                    end.encode_utf8(&mut slice);
                    let end = slice[0];

                    GeneratedExpr::Inline(quote! {
                        match self.input.get(self.index) {
                            Some(#start...#end) => {
                                self.index += 1;
                                return self.#accept();
                            }
                            _ => return self.#reject()
                        }
                    })
                }
                _ => {
                    let len = cmp::max(start.len_utf8(), end.len_utf8());
                    let start = *start as u32;
                    let end = *end as u32;

                    GeneratedExpr::Inline(quote! {
                        if let Some(slice) = self.input.get(self.index..self.index + #len) {
                            let mut bytes = [0u8; 4];
                            bytes[..#len].copy_from_slice(slice);

                            match u32::from_le_bytes(bytes) {
                                #start...#end => {
                                    self.index += #len;
                                    return self.#accept();
                                }
                                _ => ()
                            }
                        }

                        return self.#reject();
                    })
                }
            },
            Expr::Seq(lhs, rhs) => {
                let lhs_state = State::new(self.next_id(), |builder, accept, reject| {
                    builder.gen_expr(lhs, accept, reject).into_token_stream()
                });
                let rhs_state = State::new(self.next_id(), |builder, accept, reject| {
                    builder.gen_expr(rhs, accept, reject).into_token_stream()
                });

                let lhs_id = lhs_state.id;

                let lhs_state = lhs_state.accept(rhs_state.id).reject(reject_id).gen(self);
                let rhs_state = rhs_state.accept(accept_id).reject(reject_id).gen(self);

                self.states.push(lhs_state);
                self.states.push(rhs_state);

                GeneratedExpr::State(lhs_id)
            }
            Expr::Choice(lhs, rhs) => {
                let lhs_state = State::new(self.next_id(), |builder, accept, reject| {
                    builder.gen_expr(lhs, accept, reject).into_token_stream()
                });
                let rhs_state = State::new(self.next_id(), |builder, accept, reject| {
                    builder.gen_expr(rhs, accept, reject).into_token_stream()
                });

                let lhs_id = lhs_state.id;

                let lhs_state = lhs_state.accept(accept_id).reject(rhs_state.id).gen(self);
                let rhs_state = rhs_state.accept(accept_id).reject(reject_id).gen(self);

                self.states.push(lhs_state);
                self.states.push(rhs_state);

                GeneratedExpr::State(lhs_id)
            }
            Expr::Rep(child) => {
                let child_state = State::new(self.next_id(), |builder, accept, reject| {
                    builder.gen_expr(child, accept, reject).into_token_stream()
                });

                let child_id = child_state.id;

                let child_state = child_state.accept(child_id).reject(accept_id).gen(self);

                self.states.push(child_state);

                GeneratedExpr::State(child_id)
            }
            _ => unimplemented!(),
        }
    }
}

struct State<G> {
    id: StateId,
    accept_state: Option<StateId>,
    reject_state: Option<StateId>,
    gen: G,
}

impl<G> State<G>
where
    G: Fn(&mut StateBuilder<'_>, StateId, StateId) -> TokenStream,
{
    fn new(id: StateId, gen: G) -> Self {
        Self {
            id,
            accept_state: None,
            reject_state: None,
            gen,
        }
    }

    fn accept(mut self, id: StateId) -> Self {
        self.accept_state = Some(id);
        self
    }

    fn reject(mut self, id: StateId) -> Self {
        self.reject_state = Some(id);
        self
    }

    fn gen(self, builder: &mut StateBuilder<'_>) -> TokenStream {
        match (self.accept_state, self.reject_state) {
            (Some(accept), Some(reject)) => {
                let name = Ident::new(&format!("state{}", self.id), Span::call_site());
                let body = (self.gen)(builder, accept, reject);

                quote! {
                    fn #name(&mut self) {
                        #body
                    }
                }
            }
            _ => panic!("State missing accept/reject transitions"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example() {
        let mut rules = HashMap::new();

        rules.insert(
            "a".to_string(),
            Expr::Rep(Box::new(Expr::Seq(
                Box::new(Expr::Choice(
                    Box::new(Expr::Range('a'..'z')),
                    Box::new(Expr::Range('A'..'Z')),
                )),
                Box::new(Expr::Range('0'..'9')),
            ))),
        );
        let result = format!("{}", StateBuilder::new(&rules).build("a").unwrap());

        dbg!(result);
    }

    #[test]
    fn example1() {
        let mut rules = HashMap::new();

        rules.insert(
            "a".to_string(),
            Expr::Rep(Box::new(Expr::Seq(
                Box::new(Expr::Choice(
                    Box::new(Expr::Range('a'..'a')),
                    Box::new(Expr::Choice(
                        Box::new(Expr::Range('c'..'c')),
                        Box::new(Expr::Choice(
                            Box::new(Expr::Range('e'..'e')),
                            Box::new(Expr::Choice(
                                Box::new(Expr::Range('g'..'g')),
                                Box::new(Expr::Choice(
                                    Box::new(Expr::Range('i'..'i')),
                                    Box::new(Expr::Choice(
                                        Box::new(Expr::Range('k'..'k')),
                                        Box::new(Expr::Choice(
                                            Box::new(Expr::Range('m'..'m')),
                                            Box::new(Expr::Range('o'..'o')),
                                        )),
                                    )),
                                )),
                            )),
                        )),
                    )),
                )),
                Box::new(Expr::Choice(
                    Box::new(Expr::Range('b'..'b')),
                    Box::new(Expr::Choice(
                        Box::new(Expr::Range('d'..'d')),
                        Box::new(Expr::Choice(
                            Box::new(Expr::Range('f'..'f')),
                            Box::new(Expr::Choice(
                                Box::new(Expr::Range('h'..'h')),
                                Box::new(Expr::Choice(
                                    Box::new(Expr::Range('j'..'j')),
                                    Box::new(Expr::Choice(
                                        Box::new(Expr::Range('l'..'l')),
                                        Box::new(Expr::Choice(
                                            Box::new(Expr::Range('n'..'n')),
                                            Box::new(Expr::Range('p'..'p')),
                                        )),
                                    )),
                                )),
                            )),
                        )),
                    )),
                )),
            ))),
        );
        let result = format!("{}", StateBuilder::new(&rules).build("a").unwrap());

        dbg!(result);
    }
}
