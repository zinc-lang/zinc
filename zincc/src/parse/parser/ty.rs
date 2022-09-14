use super::*;

/// Parse ty
impl Parser<'_> {
    pub fn parse_ty(&mut self, parent: NodeId) {
        match self.peek() {
            TK::kw_fn => self.parse_ty_func(parent),
            TK::ident => {
                let mut path = self.parse_path();
                path.parent = Some(parent);
            }
            TK::brkt_square_open => {
                let mut ty = self.pnode(NK::ty_slice).parent(parent);
                _ = ty.peek();
                ty.push_token();

                if !ty.at(TK::brkt_square_close) {
                    ty.kind = NK::ty_array;
                    self.parse_expr(*ty);
                }

                ty.expect(TK::brkt_square_close);

                self.parse_ty(*ty);
            }
            TK::punct_question => {
                let mut nullable = self.pnode(NK::ty_nullable).parent(parent);
                _ = nullable.peek();
                nullable.push_token();
                self.parse_ty(*nullable);
            }
            _ => todo!(),
        }
    }

    pub fn parse_ty_func_wo_return(&mut self) -> PNode {
        let mut sig = self.pnode(NK::func_sig);

        sig.expect(TK::kw_fn);

        if sig.at(TK::brkt_square_open) {
            self.parse_generics_list(*sig);
        }

        if sig.at(TK::brkt_paren_open) {
            self.parse_params_list(*sig);
        }

        sig
    }

    fn parse_ty_func(&mut self, parent: NodeId) {
        let mut sig = self.parse_ty_func_wo_return();
        sig.parent = Some(parent);
        self.parse_ty_func_return(*sig);
    }

    pub fn parse_ty_func_return(&mut self, parent: NodeId) {
        let ret = self.pnode(NK::func_sig_ret).parent(parent);
        self.parse_ty(*ret);
    }

    fn parse_params_list(&mut self, parent: NodeId) {
        let mut list = self.pnode(NK::paramsList).parent(parent);

        list.expect(TK::brkt_paren_open);

        while !list.at(TK::brkt_paren_close) {
            let mut param = self.pnode(NK::param_param).parent(*list);

            param.expect(TK::ident);
            self.parse_ty(*param);

            if param.at(TK::punct_eq) {
                let mut def = self.pnode(NK::param_param_default).parent(*param);
                def.push_token();
                self.parse_expr(*def);
            }

            if !param.eat(TK::punct_comma) {
                break;
            }
        }

        list.expect(TK::brkt_paren_close);
    }

    fn parse_generics_list(&mut self, parent: NodeId) {
        let mut list = self.pnode(NK::genericsList).parent(parent);

        list.expect(TK::brkt_square_open);

        while !list.at(TK::brkt_square_close) {
            let mut generic = self.pnode(NK::genericParam_type).parent(*list);

            generic.expect(TK::ident);

            // Comptime value generic
            if generic.eat(TK::punct_bang) {
                generic.kind = NK::genericParam_value;
                self.parse_ty(*generic);

                // Default value
                if generic.at(TK::punct_eq) {
                    let mut def = self.pnode(NK::genericParam_value_default).parent(*generic);
                    def.push_token();
                    self.parse_expr(*def);
                }
            }
            // Type generic
            else {
                // '+' separated type constraints
                if !generic.at_one_of(&[TK::punct_comma, TK::brkt_square_close, TK::punct_eq]) {
                    loop {
                        let con = self
                            .pnode(NK::genericParam_type_constraint)
                            .parent(*generic);
                        self.parse_ty(*con);

                        drop(con); // Make sure 'con' gets appended to generic before the '+'
                        if !generic.eat(TK::punct_plus) {
                            break;
                        }
                    }
                }

                // Default type
                if generic.at(TK::punct_eq) {
                    let mut def = self.pnode(NK::genericParam_type_default).parent(*generic);
                    def.push_token();
                    self.parse_ty(*def);
                }
            }

            if !generic.eat(TK::punct_comma) {
                break;
            }
        }

        list.expect(TK::brkt_square_close);
    }

    // fn parse_ty(&mut self, parent: &mut PNode) {
    //     match self.peek() {
    //         TK::ident | TK::punct_dblColon => {
    //             let path = self.parse_path();
    //             self.append_node(path, parent);
    //         }
    //         TK::brkt_square_open => {
    //             let mut slice_ty = self.node(NK::ty_slice);
    //             self.bump(&mut slice_ty); // '['

    //             // @TODO: parse optional size
    //             self.expect(TK::brkt_square_close, ParseContext::TySlice, &mut slice_ty); // ']'

    //             self.parse_ty(&mut slice_ty); // ty
    //             self.append_node(slice_ty, parent);
    //         }
    //         TK::punct_question => {
    //             let mut nullable_ty = self.node(NK::ty_nullable);
    //             self.bump(&mut nullable_ty); // '?'
    //             self.parse_ty(&mut nullable_ty); // ty
    //             self.append_node(nullable_ty, parent);
    //         }
    //         TK::kw_fn => self.parse_ty_func(parent),
    //         _ => self.report(
    //             parent,
    //             ParseError::Expected(ParseErrorExpected {
    //                 what: ParseErrorExpectedWhat::Item(ParseErrorItem::Ty),
    //                 at: self.cursor,
    //                 found: self.cursor,
    //                 context: ParseContext::Ty,
    //             }),
    //         ),
    //     }
    // }

    // fn parse_func_params_and_ret(&mut self, ctx: ParseContext, parent: &mut PNode) {
    //     self.expect(TK::brkt_paren_open, ctx, parent);
    //     while !self.at(TK::brkt_paren_close) {
    //         let mut param = self.node(NK::func_proto_param);

    //         if self.at(TK::ident) && self.peek_n(1) == TK::punct_colon {
    //             self.expect(TK::ident, ctx, &mut param);
    //             self.expect(TK::punct_colon, ctx, &mut param);
    //         }

    //         self.parse_ty(&mut param);
    //         self.append_node(param, parent);

    //         if !self.eat(TK::punct_comma, parent) {
    //             break;
    //         }
    //     }
    //     self.expect(TK::brkt_paren_close, ctx, parent);

    //     if self.eat(TK::punct_colon, parent) {
    //         let mut ret = self.node(NK::func_proto_ret);
    //         self.parse_ty(&mut ret);
    //         self.append_node(ret, parent);
    //     }
    // }

    // fn parse_ty_func(&mut self, parent: &mut PNode) {
    //     assert!(self.at(TK::kw_fn));

    //     let mut proto = self.node(NK::func_proto);
    //     self.bump(&mut proto); // 'fn'

    //     self.parse_func_params_and_ret(ParseContext::TyFunc, &mut proto);

    //     self.append_node(proto, parent);
    // }
}
