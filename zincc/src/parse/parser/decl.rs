use super::*;

/// Parse decl
impl Parser<'_> {
    pub fn parse_decl(&mut self, parent: NodeId) {
        let mut decl = self.pnode(NK::decl, parent);

        decl.expect(TK::punct_doubleColon);

        match decl.peek() {
            TK::ident => {
                decl.push_token();

                match decl.peek() {
                    TK::kw_fn => self.parse_decl_func(*decl),
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }

    fn parse_decl_func(&mut self, parent: NodeId) {
        let mut func = self.pnode(NK::decl_func, parent);

        let mut sig = self.parse_ty_func_wo_return();
        sig.parent = Some(*func);

        if !func.at_one_of(&[TK::brkt_brace_open, TK::punct_fatArrow]) {
            self.parse_ty(*sig);
        }

        if func.eat(TK::punct_fatArrow) {
            func.push_token();
            self.parse_expr(*func);
            func.expect(TK::punct_semicolon);
        } else {
            let mut block = self.parse_expr_block();
            block.parent = Some(*func);
        }
    }

    // fn parse_decl(&mut self, contex: ParseContext, parent: &mut PNode) {
    //     let mut decl = self.node(NK::decl);
    //     self.expect(TK::punct_doubleColon, ParseContext::Decl, &mut decl);

    //     match self.peek() {
    //         TK::ident => {
    //             self.bump(&mut decl);
    //         }
    //         _ => todo!(),
    //     }

    //     self.append_node(decl, parent);
    // }

    // fn parse_decl(&mut self, context: ParseContext, parent: &mut PNode) {
    //     if let Some(decl) = self.try_parse_decl() {
    //         self.append_node(decl, parent);
    //     } else {
    //         self.report(
    //             parent,
    //             ParseError::Expected(ParseErrorExpected {
    //                 what: ParseErrorExpectedWhat::Item(ParseErrorItem::Decl),
    //                 at: self.cursor,
    //                 found: self.cursor,
    //                 context,
    //             }),
    //         )
    //     }
    // }

    // fn try_parse_decl(&mut self) -> Option<PNode> {
    //     let decl = match self.peek() {
    //         TK::kw_fn => {
    //             let mut func = self.node(NK::decl_func);
    //             self.bump(&mut func); // kw_fn
    //             self.bump(&mut func); // ident

    //             let mut proto = self.node(NK::func_proto);
    //             self.parse_func_params_and_ret(ParseContext::DeclFunc, &mut proto);
    //             self.append_node(proto, &mut func);

    //             let mut body = self.node(NK::decl_func_body);

    //             if self.eat(TK::punct_fat_arrow, &mut body) {
    //                 self.parse_stmt_expr(&mut body);
    //             } else {
    //                 let block = self.parse_block();
    //                 self.append_node(block, &mut body);
    //             }

    //             self.append_node(body, &mut func);
    //             func
    //         }
    //         TK::kw_const => {
    //             let mut konst = self.node(NK::decl_const);

    //             self.bump(&mut konst); // 'const'
    //             self.parse_binding(&mut konst);

    //             konst
    //         }
    //         _ => return None,
    //     };
    //     Some(decl)
    // }
}
