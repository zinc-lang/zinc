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
}
