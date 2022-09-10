use super::*;

/// Parse decl
impl Parser<'_> {
    pub fn parse_decl(&mut self, parent: NodeId) {
        let mut decl = self.pnode(NK::decl).parent(parent);

        decl.expect(TK::punct_doubleColon);

        match decl.peek() {
            TK::ident => {
                decl.push_token();

                match decl.peek() {
                    TK::kw_fn => self.parse_decl_func(*decl),
                    TK::kw_struct => {
                        // @TODO
                        self.report_unimpl(|| Report::builder().message("struct declaration"));
                    }
                    TK::kw_enum => {
                        // @TODO
                        self.report_unimpl(|| Report::builder().message("enum declaration"));
                    }
                    TK::kw_union => {
                        // @TODO
                        self.report_unimpl(|| Report::builder().message("union declaration"));
                    }
                    TK::kw_trait => {
                        // @TODO
                        self.report_unimpl(|| Report::builder().message("trait declaration"));
                    }
                    TK::kw_mixin => {
                        // @TODO
                        self.report_unimpl(|| Report::builder().message("mixin declaration"));
                    }
                    TK::kw_class => {
                        // @TODO
                        self.report_unimpl(|| Report::builder().message("class declaration"));
                    }
                    _ => self.report(|| {
                        Report::builder().error().message(
                            "expected declcaration kind; 'fn', 'struct', 'enum', 'union', 'trait', 'mixin' or 'class'",
                        )
                    }),
                }
            }
            TK::kw_import => {
                // @TODO
                self.report_unimpl(|| Report::builder().message("import delcaration"));
            }
            TK::kw_impl => {
                // @TODO
                self.report_unimpl(|| Report::builder().message("impl declaration"));
            }
            TK::kw_module => {
                // @TODO
                self.report_unimpl(|| Report::builder().message("module declaration"));
            }
            _ => self.report(|| {
                Report::builder()
                    .error()
                    .message("expected declcaration; identifer, keyword 'import', keyword 'impl' or keyword 'module'")
            }),
        }
    }

    fn parse_decl_func(&mut self, parent: NodeId) {
        let mut func = self.pnode(NK::decl_func).parent(parent);

        {
            let mut sig = self.parse_ty_func_wo_return();
            sig.parent = Some(*func);

            if !func.at_one_of(&[TK::brkt_brace_open, TK::punct_fatArrow]) {
                self.parse_ty(*sig);
            }
        }

        if func.at(TK::punct_fatArrow) {
            let mut body = self.pnode(NK::decl_func_body).parent(*func);

            body.push_token();
            self.parse_expr(*body);
            body.expect(TK::punct_semicolon);
        } else {
            let body = self.pnode(NK::decl_func_body).parent(*func);
            let mut block = self.parse_expr_block();
            block.parent = Some(*body);
        }
    }
}
