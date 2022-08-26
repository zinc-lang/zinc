use super::*;

/// Parse
impl Parser<'_> {
    pub fn parse_top_level(&mut self) -> cst::NamedNodeId {
        // let mut root = self.pnode_np(NK::root);

        let mut root = std::mem::ManuallyDrop::new(self.pnode_np(NK::root));

        while !root.at_end() {
            self.parse_decl(**root);
        }

        root.push_token();

        cst::NamedNodeId {
            kind: root.kind,
            raw: root.node,
        }

        // do not call root.drop()
    }

    pub fn parse_path(&mut self) -> PNode {
        let mut path = self.pnode_np(NK::path);

        path.expect(TK::ident);

        while path.at(TK::punct_doubleColon) {
            path.expect(TK::punct_doubleColon);
            path.expect(TK::ident);
        }

        path
    }

    pub fn parse_string(&mut self) -> PNode {
        let mut str = self.pnode_np(NK::string);

        str.expect(TK::string_open);

        while str.eat_one_of(&[
            TK::string_literal,
            TK::esc_asciicode,
            TK::esc_unicode,
            TK::esc_asciicode,
            TK::esc_unicode,
            TK::esc_char_newline,
            TK::esc_char_return,
            TK::esc_char_tab,
            TK::esc_char_backslash,
            TK::esc_char_doubleQuote,
            TK::esc_char_singleQuote,
            TK::esc_char_other,
        ]) {}
        str.expect(TK::string_close);

        str
    }

    pub fn parse_int(&mut self) -> PNode {
        let mut int = self.pnode_np(NK::integer);
        int.expect_one_of(&[TK::int_dec, TK::int_bin, TK::int_hex, TK::int_oct]);
        int
    }

    pub fn parse_float(&mut self) -> PNode {
        let mut float = self.pnode_np(NK::float);
        float.expect(TK::float);
        float
    }

    pub fn parse_bool(&mut self) -> PNode {
        let mut bool = self.pnode_np(NK::boolean);
        bool.expect_one_of(&[TK::kw_true, TK::kw_false]);
        bool
    }

    // /// 'let's and 'const's
    // fn parse_binding(&mut self, binding: &mut PNode) {
    //     // ident
    //     self.expect(TK::ident, ParseContext::DeclConst, binding);

    //     // ( ':' ty )?
    //     if self.eat(TK::punct_colon, binding) {
    //         let mut ty = self.node(NK::binding_ty);
    //         self.parse_ty(&mut ty);
    //         self.append_node(ty, binding);
    //     }

    //     // '=' expr
    //     self.expect(TK::punct_eq, ParseContext::DeclConst, binding);
    //     self.parse_stmt_expr(binding);
    // }
}
