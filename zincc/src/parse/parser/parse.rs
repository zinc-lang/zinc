use super::*;

/// Parse
impl Parser<'_> {
    // pub fn parse_top_level(&mut self) -> cst::NamedNodeId {
    //     let root = self.node_map.push(cst::Node::new());

    //     while self.peek() != TK::eof {
    //         self.parse_decl(root);
    //     }

    //     // @TODO: Should we append the EOF token? Doing so adds a lot of complexity.

    //     cst::NamedNodeId {
    //         kind: NK::root,
    //         raw: root,
    //     }
    // }

    pub fn parse_top_level(&mut self) {
        let root = self.cst.root;

        while self.peek() != TK::eof {
            self.parse_decl(root);
        }
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
}
