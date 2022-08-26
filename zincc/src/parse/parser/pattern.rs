use super::*;

impl Parser<'_> {
    pub fn parse_pattern(&mut self, parent: NodeId) {
        let mut node = self.pnode(NK::err, parent);
        match node.peek() {
            TK::ident => {
                if node.peek_next() == TK::punct_doubleColon {
                    node.kind = NK::pattern_path;
                    let mut p = self.parse_path();
                    p.parent = Some(*node);
                } else {
                    node.kind = NK::pattern_ident;
                    node.push_token();
                }
            }
            _ => todo!(),
        }
    }
}
