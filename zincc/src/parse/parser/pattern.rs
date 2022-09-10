use super::*;

impl Parser<'_> {
    /// # Returns
    /// true: ident
    /// false: not ident
    pub fn parse_pattern(&mut self, parent: NodeId) -> bool {
        let mut node = self.pnode(NK::err).parent(parent);
        match node.peek() {
            TK::ident => {
                if node.peek_next() == TK::punct_doubleColon {
                    node.kind = NK::pattern_path;
                    let mut p = self.parse_path();
                    p.parent = Some(*node);
                } else {
                    node.kind = NK::pattern_ident;
                    node.push_token();

                    return true;
                }
            }
            _ => todo!(),
        }

        false
    }
}
