use std::fmt;

/// Simplified VDOM node for tracking page state
#[derive(Clone, Debug, PartialEq)]
pub enum VdomNode {
    Text(String),
    Element {
        tag: String,
        children: Vec<VdomNode>,
    },
}

/// A patch describing a change between two VDOM states
#[derive(Clone, Debug)]
#[allow(dead_code)]
pub enum DomPatch {
    ReplaceText { path: Vec<usize>, new_text: String },
    InsertChild { path: Vec<usize>, index: usize, node: VdomNode },
    RemoveChild { path: Vec<usize>, index: usize },
    ReplaceNode { path: Vec<usize>, node: VdomNode },
}

/// Maintains the server-side VDOM state and computes diffs
pub struct VdomState {
    current: Option<VdomNode>,
}

impl VdomState {
    pub fn new() -> Self {
        Self { current: None }
    }

    /// Update the VDOM with new content and return patches
    pub fn update(&mut self, new_tree: VdomNode) -> Vec<DomPatch> {
        let patches = match &self.current {
            Some(old) => Self::diff(old, &new_tree, &mut vec![]),
            None => vec![DomPatch::ReplaceNode {
                path: vec![],
                node: new_tree.clone(),
            }],
        };
        self.current = Some(new_tree);
        patches
    }

    /// Compute diff between old and new VDOM trees
    fn diff(old: &VdomNode, new: &VdomNode, path: &mut Vec<usize>) -> Vec<DomPatch> {
        let mut patches = Vec::new();

        match (old, new) {
            (VdomNode::Text(old_text), VdomNode::Text(new_text)) => {
                if old_text != new_text {
                    patches.push(DomPatch::ReplaceText {
                        path: path.clone(),
                        new_text: new_text.clone(),
                    });
                }
            }
            (
                VdomNode::Element {
                    tag: old_tag,
                    children: old_children,
                },
                VdomNode::Element {
                    tag: new_tag,
                    children: new_children,
                },
            ) => {
                if old_tag != new_tag {
                    patches.push(DomPatch::ReplaceNode {
                        path: path.clone(),
                        node: new.clone(),
                    });
                    return patches;
                }

                let min_len = old_children.len().min(new_children.len());

                // Diff common children
                for i in 0..min_len {
                    path.push(i);
                    patches.extend(Self::diff(&old_children[i], &new_children[i], path));
                    path.pop();
                }

                // Handle added children
                for i in min_len..new_children.len() {
                    patches.push(DomPatch::InsertChild {
                        path: path.clone(),
                        index: i,
                        node: new_children[i].clone(),
                    });
                }

                // Handle removed children
                for i in (min_len..old_children.len()).rev() {
                    patches.push(DomPatch::RemoveChild {
                        path: path.clone(),
                        index: i,
                    });
                }
            }
            _ => {
                // Type mismatch — full replace
                patches.push(DomPatch::ReplaceNode {
                    path: path.clone(),
                    node: new.clone(),
                });
            }
        }

        patches
    }

    /// Serialize patches to bytes for compression
    pub fn serialize_patches(patches: &[DomPatch]) -> Vec<u8> {
        // Simple binary serialization of patches
        let json = serde_json_minimal(patches);
        json.into_bytes()
    }
}

/// Minimal JSON-like serialization without pulling in serde
fn serde_json_minimal(patches: &[DomPatch]) -> String {
    let mut out = String::from("[");
    for (i, patch) in patches.iter().enumerate() {
        if i > 0 {
            out.push(',');
        }
        match patch {
            DomPatch::ReplaceText { path, new_text } => {
                out.push_str(&format!(
                    r#"{{"type":"text","path":{:?},"text":"{}"}}"#,
                    path,
                    new_text.replace('"', "\\\"")
                ));
            }
            DomPatch::InsertChild { path, index, .. } => {
                out.push_str(&format!(
                    r#"{{"type":"insert","path":{:?},"index":{}}}"#,
                    path, index
                ));
            }
            DomPatch::RemoveChild { path, index } => {
                out.push_str(&format!(
                    r#"{{"type":"remove","path":{:?},"index":{}}}"#,
                    path, index
                ));
            }
            DomPatch::ReplaceNode { path, .. } => {
                out.push_str(&format!(
                    r#"{{"type":"replace","path":{:?}}}"#,
                    path
                ));
            }
        }
    }
    out.push(']');
    out
}

impl fmt::Display for VdomNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VdomNode::Text(t) => write!(f, "{}", t),
            VdomNode::Element { tag, children } => {
                write!(f, "<{}>", tag)?;
                for child in children {
                    write!(f, "{}", child)?;
                }
                write!(f, "</{}>", tag)
            }
        }
    }
}
