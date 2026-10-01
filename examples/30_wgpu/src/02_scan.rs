//! Rekursiver Dateisystem-Scan: baut den [`Node`]-Baum mit Größen auf.
//!
//! Regeln: symbolische Links werden ignoriert, virtuelle Dateisysteme
//! (`/proc`, `/sys`, `/dev`) übersprungen, unlesbare Einträge nach
//! stderr gemeldet und ausgelassen.

use std::fs;
use std::path::Path;

use crate::color::{DIR_COLOR, color_for_path};
use crate::types::{Node, Rect};

/// Obergrenze für plausible Dateigrößen (verhindert Absurditäten aus
/// Spezialdateien); entspricht der MVP-Regel `size < 1 << 48`.
const MAX_FILE_SIZE: u64 = 1 << 48;

/// Meldet einen übersprungenen Eintrag nach stderr.
fn report_skip(context: &str, path: &Path, err: &std::io::Error) {
    eprintln!("skip [{context}]: {}: {err:?}", path.display());
}

/// True für virtuelle Dateisysteme ohne sinnvolle Größenangaben.
fn is_virtual_fs(path: &Path) -> bool {
    path.starts_with("/proc") || path.starts_with("/sys") || path.starts_with("/dev")
}

/// Scannt `path` rekursiv. `size` ist die Summe aller enthaltenen Dateien.
pub fn scan_tree(path: &Path) -> Node {
    let mut node = Node {
        path: path.to_path_buf(),
        size: 0,
        is_dir: true,
        children: Vec::new(),
        rect: Rect::default(),
        color: DIR_COLOR,
    };
    if is_virtual_fs(path) {
        return node;
    }
    match fs::read_dir(path) {
        Ok(entries) => {
            for entry in entries {
                match entry {
                    Ok(entry) => scan_entry(&mut node, &entry),
                    Err(err) => report_skip("entry", path, &err),
                }
            }
        }
        Err(err) => report_skip("read_dir", path, &err),
    }
    node
}

/// Fügt einen Verzeichniseintrag in `node` ein (Datei oder Unterverzeichnis).
fn scan_entry(node: &mut Node, entry: &fs::DirEntry) {
    let file_type = match entry.file_type() {
        Ok(ft) => ft,
        Err(err) => {
            report_skip("file_type", &entry.path(), &err);
            return;
        }
    };
    if file_type.is_symlink() {
        return;
    }
    let entry_path = entry.path();
    if file_type.is_dir() {
        let child = scan_tree(&entry_path);
        if child.size > 0 {
            node.size += child.size;
            node.children.push(child);
        }
    } else if file_type.is_file() {
        let size = match entry.metadata() {
            Ok(meta) => meta.len(),
            Err(err) => {
                report_skip("metadata", &entry_path, &err);
                0
            }
        };
        if size > 0 && size < MAX_FILE_SIZE {
            node.size += size;
            node.children.push(Node {
                path: entry_path.clone(),
                size,
                is_dir: false,
                children: Vec::new(),
                rect: Rect::default(),
                color: color_for_path(&entry_path),
            });
        }
    }
}

/// Zählt Knoten im Baum (für Diagnose/Tests).
pub fn count_nodes(node: &Node) -> usize {
    1 + node.children.iter().map(count_nodes).sum::<usize>()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;
    use std::sync::atomic::{AtomicU64, Ordering};

    /// Prüft, dass jede Verzeichnisgröße der Kindersumme entspricht.
    fn sizes_consistent(node: &Node) -> bool {
        if node.is_dir {
            let sum: u64 = node.children.iter().map(|c| c.size).sum();
            if sum != node.size {
                return false;
            }
        }
        node.children.iter().all(sizes_consistent)
    }

    static FIXTURE_SEQ: AtomicU64 = AtomicU64::new(0);

    /// Legt einen Fixture-Baum an, gibt das Wurzelverzeichnis zurück.
    /// Struktur: `root/a.txt` (100 B), `root/sub/b.rs` (300 B), `root/sub/c.bin` (600 B),
    /// `root/empty/` (leer), `root/link` (Symlink, muss ignoriert werden).
    fn fixture_tree() -> PathBuf {
        let id = FIXTURE_SEQ.fetch_add(1, Ordering::SeqCst);
        let root =
            std::env::temp_dir().join(format!("treemap-scan-test-{}-{id}", std::process::id()));
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(root.join("sub")).unwrap();
        fs::create_dir_all(root.join("empty")).unwrap();
        fs::write(root.join("a.txt"), vec![b'x'; 100]).unwrap();
        fs::write(root.join("sub").join("b.rs"), vec![b'y'; 300]).unwrap();
        fs::write(root.join("sub").join("c.bin"), vec![b'z'; 600]).unwrap();
        #[cfg(unix)]
        std::os::unix::fs::symlink(root.join("a.txt"), root.join("link")).unwrap();
        root
    }

    #[test]
    fn scan_sums_files_and_dirs() {
        let root = fixture_tree();
        let tree = scan_tree(&root);
        assert_eq!(tree.size, 1000);
        assert!(tree.is_dir);
        assert!(sizes_consistent(&tree));
        // a.txt + sub (leeres Verzeichnis und Symlink entfallen).
        assert_eq!(tree.children.len(), 2);
        let sub = tree.children.iter().find(|c| c.is_dir).unwrap();
        assert_eq!(sub.size, 900);
        assert_eq!(sub.children.len(), 2);
        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn scan_missing_dir_is_empty() {
        let tree = scan_tree(Path::new("/nonexistent-treemap-dir-xyz"));
        assert_eq!(tree.size, 0);
        assert!(tree.children.is_empty());
    }

    #[test]
    fn virtual_fs_is_skipped() {
        assert!(is_virtual_fs(Path::new("/proc/self")));
        assert!(is_virtual_fs(Path::new("/sys")));
        assert!(!is_virtual_fs(Path::new("/tmp")));
        assert!(!is_virtual_fs(Path::new("/workspace")));
    }

    #[test]
    fn root_uses_dir_color() {
        let root = fixture_tree();
        let tree = scan_tree(&root);
        assert_eq!(tree.color, DIR_COLOR);
        fs::remove_dir_all(&root).unwrap();
    }
}
