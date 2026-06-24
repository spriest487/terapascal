use crate::error::BuildError;
use crate::error::BuildResult;
use linked_hash_map::LinkedHashMap;
use std::env;
use std::fs;
use std::io::Read;
use std::iter;
use std::path::PathBuf;
use std::rc::Rc;
use terapascal_backend_c::ir;
use terapascal_common::build_log::BuildLog;
use terapascal_common::IR_LIB_EXT;
use terapascal_common::LIB_DIR_VAR;
use terapascal_frontend::codegen::library_builder::LibraryRef;
use terapascal_frontend::import::import_lib;
use terapascal_frontend::typ::Context;

pub struct LibraryLoader<'a> {
    lib_collection: LinkedHashMap<String, Rc<ir::Library>>,

    search_dirs: Vec<PathBuf>,

    log: &'a mut BuildLog,
}

impl<'a> LibraryLoader<'a> {
    pub fn new(search_paths: impl IntoIterator<Item=PathBuf>, log: &'a mut BuildLog) -> Self {
        Self {
            lib_collection: LinkedHashMap::new(),
            search_dirs: search_paths.into_iter().collect(),
            log,
        }
    }

    fn to_collection(&self, main: &str) -> BuildResult<LibraryCollection> {
        let Some(main_lib) = self.lib_collection.get(main) else {
            let msg = format!("missing main library in collection: {main}");
            return Err(BuildError::InternalError(msg));
        };

        let mut ref_libs = Vec::new();
        for ref_name in &main_lib.references {
            let Some(ref_lib) = self.lib_collection.get(ref_name) else {
                let msg = format!("missing referenced library in collection: {ref_name}");
                return Err(BuildError::InternalError(msg));
            };

            ref_libs.push(ref_lib.clone());
        }

        Ok(LibraryCollection {
            main: main_lib.clone(),
            refs: ref_libs,
        })
    }

    pub fn load_lib_file(
        &mut self,
        path: impl Into<PathBuf>,
    ) -> BuildResult<LibraryCollection> {
        let (lib_name, _) = self.load_lib_file_rec(path)?;

        self.to_collection(&lib_name)
    }

    pub fn load_lib(
        &mut self,
        name: impl Into<String>,
    ) -> BuildResult<LibraryCollection> {
        let name = name.into();

        self.load_libs_rec(&name)?;

        self.to_collection(&name)
    }

    fn load_lib_file_rec(
        &mut self,
        path: impl Into<PathBuf>,
    ) -> BuildResult<(String, Vec<String>)> {
        let mut loaded_libs = Vec::new();

        let path = path.into();
        let mut file = fs::File::open(&path)?;

        let mut lib_bytes = Vec::new();
        file.read_to_end(&mut lib_bytes)?;

        let library = ir::decode_lib(&lib_bytes)
            .map_err(|err| {
                BuildError::ReadSourceFileFailed {
                    msg: format!("deserialization failed: {err}"),
                    path,
                }
            })?;

        // we don't know the library name until we decode it - now check if it's already loaded
        if self.lib_collection.contains_key(&library.name) {
            self.log.trace("load_lib_file_rec: library {} was already loaded");
            return Ok((library.name.clone(), Vec::new()));
        }

        for ref_name in &library.references {
            if self.lib_collection.contains_key(ref_name) {
                continue;
            }

            let (ref_name, transitive_refs) = self.load_libs_rec(ref_name)?;
            loaded_libs.push(ref_name);
            loaded_libs.extend(transitive_refs);
        }

        let lib_name = library.name.clone();

        self.lib_collection.insert(lib_name.clone(), Rc::new(library));

        Ok((lib_name, loaded_libs))
    }

    fn load_libs_rec(
        &mut self,
        name: impl Into<String>,
    ) -> BuildResult<(String, Vec<String>)> {
        let name = name.into();

        let mut search_dirs = self.search_dirs.to_vec();

        if let Ok(lib_path) = env::var(LIB_DIR_VAR) {
            search_dirs.push(PathBuf::from(lib_path));
        }

        let path = search_dirs
            .iter()
            .find_map(|dir| {
                let full_path = dir.join(&name).with_added_extension(IR_LIB_EXT);

                self.log.trace(format!("searching for library {} at {}", name, full_path.display()));

                full_path.exists().then_some(full_path)
            })
            .ok_or_else(|| {
                BuildError::FileNotFound(PathBuf::from(&name).with_added_extension(IR_LIB_EXT), None)
            })?;

        self.load_lib_file_rec(path)
    }

    pub fn import_package(
        &mut self,
        name: &str,
        type_ctx: Option<&mut Context>,
    ) -> BuildResult<Vec<LibraryRef>> {
        if self.lib_collection.contains_key(name) {
            return Ok(Vec::new());
        }

        let (main_name, mut loaded_libs) = self.load_libs_rec(name)?;
        loaded_libs.push(main_name);

        let mut imported_libs = Vec::with_capacity(loaded_libs.len());

        match type_ctx {
            Some(ctx) => {
                for lib_name in &loaded_libs {
                    let loaded_lib = self.lib_collection[lib_name].clone();

                    let loaded_refs = loaded_lib.references
                        .iter()
                        .map(|ref_name| self.lib_collection[ref_name].as_ref());

                    let output = import_lib(&loaded_lib, loaded_refs, Some(ctx))?;

                    for warning in output.warnings {
                        self.log.diagnostic(warning);
                    }

                    imported_libs.push(LibraryRef {
                        lib: loaded_lib,
                        imported_funcs: output.imported_funcs,
                        imported_namespaces: output.namespaces,
                    });
                }
            }

            None => {
                for lib_name in &loaded_libs {
                    let loaded_lib = self.lib_collection[lib_name].clone();

                    let loaded_refs = loaded_lib.references
                        .iter()
                        .map(|ref_name| self.lib_collection[ref_name].as_ref());

                    let output = import_lib(&loaded_lib, loaded_refs, None)?;

                    for warning in output.warnings {
                        self.log.diagnostic(warning);
                    }

                    imported_libs.push(LibraryRef {
                        lib: loaded_lib,
                        imported_funcs: output.imported_funcs,
                        imported_namespaces: output.namespaces,
                    });
                }
            }
        }

        Ok(imported_libs)
    }
}

pub struct LibraryCollection {
    pub main: Rc<ir::Library>,
    pub refs: Vec<Rc<ir::Library>>,
}

impl LibraryCollection {
    pub fn merge(&self) -> ir::Library {
        let mut merged = self.main.as_ref().clone();

        for ref_lib in self.refs.iter() {
            merged.merge_from(ref_lib.as_ref());
        }

        merged
    }

    pub fn to_metadata_builder(&self) -> ir::MetadataBuilder {
        ir::MetadataBuilder::with_refs(self.refs.iter()
            .map(|r| r.metadata.clone())
            .chain(iter::once(self.main.metadata.clone())))
    }

    pub fn iter(&self) -> impl Iterator<Item=&ir::Library> {
        self.refs
            .iter()
            .map(|r| r.as_ref())
            .chain(iter::once(self.main.as_ref()))
    }
}