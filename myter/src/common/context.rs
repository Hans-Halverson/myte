use common::error::ErrorContext;
use common::ident::SymbolTable;
use common::source::FileTable;

#[derive(Clone)]
pub struct Context {
    pub symbol_table: SymbolTable,
    pub error_context: ErrorContext,
    pub file_table: FileTable,
}

impl Context {
    pub fn new() -> Context {
        Context {
            symbol_table: SymbolTable::new(),
            error_context: ErrorContext::new(),
            file_table: FileTable::new(),
        }
    }
}
