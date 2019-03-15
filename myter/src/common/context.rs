use common::error::ErrorContext;
use common::ident::SymbolTable;
use common::source::FileTable;

use ir::nodes::IrContext;
use types::infer::InferContext;

#[derive(Clone)]
pub struct Context {
    pub symbol_table: SymbolTable,
    pub error_ctx: ErrorContext,
    pub file_table: FileTable,
    pub ir_ctx: IrContext,
    pub infer_ctx: InferContext,
}

impl Context {
    pub fn new() -> Context {
        Context {
            symbol_table: SymbolTable::new(),
            error_ctx: ErrorContext::new(),
            file_table: FileTable::new(),
            ir_ctx: IrContext::new(),
            infer_ctx: InferContext::new(),
        }
    }

    pub fn new_for_repl() -> Context {
        Context {
            symbol_table: SymbolTable::new_for_repl(),
            error_ctx: ErrorContext::new(),
            file_table: FileTable::new(),
            ir_ctx: IrContext::new(),
            infer_ctx: InferContext::new(),
        }
    }
}
