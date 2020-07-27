macro_rules! add_type_error {
    ($self:ident, $fmt:expr, $loc:expr) => {{
        let err = format!($fmt);
        $self.ctx
            .error_ctx
            .add_error(MyteError::new(err, $loc, MyteErrorType::Type));
    }};
    ($self:ident, $fmt:expr, $arg1:expr, $loc:expr) => {{
        let formatted_types = InferType::format_types(&[$self.rep(&$arg1)]);
        let err = format!($fmt, formatted_types[0])
        $self.ctx
            .error_ctx
            .add_error(MyteError::new(err, $loc, MyteErrorType::Type));
    }};
    ($self:ident, $fmt:expr, $arg1:expr, $arg2:expr, $loc:expr) => {{
        let formatted_types = InferType::format_types(&[$self.rep($arg1), $self.rep($arg2)]);
        let err = format!($fmt, formatted_types[0], formatted_types[1]);
        $self.ctx
            .error_ctx
            .add_error(MyteError::new(err, $loc, MyteErrorType::Type));
    }};
}

macro_rules! add_formatted_type_error {
    ($self:ident, $fmt:expr, $name:expr, $arg1:expr, $arg2:expr, $loc:expr) => {{
        let formatted_types = InferType::format_types(&[$self.rep($arg1), $self.rep($arg2)]);
        let err = format!($fmt, $name, formatted_types[0], formatted_types[1]);
        $self
            .ctx
            .error_ctx
            .add_error(MyteError::new(err, $loc, MyteErrorType::Type));
    }};
}
