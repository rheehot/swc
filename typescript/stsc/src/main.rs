use std::sync::Arc;
use swc_common::{
    errors::{ColorConfig, Handler},
    SourceMap,
};
use swc_ecma_parser::TsConfig;
use swc_ts_checker::{Checker, Rule};

fn main() {
    let cm: Arc<SourceMap> = Default::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(cm.clone()));

    let c = Checker::new(
        cm,
        &handler,
        vec![],
        Rule {
            ..Default::default()
        },
        TsConfig {
            decorators: true,
            tsx: true,
            dynamic_import: true,
        },
    );
}
