use std::any::Any;
use swc_visit::define;

/// Visitable nodes.
pub trait Node: Any {}

impl<T: ?Sized> Node for T where T: Any {}

pub struct Item {
    pub opt_box1: Option<Vec<Box<Item>>>,
    pub opt_box_conflict: Option<Vec<Item>>,
    pub opt_box2: Option<Box<Enum>>,
}
pub enum Enum {
    Item(Item),
}

define!({
    pub struct Item {
        pub opt_box1: Option<Vec<Box<Item>>>,
        pub opt_box_conflict: Option<Vec<Item>>,
        pub opt_box2: Option<Box<Enum>>,
    }
    pub enum Enum {
        Item(Item),
    }
});
