use wasm_bindgen::JsCast;
use web_sys::{window, Document, Element, HtmlElement, HtmlInputElement, Node, Window};

pub(crate) trait NodeUnchecked {
    fn append_child_unchecked(&self, child: &Node) -> Node;
}

impl NodeUnchecked for Node {
    fn append_child_unchecked(&self, child: &Node) -> Node {
        self.append_child(child).expect("append child must work")
    }
}

pub(crate) trait ElementUnchecked {
    fn set_attribute_unchecked(&self, name: &str, value: &str);
}

impl ElementUnchecked for Element {
    fn set_attribute_unchecked(&self, name: &str, value: &str) {
        self.set_attribute(name, value)
            .expect("set attribute must succeed");
    }
}

pub(crate) trait DocumentUnchecked {
    fn create_element_unchecked(&self, element: &str) -> Element;

    fn html_element_by_id_unchecked(&self, id: &str) -> HtmlElement;

    fn input_by_id_unchecked(&self, id: &str) -> HtmlInputElement {
        self.html_element_by_id_unchecked(id).unchecked_into()
    }

    fn create_div_unchecked(&self) -> HtmlElement {
        self.create_element_unchecked("div").unchecked_into()
    }
}

impl DocumentUnchecked for Document {
    fn create_element_unchecked(&self, element: &str) -> Element {
        self.create_element(element)
            .expect("create element must work")
    }

    fn html_element_by_id_unchecked(&self, id: &str) -> HtmlElement {
        self.get_element_by_id(id)
            .unwrap_or_else(|| {
                panic!(
                    "{}",
                    format_args!("element with id '{id}' must exist")
                        .as_str()
                        .unwrap()
                        .to_string()
                )
            })
            .unchecked_into()
    }
}

pub(crate) trait WindowUnchecked {
    fn document_unchecked(&self) -> Document;
}

impl WindowUnchecked for Window {
    fn document_unchecked(&self) -> Document {
        self.document().expect("document must exist")
    }
}

pub(crate) fn unchecked_now() -> f64 {
    window_unchecked()
        .performance()
        .expect("there shold be performance")
        .now()
}

pub(crate) fn document_unchecked() -> Document {
    let window = web_sys::window().expect("window must exist");
    window.document_unchecked()
}

pub(crate) fn window_unchecked() -> Window {
    window().expect("there should be a window")
}
