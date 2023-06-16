use wasm_bindgen::{JsCast, JsValue};
use web_sys::{
    window, Document, Element, EventTarget, HtmlElement, HtmlInputElement, Node, Url, Window,
    Worker,
};

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

    fn body_unchecked(&self) -> HtmlElement;
}

impl DocumentUnchecked for Document {
    fn create_element_unchecked(&self, element: &str) -> Element {
        self.create_element(element)
            .expect("create element must work")
    }

    fn html_element_by_id_unchecked(&self, id: &str) -> HtmlElement {
        self.get_element_by_id(id)
            .unwrap_or_else(|| panic!("{}", format_args!("element with id '{id}' must exist")))
            .unchecked_into()
    }

    fn body_unchecked(&self) -> HtmlElement {
        self.query_selector("body")
            .expect("query selector must succeed")
            .expect("there must be a body")
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

pub(crate) fn document_unchecked() -> Document {
    let window = web_sys::window().expect("window must exist");
    window.document_unchecked()
}

pub(crate) fn window_unchecked() -> Window {
    window().expect("there should be a window")
}

pub(crate) trait UrlUnchecked {
    fn new_unchecked(url: &str) -> Url;
}

impl UrlUnchecked for Url {
    fn new_unchecked(url: &str) -> Url {
        Url::new(url).expect("new URL should succeed")
    }
}

pub(crate) trait WorkerUnchecked {
    fn post_message_unchecked(&self, message: &JsValue);
}

impl WorkerUnchecked for Worker {
    fn post_message_unchecked(&self, message: &JsValue) {
        self.post_message(message)
            .expect("post message should succeed");
    }
}

pub(crate) trait EventTargetUnchecked {
    fn add_event_listener_with_callback_unchecked(
        &self,
        type_: &str,
        listener: &::js_sys::Function,
    );
    fn remove_event_listener_with_callback_unchecked(
        &self,
        type_: &str,
        listener: &::js_sys::Function,
    );
}

impl EventTargetUnchecked for EventTarget {
    fn add_event_listener_with_callback_unchecked(&self, type_: &str, listener: &js_sys::Function) {
        self.add_event_listener_with_callback(type_, listener)
            .expect("add event listener should succeed")
    }

    fn remove_event_listener_with_callback_unchecked(
        &self,
        type_: &str,
        listener: &js_sys::Function,
    ) {
        self.remove_event_listener_with_callback(type_, listener)
            .expect("remove event listener should succeed")
    }
}
