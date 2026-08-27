/**
 * DOM reference renderer for the NaijaScript UI protocol.
 *
 * This maps the renderer-neutral primitives to concrete DOM elements and
 * mirrors patch transactions onto the live DOM incrementally. The retained
 * `RetainedTree` remains the logical source of truth; a parallel id -> DOM
 * map keeps the rendered nodes in lockstep.
 *
 * Security posture (see `docs/spec/UI_IR_PROTOCOL.md`): text is only ever
 * written through text-node / property APIs, never `innerHTML`. Image
 * assets are surfaced as an opaque `data-asset` attribute rather than a
 * network `src`, deferring URL resolution to a host allowlist.
 *
 * Event posture: renderers emit *data*, never callbacks across the
 * boundary. DOM listeners look up the node's current handler binding in the
 * retained tree and, if bound, hand a plain `UiEvent` object to the emit
 * callback supplied by the host.
 */

import type {
  ElementNode,
  EventKind,
  Node,
  NodeId,
  Primitive,
  PropValue,
  TextNode,
  UiEvent,
  UiEventPayload,
  MountDoc,
} from "./protocol.js";
import {
  PatchError,
  RetainedTree,
  buildTreeFromMount,
} from "./tree.js";
import type { PatchDoc } from "./protocol.js";

/** A host-supplied sink for emitted UI events. */
export type EmitFn = (event: UiEvent) => void;

/** DOM node produced for a protocol node (element -> HTMLElement, text -> Text). */
type DomNode = HTMLElement | Text;

/** Maps a primitive to the tag name used to render it. */
export function tagFor(primitive: Primitive): keyof HTMLElementTagNameMap {
  switch (primitive) {
    case "stack":
      return "div";
    case "row":
      return "div";
    case "scroll":
      return "div";
    case "label":
      return "span";
    case "button":
      return "button";
    case "input":
      return "input";
    case "image":
      return "img";
    case "spacer":
      return "div";
  }
}

/**
 * A live DOM renderer bound to one mounted session. Construct it with a
 * mount document; it builds the initial DOM under `container` and then
 * accepts patches via `applyPatch`.
 */
export class DomRenderer {
  /** Logical retained tree; the source of truth for structure and handlers. */
  readonly tree: RetainedTree;

  private readonly container: HTMLElement;
  private readonly emit: EmitFn;
  private readonly domById: Map<NodeId, DomNode> = new Map();

  constructor(container: HTMLElement, mount: MountDoc, emit: EmitFn) {
    this.container = container;
    this.emit = emit;
    this.tree = buildTreeFromMount(mount);
    const rootDom = this.createDom(this.tree.root);
    this.container.appendChild(rootDom);
  }

  /** Resolve the DOM node for an id, or throw. */
  domFor(id: NodeId): DomNode {
    const dom = this.domById.get(id);
    if (dom === undefined) {
      throw new PatchError(`no DOM node for id ${id}`);
    }
    return dom;
  }

  /** Build (and index) the DOM subtree for a protocol node. */
  private createDom(node: Node): DomNode {
    if (node.kind === "text") {
      const textDom = document.createTextNode(node.text);
      this.domById.set(node.id, textDom);
      return textDom;
    }
    const el = document.createElement(tagFor(node.primitive));
    el.dataset.uiId = node.id;
    el.dataset.primitive = node.primitive;
    this.applyBaseStyle(el, node.primitive);
    for (const [name, value] of Object.entries(node.props)) {
      this.applyProp(el, node.primitive, name, value);
    }
    this.wireEvents(el, node);
    this.domById.set(node.id, el);
    for (const child of node.children) {
      el.appendChild(this.createDom(child));
    }
    return el;
  }

  /** Baseline flexbox / overflow styling per primitive. */
  private applyBaseStyle(el: HTMLElement, primitive: Primitive): void {
    switch (primitive) {
      case "stack":
        el.style.display = "flex";
        el.style.flexDirection = "column";
        break;
      case "row":
        el.style.display = "flex";
        el.style.flexDirection = "row";
        break;
      case "scroll":
        el.style.overflow = "auto";
        break;
      case "spacer":
        el.style.flexGrow = "1";
        break;
      default:
        break;
    }
  }

  /**
   * Apply one property to an element. Known semantic names get a real DOM
   * effect; everything else is surfaced as a `data-*` attribute so it stays
   * observable without inventing behavior.
   */
  private applyProp(
    el: HTMLElement,
    primitive: Primitive,
    name: string,
    value: PropValue,
  ): void {
    switch (value.type) {
      case "text": {
        if (primitive === "button" && name === "label") {
          el.textContent = value.value;
        } else if (primitive === "input" && name === "value") {
          (el as HTMLInputElement).value = value.value;
        } else if (name === "placeholder") {
          el.setAttribute("placeholder", value.value);
        } else if (name === "a11yLabel") {
          el.setAttribute("aria-label", value.value);
        } else if (primitive === "label" && name === "text") {
          el.textContent = value.value;
        } else {
          el.dataset[name] = value.value;
        }
        break;
      }
      case "number": {
        if (name === "size") {
          el.style.fontSize = `${value.value}px`;
        } else {
          el.dataset[name] = String(value.value);
        }
        break;
      }
      case "boolean": {
        if (name === "enabled") {
          (el as HTMLButtonElement | HTMLInputElement).disabled = !value.value;
        } else {
          el.dataset[name] = String(value.value);
        }
        break;
      }
      case "key": {
        el.dataset[name] = value.value;
        break;
      }
      case "color": {
        const rgba = `rgba(${value.r}, ${value.g}, ${value.b}, ${value.a})`;
        if (name === "tone") {
          el.style.color = rgba;
        } else if (name === "background") {
          el.style.backgroundColor = rgba;
        } else {
          el.dataset[name] = rgba;
        }
        break;
      }
      case "length": {
        const px = `${value.px}px`;
        if (name === "spacing" || name === "gap") {
          el.style.gap = px;
        } else if (name === "padding") {
          el.style.padding = px;
        } else if (name === "size") {
          el.style.flexBasis = px;
        } else {
          el.dataset[name] = px;
        }
        break;
      }
      case "image": {
        el.dataset.asset = value.asset;
        break;
      }
      case "enum": {
        this.applyEnum(el, primitive, name, value.enum, value.case);
        break;
      }
      default: {
        const exhaustive: never = value;
        throw new PatchError(`unknown prop value: ${JSON.stringify(exhaustive)}`);
      }
    }
  }

  /** Map Axis / Alignment enums onto flexbox properties. */
  private applyEnum(
    el: HTMLElement,
    primitive: Primitive,
    name: string,
    enumType: string,
    enumCase: string,
  ): void {
    el.dataset[name] = `${enumType}.${enumCase}`;
    if (enumType === "Axis") {
      const vertical = enumCase === "vertical";
      if (primitive === "scroll") {
        el.style.overflowY = vertical ? "auto" : "hidden";
        el.style.overflowX = vertical ? "hidden" : "auto";
      } else {
        el.style.flexDirection = vertical ? "column" : "row";
      }
    } else if (enumType === "Alignment") {
      const map: Record<string, string> = {
        start: "flex-start",
        center: "center",
        end: "flex-end",
        stretch: "stretch",
      };
      el.style.alignItems = map[enumCase] ?? "stretch";
    }
  }

  /** Remove a property's DOM effect. */
  private removeProp(el: HTMLElement, name: string): void {
    if (name in el.dataset) {
      delete el.dataset[name];
    }
    if (name === "label" || name === "text") {
      el.textContent = "";
    } else if (name === "placeholder") {
      el.removeAttribute("placeholder");
    } else if (name === "a11yLabel") {
      el.removeAttribute("aria-label");
    } else if (name === "enabled") {
      (el as HTMLButtonElement | HTMLInputElement).disabled = false;
    } else if (name === "spacing" || name === "gap") {
      el.style.removeProperty("gap");
    } else if (name === "padding") {
      el.style.removeProperty("padding");
    } else if (name === "tone" || name === "size") {
      el.style.removeProperty("color");
      el.style.removeProperty("font-size");
    }
  }

  /**
   * Attach DOM listeners that translate user interaction into typed
   * `UiEvent` data. Listeners consult the retained tree at fire time, so
   * later bind/unbind patches take effect without re-wiring the DOM.
   */
  private wireEvents(el: HTMLElement, node: ElementNode): void {
    const id = node.id;
    if (node.primitive === "button") {
      el.addEventListener("click", () => {
        this.fire(id, "tap", { kind: "tap" });
      });
    } else if (node.primitive === "input") {
      el.addEventListener("input", () => {
        this.fire(id, "change", {
          kind: "change",
          value: (el as HTMLInputElement).value,
        });
      });
      el.addEventListener("keydown", (ev) => {
        if ((ev as KeyboardEvent).key === "Enter") {
          this.fire(id, "submit", {
            kind: "submit",
            value: (el as HTMLInputElement).value,
          });
        }
      });
      el.addEventListener("focus", () => {
        this.fire(id, "focus", { kind: "focus", focused: true });
      });
      el.addEventListener("blur", () => {
        this.fire(id, "focus", { kind: "focus", focused: false });
      });
    }
  }

  /** Emit a typed event if the node currently has a binding for `kind`. */
  private fire(id: NodeId, kind: EventKind, payload: UiEventPayload): void {
    if (!this.tree.has(id)) {
      return;
    }
    const node = this.tree.get(id);
    if (node.kind !== "element") {
      return;
    }
    const binding = node.handlers[kind];
    if (binding === undefined) {
      return;
    }
    this.emit({
      session_id: this.tree.sessionId,
      node_id: id,
      handler_id: binding.id,
      handler_generation: binding.generation,
      event: payload,
    });
  }

  /**
   * Apply a patch transaction to both the retained tree and the live DOM.
   * Validation matches `applyPatch` in `tree.ts`. Each op mirrors onto the
   * DOM before the logical tree mutates, using the pre-mutation structure to
   * resolve subtrees for removal.
   */
  applyPatch(patch: PatchDoc): void {
    if (patch.status !== "ok") {
      throw new PatchError(
        `refusing to apply non-ok patch (status=${patch.status})`,
      );
    }
    if (patch.session_id !== this.tree.sessionId) {
      throw new PatchError(
        `session mismatch: patch ${patch.session_id} vs tree ${this.tree.sessionId}`,
      );
    }
    if (patch.target_revision <= patch.base_revision) {
      throw new PatchError(
        `target_revision ${patch.target_revision} must exceed base_revision ${patch.base_revision}`,
      );
    }
    if (patch.base_revision !== this.tree.revision) {
      throw new PatchError(
        `base_revision ${patch.base_revision} does not match tree revision ${this.tree.revision}`,
      );
    }
    for (const op of patch.operations) {
      this.applyDomOp(op);
      this.tree.applyOp(op);
    }
    this.tree.revision = patch.target_revision;
  }

  private applyDomOp(op: PatchDoc["operations"][number]): void {
    switch (op.op) {
      case "insert": {
        const parent = this.domFor(op.parent) as HTMLElement;
        const newDom = this.createDom(op.node);
        const ref = parent.childNodes[op.index] ?? null;
        parent.insertBefore(newDom, ref);
        break;
      }
      case "remove": {
        const dom = this.domFor(op.node);
        this.forgetSubtree(this.tree.get(op.node));
        dom.parentNode?.removeChild(dom);
        break;
      }
      case "move": {
        const dom = this.domFor(op.node);
        dom.parentNode?.removeChild(dom);
        const parent = this.domFor(op.parent) as HTMLElement;
        const ref = parent.childNodes[op.index] ?? null;
        parent.insertBefore(dom, ref);
        break;
      }
      case "replace": {
        const oldDom = this.domFor(op.node);
        this.forgetSubtree(this.tree.get(op.node));
        const newDom = this.createDom(op.replacement);
        oldDom.parentNode?.replaceChild(newDom, oldDom);
        break;
      }
      case "set_property": {
        const el = this.domFor(op.node) as HTMLElement;
        const node = this.tree.get(op.node);
        const primitive =
          node.kind === "element" ? node.primitive : "label";
        this.applyProp(el, primitive, op.property, op.value);
        break;
      }
      case "remove_property": {
        const el = this.domFor(op.node) as HTMLElement;
        this.removeProp(el, op.property);
        break;
      }
      case "set_text": {
        const dom = this.domFor(op.node);
        if (dom instanceof Text) {
          dom.data = op.text;
        }
        break;
      }
      case "bind_handler":
      case "unbind_handler": {
        // Handler wiring is resolved at fire time from the retained tree, so
        // there is nothing to mutate on the DOM here.
        break;
      }
      default: {
        const exhaustive: never = op;
        throw new PatchError(`unknown op: ${JSON.stringify(exhaustive)}`);
      }
    }
  }

  /** Drop a subtree's nodes from the id -> DOM index. */
  private forgetSubtree(node: Node): void {
    this.domById.delete(node.id);
    if (node.kind === "element") {
      for (const child of node.children) {
        this.forgetSubtree(child);
      }
    }
  }
}

/**
 * Convenience: build a detached DOM subtree for a node without a full
 * renderer/session. Text is written via text nodes only.
 */
export function renderNode(node: Node): DomNode {
  if (node.kind === "text") {
    return document.createTextNode((node as TextNode).text);
  }
  const el = document.createElement(tagFor(node.primitive));
  el.dataset.uiId = node.id;
  el.dataset.primitive = node.primitive;
  for (const child of node.children) {
    el.appendChild(renderNode(child));
  }
  return el;
}
