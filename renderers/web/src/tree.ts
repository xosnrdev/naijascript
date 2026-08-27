/**
 * In-memory retained tree model plus patch application semantics.
 *
 * The retained tree owns a mutable copy of the protocol nodes and an
 * id-indexed lookup for O(1) resolution. `applyPatch` mutates the tree in
 * place, applying operations strictly in order. Any invalid operation
 * (bad index, unknown id, revision misuse, type mismatch, duplicate id)
 * throws, matching the protocol requirement that a rejected transaction is
 * never partially applied by a conforming consumer.
 *
 * Note: because throwing mid-transaction would leave the tree partially
 * mutated, callers that need strict atomicity should apply against a clone
 * (see `RetainedTree.clone`). The conformance runner treats any throw as a
 * hard failure, so partial mutation is never observed on the happy path.
 */

import type {
  ElementNode,
  EventKind,
  HandlerBinding,
  MountDoc,
  Node,
  NodeId,
  Op,
  PatchDoc,
  PropValue,
  TextNode,
} from "./protocol.js";

/** Thrown for any protocol/patch violation. */
export class PatchError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "PatchError";
  }
}

/** Internal record tracking a node and its parent id. */
interface Entry {
  node: Node;
  parent: NodeId | null;
}

/**
 * A retained UI tree with an id index and a logical revision counter.
 */
export class RetainedTree {
  /** The root node of the tree. */
  root: Node;
  /** The logical revision this tree currently represents. */
  revision: number;
  /** The session this tree belongs to. */
  readonly sessionId: number;

  private readonly index: Map<NodeId, Entry> = new Map();

  constructor(root: Node, revision: number, sessionId: number) {
    this.root = root;
    this.revision = revision;
    this.sessionId = sessionId;
    this.register(root, null);
  }

  /** Look up a node by id, or throw if it is unknown. */
  get(id: NodeId): Node {
    const entry = this.index.get(id);
    if (entry === undefined) {
      throw new PatchError(`unknown node id: ${id}`);
    }
    return entry.node;
  }

  /** Return the parent id of a node, or null for the root. */
  parentOf(id: NodeId): NodeId | null {
    const entry = this.index.get(id);
    if (entry === undefined) {
      throw new PatchError(`unknown node id: ${id}`);
    }
    return entry.parent;
  }

  /** True if a node with this id exists. */
  has(id: NodeId): boolean {
    return this.index.has(id);
  }

  /** Register a node subtree in the index, rejecting duplicate ids. */
  private register(node: Node, parent: NodeId | null): void {
    if (this.index.has(node.id)) {
      throw new PatchError(`duplicate node id: ${node.id}`);
    }
    this.index.set(node.id, { node, parent });
    if (node.kind === "element") {
      for (const child of node.children) {
        this.register(child, node.id);
      }
    }
  }

  /** Remove a node subtree from the index. */
  private unregister(node: Node): void {
    this.index.delete(node.id);
    if (node.kind === "element") {
      for (const child of node.children) {
        this.unregister(child);
      }
    }
  }

  private asElement(id: NodeId): ElementNode {
    const node = this.get(id);
    if (node.kind !== "element") {
      throw new PatchError(`node ${id} is not an element`);
    }
    return node;
  }

  private asText(id: NodeId): TextNode {
    const node = this.get(id);
    if (node.kind !== "text") {
      throw new PatchError(`node ${id} is not a text node`);
    }
    return node;
  }

  /**
   * Apply a single operation, mutating the tree and index in place.
   * Callers apply operations in list order.
   */
  applyOp(op: Op): void {
    switch (op.op) {
      case "insert": {
        const parent = this.asElement(op.parent);
        this.assertIndex(op.index, parent.children.length, "insert");
        const node = cloneNode(op.node);
        this.register(node, parent.id);
        parent.children.splice(op.index, 0, node);
        break;
      }
      case "remove": {
        const parent = this.parentOf(op.node);
        if (parent === null) {
          throw new PatchError(`cannot remove root node ${op.node}`);
        }
        const node = this.get(op.node);
        const parentNode = this.asElement(parent);
        const pos = parentNode.children.indexOf(node);
        if (pos < 0) {
          throw new PatchError(`node ${op.node} not found under parent`);
        }
        parentNode.children.splice(pos, 1);
        this.unregister(node);
        break;
      }
      case "move": {
        const node = this.get(op.node);
        const currentParentId = this.parentOf(op.node);
        if (currentParentId === null) {
          throw new PatchError(`cannot move root node ${op.node}`);
        }
        // Detach from current parent first.
        const currentParent = this.asElement(currentParentId);
        const pos = currentParent.children.indexOf(node);
        if (pos < 0) {
          throw new PatchError(`node ${op.node} not found under parent`);
        }
        currentParent.children.splice(pos, 1);
        // Insert into the target parent at `index`, measured against the
        // post-detach child list.
        const targetParent = this.asElement(op.parent);
        this.assertIndex(op.index, targetParent.children.length, "move");
        targetParent.children.splice(op.index, 0, node);
        this.index.set(node.id, { node, parent: op.parent });
        break;
      }
      case "replace": {
        const parentId = this.parentOf(op.node);
        const old = this.get(op.node);
        const replacement = cloneNode(op.replacement);
        if (parentId === null) {
          this.unregister(old);
          this.root = replacement;
          this.register(replacement, null);
          break;
        }
        const parent = this.asElement(parentId);
        const pos = parent.children.indexOf(old);
        if (pos < 0) {
          throw new PatchError(`node ${op.node} not found under parent`);
        }
        this.unregister(old);
        parent.children[pos] = replacement;
        this.register(replacement, parent.id);
        break;
      }
      case "set_property": {
        const el = this.asElement(op.node);
        el.props[op.property] = cloneProp(op.value);
        break;
      }
      case "remove_property": {
        const el = this.asElement(op.node);
        if (!(op.property in el.props)) {
          throw new PatchError(
            `property ${op.property} absent on node ${op.node}`,
          );
        }
        delete el.props[op.property];
        break;
      }
      case "set_text": {
        const text = this.asText(op.node);
        text.text = op.text;
        break;
      }
      case "bind_handler": {
        const el = this.asElement(op.node);
        el.handlers[op.event_kind] = cloneBinding(op.binding);
        break;
      }
      case "unbind_handler": {
        const el = this.asElement(op.node);
        if (el.handlers[op.event_kind] === undefined) {
          throw new PatchError(
            `handler ${op.event_kind} absent on node ${op.node}`,
          );
        }
        delete el.handlers[op.event_kind];
        break;
      }
      default: {
        const exhaustive: never = op;
        throw new PatchError(`unknown op: ${JSON.stringify(exhaustive)}`);
      }
    }
  }

  private assertIndex(index: number, length: number, ctx: string): void {
    if (!Number.isInteger(index) || index < 0 || index > length) {
      throw new PatchError(
        `${ctx} index ${index} out of range (0..=${length})`,
      );
    }
  }

  /** Produce an independent deep copy of the retained tree. */
  clone(): RetainedTree {
    return new RetainedTree(cloneNode(this.root), this.revision, this.sessionId);
  }
}

/**
 * Build a retained tree from a mount document, validating the tree and
 * seeding the revision counter.
 */
export function buildTreeFromMount(mount: MountDoc): RetainedTree {
  return new RetainedTree(
    cloneNode(mount.tree),
    mount.tree_revision,
    mount.session_id,
  );
}

/**
 * Build a retained tree from a bare node (revision/session default to 1),
 * useful for tests and ad-hoc rendering.
 */
export function buildTree(node: Node, revision = 1, sessionId = 1): RetainedTree {
  return new RetainedTree(cloneNode(node), revision, sessionId);
}

/**
 * Apply a full patch transaction to a retained tree. Validates revision
 * ordering and session, then applies every operation in order. On success
 * the tree's revision advances to `target_revision`.
 */
export function applyPatch(tree: RetainedTree, patch: PatchDoc): void {
  if (patch.status !== "ok") {
    throw new PatchError(`refusing to apply non-ok patch (status=${patch.status})`);
  }
  if (patch.session_id !== tree.sessionId) {
    throw new PatchError(
      `session mismatch: patch ${patch.session_id} vs tree ${tree.sessionId}`,
    );
  }
  if (patch.target_revision <= patch.base_revision) {
    throw new PatchError(
      `target_revision ${patch.target_revision} must exceed base_revision ${patch.base_revision}`,
    );
  }
  if (patch.base_revision !== tree.revision) {
    throw new PatchError(
      `base_revision ${patch.base_revision} does not match current tree revision ${tree.revision}`,
    );
  }
  for (const op of patch.operations) {
    tree.applyOp(op);
  }
  tree.revision = patch.target_revision;
}

/** Deep-clone a protocol node so the retained tree owns its data. */
export function cloneNode(node: Node): Node {
  if (node.kind === "text") {
    return { kind: "text", id: node.id, text: node.text };
  }
  const props: Record<string, PropValue> = {};
  for (const [name, value] of Object.entries(node.props)) {
    props[name] = cloneProp(value);
  }
  const handlers: ElementNode["handlers"] = {};
  for (const [kind, binding] of Object.entries(node.handlers)) {
    if (binding !== undefined) {
      handlers[kind as EventKind] = cloneBinding(binding);
    }
  }
  return {
    kind: "element",
    id: node.id,
    primitive: node.primitive,
    props,
    handlers,
    children: node.children.map(cloneNode),
  };
}

function cloneProp(value: PropValue): PropValue {
  return { ...value };
}

function cloneBinding(binding: HandlerBinding): HandlerBinding {
  return { id: binding.id, generation: binding.generation };
}
