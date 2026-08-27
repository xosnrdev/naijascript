@file:OptIn(ExperimentalSerializationApi::class)

package protocol

import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonClassDiscriminator

/**
 * Renderer-neutral UI protocol model for NaijaScript.
 *
 * These types mirror the JSON emitted by the Rust engine's transport adapter
 * (see `docs/spec/UI_IR_PROTOCOL.md` and `wasm-abi/src/ui.rs`). They are pure
 * data with no renderer callbacks or platform objects, matching the protocol's
 * boundary rules.
 *
 * Node ids and handler ids are 64-bit values encoded as decimal strings on the
 * wire, so they survive JSON's 53-bit safe-integer limit. We keep them as
 * [String] and never do arithmetic on them.
 */

/** Shared, lenient JSON configuration used by the whole renderer. */
val ProtocolJson: Json = Json {
    ignoreUnknownKeys = true
    // Polymorphic hierarchies below each pin their own discriminator via
    // @JsonClassDiscriminator; PropValue uses the default "type" key.
    classDiscriminator = "type"
    isLenient = false
    explicitNulls = false
}

// ---------------------------------------------------------------------------
// Nodes
// ---------------------------------------------------------------------------

/** A UI tree node: either an element or a text leaf. Discriminated by "kind". */
@Serializable
@JsonClassDiscriminator("kind")
sealed interface Node {
    val id: String
}

@Serializable
@SerialName("element")
data class Element(
    override val id: String,
    val primitive: String,
    val props: Map<String, PropValue> = emptyMap(),
    val handlers: Map<String, HandlerBinding> = emptyMap(),
    val children: List<Node> = emptyList(),
) : Node

@Serializable
@SerialName("text")
data class Text(
    override val id: String,
    val text: String,
) : Node

/** A handler binding: opaque routing identity, never a callback. */
@Serializable
data class HandlerBinding(
    val id: String,
    val generation: Long,
)

// ---------------------------------------------------------------------------
// Property values (tagged by "type")
// ---------------------------------------------------------------------------

@Serializable
sealed interface PropValue

@Serializable
@SerialName("text")
data class TextProp(val value: String) : PropValue

@Serializable
@SerialName("number")
data class NumberProp(val value: Double) : PropValue

@Serializable
@SerialName("boolean")
data class BooleanProp(val value: Boolean) : PropValue

@Serializable
@SerialName("key")
data class KeyProp(val value: String) : PropValue

@Serializable
@SerialName("color")
data class ColorProp(
    val r: Int,
    val g: Int,
    val b: Int,
    val a: Double,
) : PropValue

@Serializable
@SerialName("length")
data class LengthProp(val px: Double) : PropValue

@Serializable
@SerialName("image")
data class ImageProp(val asset: String) : PropValue

@Serializable
@SerialName("enum")
data class EnumProp(
    @SerialName("enum") val enumType: String,
    val case: String,
) : PropValue

// ---------------------------------------------------------------------------
// Patch operations (tagged by "op")
// ---------------------------------------------------------------------------

@Serializable
@JsonClassDiscriminator("op")
sealed interface Op

@Serializable
@SerialName("insert")
data class InsertOp(
    val parent: String,
    val index: Int,
    val node: Node,
) : Op

@Serializable
@SerialName("remove")
data class RemoveOp(
    val node: String,
) : Op

@Serializable
@SerialName("move")
data class MoveOp(
    val node: String,
    val parent: String,
    val index: Int,
) : Op

@Serializable
@SerialName("replace")
data class ReplaceOp(
    val node: String,
    val replacement: Node,
) : Op

@Serializable
@SerialName("set_property")
data class SetPropertyOp(
    val node: String,
    val property: String,
    val value: PropValue,
) : Op

@Serializable
@SerialName("remove_property")
data class RemovePropertyOp(
    val node: String,
    val property: String,
) : Op

@Serializable
@SerialName("set_text")
data class SetTextOp(
    val node: String,
    val text: String,
) : Op

@Serializable
@SerialName("bind_handler")
data class BindHandlerOp(
    val node: String,
    @SerialName("event_kind") val eventKind: String,
    val binding: HandlerBinding,
) : Op

@Serializable
@SerialName("unbind_handler")
data class UnbindHandlerOp(
    val node: String,
    @SerialName("event_kind") val eventKind: String,
) : Op

// ---------------------------------------------------------------------------
// Documents: mount, patch, and the golden fixture envelope
// ---------------------------------------------------------------------------

@Serializable
data class MountDoc(
    @SerialName("abi_version") val abiVersion: Int,
    val type: String,
    val status: String,
    @SerialName("session_id") val sessionId: Long,
    @SerialName("tree_revision") val treeRevision: Long,
    val tree: Node,
)

@Serializable
data class PatchDoc(
    @SerialName("abi_version") val abiVersion: Int,
    val type: String,
    val status: String,
    @SerialName("session_id") val sessionId: Long? = null,
    @SerialName("base_revision") val baseRevision: Long? = null,
    @SerialName("target_revision") val targetRevision: Long? = null,
    val operations: List<Op> = emptyList(),
)

@Serializable
data class Step(
    val status: String,
    val patch: PatchDoc? = null,
)

@Serializable
data class Golden(
    @SerialName("protocol_version") val protocolVersion: Int,
    val status: String,
    val mount: MountDoc,
    val steps: List<Step> = emptyList(),
    @SerialName("final_tree") val finalTree: Node,
)

// ---------------------------------------------------------------------------
// Event envelope emitted by renderers (data only, never callbacks)
// ---------------------------------------------------------------------------

/** The semantic payload a renderer reports for a fired handler. */
sealed interface EventPayload {
    data object Tap : EventPayload
    data class Change(val value: String, val composing: Boolean = false) : EventPayload
    data class Submit(val value: String?) : EventPayload
    data class Focus(val focused: Boolean) : EventPayload

    /** Wire event kind matching the protocol's `EventKind`. */
    val kind: String
        get() = when (this) {
            is Tap -> "tap"
            is Change -> "change"
            is Submit -> "submit"
            is Focus -> "focus"
        }
}

/**
 * A fully-routed event the renderer hands back to the host. It carries only
 * routing identity plus the semantic payload — no renderer closures escape the
 * view layer.
 */
data class EventData(
    val sessionId: Long,
    val nodeId: String,
    val handlerId: String,
    val handlerGeneration: Long,
    val event: EventPayload,
)
