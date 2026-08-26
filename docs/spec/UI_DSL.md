# NaijaScript Declarative UI DSL (Stage 2)

Status: MVP specification draft

Surface name: NaijaScript UI (`.nui`)

Normative words: MUST, MUST NOT, SHOULD, and MAY are requirements as defined by RFC 2119.

## Scope

NaijaScript UI is a dedicated declarative language that compiles to the renderer-neutral UI IR. It is not HTML, JSX, SwiftUI, or Compose syntax, and a renderer MUST NOT infer platform behavior from source spelling. The MVP supports screens, reusable components, nested elements, typed properties, text, conditionals, keyed lists, reactive local state, and named event handlers.

The `.nui` parser may reuse the core NaijaScript expression grammar for literals, arithmetic, comparisons, indexing, and function calls. UI declarations, types, elements, bindings, and handlers belong to this DSL and MUST NOT change the grammar of `.ns` programs.

## Concrete MVP grammar

The grammar is EBNF. `IDENT`, `NUMBER`, and `STRING` use the lexical rules of core NaijaScript. `LENGTH` is a number immediately followed by a supported unit, such as `12px`. Whitespace separates tokens; `#` starts a line comment.

```ebnf
file              = { import_decl }, { component_decl }, { screen_decl } ;
import_decl       = "use", STRING, [ "as", IDENT ] ;

screen_decl       = "screen", IDENT, parameter_list?, "start",
                      { state_decl | handler_decl },
                      "view", "start", child*, "end",
                    "end" ;

component_decl    = "component", IDENT, parameter_list, "start",
                      { state_decl | handler_decl },
                      "view", "start", child*, "end",
                    "end" ;

parameter_list    = "(", [ parameter, { ",", parameter } ], ")" ;
parameter         = IDENT, ":", type, [ "get", expression ] ;
type              = primitive_type
                  | IDENT
                  | "List", "<", type, ">"
                  | "Option", "<", type, ">" ;
primitive_type    = "Text" | "Number" | "Boolean" | "Color" | "Length"
                  | "ImageSource" | "Key" | "Alignment" | "Axis"
                  | "ImageFit" ;

state_decl        = "state", IDENT, ":", type, "get", expression ;
handler_decl      = "when", IDENT, "(", IDENT, ":", event_type, ")",
                    "start", statement*, "end" ;
event_type        = "TapEvent" | "ChangeEvent" | "SubmitEvent"
                  | "FocusEvent" ;

child             = element | text_node | if_node | each_node ;
element           = element_name, [ prop_list ], [ "key", expression ],
                    ( "start", child*, "end" | "/" ) ;
element_name      = primitive_name | IDENT ;
primitive_name    = "stack" | "row" | "scroll" | "label" | "button"
                  | "input" | "image" | "spacer" ;
prop_list         = "(", [ prop, { ",", prop } ], ")" ;
prop              = IDENT, ":", prop_value ;
prop_value        = expression | handler_ref ;
handler_ref       = "@", IDENT ;

text_node         = "text", expression ;
if_node           = "show", expression, "start", child*, "end",
                    [ "otherwise", "start", child*, "end" ] ;
each_node         = "each", IDENT, "in", expression, "key", expression,
                    "start", child*, "end" ;
```

`statement` and `expression` above are core NaijaScript statements and expressions plus assignment to state declared in the same screen or component. UI expressions additionally admit `LENGTH` literals. Arbitrary statements are forbidden directly inside `view`.

The `key expression` after an element's property list is an explicit key. It is separate from a property and accepts only a value of type `Key`.

## Type rules

- Every component parameter, state declaration, primitive property, and event parameter MUST have a statically known type.
- `Text`, `Number`, and `Boolean` map to the corresponding core scalar values. `Color`, `Length`, `ImageSource`, `Key`, `Alignment`, `Axis`, and `ImageFit` are distinct UI types and are not implicitly interchangeable with `Text`.
- An `IDENT` in type position names a host-provided record schema. Record fields are immutable in `view` and are resolved statically.
- `Option<T>` is the only nullable UI type. A missing optional property and a property whose value is `null` have the same IR result.
- Component calls are type-checked against their declared parameters. Unknown, duplicate, or missing required properties are compile errors.
- Primitive property names and types come from the capability schema in `UI_IR_PROTOCOL.md`. A source may compile only for a declared capability set.
- `text expression` requires `Text`. There is no implicit stringification.
- A handler reference has type `Handler<E>`. The event type accepted by the named handler MUST exactly match the primitive property's event type.
- State initializers and default parameter values MUST be pure expressions and MUST type-check without renderer access.
- A component cannot mutate a parameter. A handler may update state owned by its current component instance, call core functions, or invoke a host action whose typed signature was supplied to the UI compiler. Host actions are forbidden during `view` evaluation.

### UI literal forms

The MVP defines these portable forms:

- `12px` is twelve logical pixels. Web maps a logical pixel to a CSS pixel, Compose to `dp`, and SwiftUI to a point; it is not a physical-device pixel.
- `Color.rgb(red, green, blue)` and `Color.rgba(red, green, blue, alpha)` construct sRGB colors from finite, range-checked numeric channels.
- `ImageSource.asset("name")` selects a packaged, manifest-declared asset. `ImageSource.uri(...)` is unavailable unless an image-URI capability and host allowlist are negotiated.
- `Alignment.start`, `Alignment.center`, `Alignment.end`, and `Alignment.stretch` are the MVP alignment cases.
- `Axis.vertical` and `Axis.horizontal` are the MVP axis cases.
- `ImageFit.contain`, `ImageFit.cover`, and `ImageFit.fill` are the MVP image-fit cases.

Unknown units, constructors, or enum cases are compile errors. Percent, intrinsic, theme, and viewport-relative lengths require future negotiated capabilities.

## Declarative behavior

The compiler evaluates `view` as a pure function of parameters and state. A state write schedules one new evaluation of the owning instance; multiple synchronous writes MAY be batched, but the visible result MUST equal evaluating them in source order.

`show` emits exactly one branch. `each` requires `List<T>` and its key expression requires `Key`. Keys MUST be unique among siblings for one evaluation. A duplicate key is a deterministic render error; the renderer MUST NOT fall back to list indexes.

Every event binding names a handler with `@name`. Inline functions and source snippets are forbidden. Handler bodies are never transferred to a renderer.

## Primitive properties

The MVP source schema is renderer-neutral:

| Primitive | Required properties | Optional properties | Events | Children |
| --- | --- | --- | --- | --- |
| `stack` | — | `gap: Length`, `padding: Length`, `align: Alignment` | — | any |
| `row` | — | `gap: Length`, `padding: Length`, `align: Alignment` | — | any |
| `scroll` | — | `axis: Axis` | — | one container |
| `label` | — | `tone: Color`, `size: Number`, `a11yLabel: Text` | — | text only |
| `button` | `onTap: Handler<TapEvent>` | `enabled: Boolean`, `a11yLabel: Text` | tap | text or one container |
| `input` | `value: Text`, `onChange: Handler<ChangeEvent>` | `placeholder: Text`, `enabled: Boolean`, `onSubmit: Handler<SubmitEvent>`, `a11yLabel: Text` | change, submit, focus | none |
| `image` | `source: ImageSource` | `a11yLabel: Text`, `fit: ImageFit` | — | none |
| `spacer` | — | `size: Length` | — | none |

Allowed cases for `Alignment`, `Axis`, and `ImageFit` are defined by the capability schema, not guessed by renderers. Their literals use qualified names such as `Axis.vertical`. Accessibility labels are semantic metadata and MUST map to the native accessibility system.

## MVP examples

### Stateful screen, condition, and named handlers

```naijascript
screen Counter(title: Text get "Counter") start
    state count: Number get 0

    when increment(event: TapEvent) start
        count get count add 1
    end

    when reset(event: TapEvent) start
        count get 0
    end

    view start
        stack(gap: 12px, padding: 16px) start
            label(a11yLabel: title) start
                text title
            end
            text "Count: {count}"

            button(onTap: @increment, enabled: true) start
                text "Add one"
            end

            show count pass 0 start
                button(onTap: @reset) start
                    text "Reset"
                end
            end
        end
    end
end
```

`12px` and `16px` are `Length` literals. String interpolation is permitted only when every interpolation has a deterministic text conversion defined by the core language.

### Typed component and keyed list

```naijascript
component TodoRow(todoId: Key, title: Text, done: Boolean) start
    when toggle(event: TapEvent) start
        toggleTodo(todoId)
    end

    view start
        row(gap: 8px) key todoId start
            label start
                text title
            end
            button(onTap: @toggle, a11yLabel: "Toggle {title}") start
                show done start
                    text "Done"
                end
                otherwise start
                    text "Open"
                end
            end
        end
    end
end

screen TodoList(items: List<Todo>) start
    view start
        scroll(axis: Axis.vertical) start
            stack start
                each item in items key item.id start
                    TodoRow(
                        todoId: item.id,
                        title: item.title,
                        done: item.done
                    ) /
                end
            end
        end
    end
end
```

`Todo` illustrates a host-provided typed record; records are not added to core NaijaScript by this specification. A build MUST supply its shape to the UI type checker.

## Diagnostics and exclusions

Diagnostics MUST include a stable code, source range, and actionable message. At minimum, the compiler reports unknown primitive/property, type mismatch, missing property, invalid child, unknown handler, handler event mismatch, duplicate sibling key, unsupported capability, and impure view expression.

The MVP deliberately excludes arbitrary native widgets, HTML/CSS, platform-condition branches, effects during view evaluation, unkeyed dynamic lists, anonymous event handlers, direct DOM/native object access, animations, navigation, networking, and persistence. These can be proposed only with IR and conformance behavior defined first.
