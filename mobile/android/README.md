# Android proof of concept

`NaijaScriptEngine.kt` is the application-facing half of the spike. Add it to
an Android library/application, package the wasm asset as described in
`../README.md`, and implement its four JNI methods in a small native library.

For the Wasmtime baseline, the native object represented by `handle` owns one
configured Pulley engine/store, module, instance, memory export, and the seven
typed function exports. `nativeEvaluate` must:

1. allocate and write both byte arrays;
2. invoke `ns_eval`;
3. copy and UTF-8 decode the result before freeing its handle;
4. free both inputs on every non-trapping path;
5. turn traps into Java exceptions and discard/recreate the instance after a
   trap, since cleanup exports cannot safely be assumed to run afterward.

Configure store memory/fuel/epoch limits in native code and serialize access to
each instance. The Kotlin facade is synchronized, but native code should also
reject calls after destruction. Run JNI calls on a worker dispatcher, not the
UI thread.

This is scaffold code, not a complete Gradle project: Wasmtime mobile build and
JNI packaging vary by selected runtime version and Android ABI.
