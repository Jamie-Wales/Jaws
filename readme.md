# Jaws R7RS Scheme Interpreter 🦈

**Live Demo:** [**https://jamie-wales.github.io/jaws/**](https://jamie-wales.github.io/jaws/)

Jaws is an **R7RS Scheme** implementation written in C++ and compiled to WebAssembly using Emscripten. It features a standard interpreter REPL, the ability to run script files, and exposes various compilation stages for educational or debugging purposes. The project includes a C++ backend and a web-based client interface.

## Features ✨

* **R7RS Scheme Implementation**: Parses and interprets code compliant with the R7RS standard.
* **WebAssembly Target** 🚀: Core interpreter logic is compiled to WASM using Emscripten, allowing it to run in web browsers.
* **REPL** ⌨️: An interactive Read-Eval-Print Loop available for direct interaction via a command-line executable.
* **Script Execution**: Can execute Scheme code from files via the command-line executable.
* **Compilation Stages** 🔬: Exposes intermediate representations through the `getAllStages` function (in the WASM module):
  * Abstract Syntax Tree (AST)
  * Macro Expanded Code
  * A-Normal Form (ANF)
  * Optimized ANF (including Dead Code Elimination)
  * Three-Address Code (TAC)
  * Dependency Graphs (Pre and Post Optimization)
* **Import System**: Supports R7RS library imports (`define-library`, `import`).
* **Macro System**: Implements R7RS `define-syntax` and `syntax-rules`.
* **Optimization**: Includes optimization passes like Dead Code Elimination and Constant Folding.
* **Multiple Backends (Potential)**: Code includes generators for QBE intermediate representation and x86-64 Assembly, suggesting potential compilation targets beyond the WASM interpreter.
* **FFI**: Includes a Foreign Function Interface (FFI) for calling C functions.
* **Web Client** 🌐: A web interface built with React and Vite for interacting with the WASM interpreter.
* **Runtime System**: Includes a basic C runtime with garbage collection for natively compiled executables.

## Technology Stack 🛠️

* **Core**: C++ (C++20 standard)
* **Build System**: CMake (version 3.14+)
* **Web Compilation**: Emscripten
* **Native Compilation**: Standard C++ Compiler (e.g., Clang, GCC - currently targets macOS arm64)
* **Testing**: GoogleTest
* **Native Backends**: QBE, Clang (for assembly and linking)
* **Web Client**: JavaScript/TypeScript, Vite, React, Tailwind CSS, Radix UI, CodeMirror.

## Project Structure 📁

* `src/`: Contains the C++ source code for the interpreter library (`app_lib`) and the command-line executable (`jaws`).
  * `frontend/`: Scanner (`scan.cpp`), Parser (`parse.cpp`), Macro Expansion (`MacroTraits.cpp`, `MacroEnvironment.cpp`), Import handling (`Import.cpp`), Environments (`Environment.cpp`), Expression representation (`Expression.cpp`).
  * `interpreter/`: Core interpreter logic (`interpret.cpp`), Value representation (`Value.cpp`), Procedures (`Procedure.cpp`), Ports (`Port.cpp`, `SocketStream.cpp`), FFI (`FFI.cpp`), Built-in functions (`builtins/`).
  * `compiler/`: ANF Transformation (`ANFTransformer.cpp`, `ANF.cpp`), Optimization (`optimise.cpp`, `DeadCodeElimination.cpp`, `ConstantFold.cpp`), TAC Generation (`ThreeAC.cpp`), Backend Code Generation (`QBEGenerator.cpp`, `AssemblyGeneration.cpp`).
  * `utils/`: Utilities for running the interpreter (`run.cpp`), debugging (`DebugUtils.cpp`), expression manipulation (`ExpressionUtils.cpp`).
  * `main.cpp`: Entry point for the command-line `jaws` executable.
* `web/`: Contains code related to the WebAssembly version.
  * `CMakeLists.txt`: CMake file for building the WASM module.
  * `JawsWrapper.cpp`: C++ wrapper using Embind for interfacing WASM with JavaScript.
  * `client/`: Contains the source code for the web interface (React, Vite).
    * `vite.config.js` (or `.ts`): Configuration for the Vite build tool, including custom plugins for handling WASM.
    * `package.json`: Node.js project file defining scripts and dependencies.
* `lib/`: Contains Scheme library files (`.scm`) used by the import system (e.g., `base.scm`, `list-utils.scm`).
* `runtime/`: Contains C source code for the runtime system (e.g., `gc.c`, `types.c`) needed for natively compiled Scheme programs.
* `include/`: Header files for the C++ code.
* `tests/`: C++ unit tests using GoogleTest.
* `CMakeLists.txt`: Main CMake file for building the native executable and tests.

## Runtime System ⚙️

When compiling Scheme code to a native executable (using the `--compile` option, QBE, and Clang), the resulting program is linked against a small C runtime. This runtime provides essential services:

* **Memory Management**: Implements a heap for allocating Scheme objects (`SchemeObject`).
* **Garbage Collection**: Includes a basic mark-and-sweep (compacting) garbage collector (`gc`, `mark_roots`, `sweep_compact`). It marks reachable objects starting from roots found on the C stack. The heap can grow and potentially shrink based on usage.
* **Object Representation**: Defines the structure for Scheme objects (`SchemeObject`, `SchemeType`, `SchemeValue` union) including types like numbers, pairs, symbols, etc.
* **Core Operations**: Provides low-level functions like `allocate`, `allocate_pair`, `init_runtime`, `cleanup_runtime`.

## Built-in and Standard Library Functions/Syntax 📚

### Core Built-ins (Implemented in C++)

* **Math**: `+`, `-`, `*`, `/`, `<=`, `>=`, `<`, `>`, `=`
* **Equality/Type Predicates**: `eq?`, `equal?`, `boolean?`, `procedure?`, `char?`, `pair?`, `null?`, `port?`, `eqv?`, `symbol?`, `number?`, `string?`, `list?`, `vector?`
* **Lists**: `list`, `car`, `cdr`, `cons`, `append`, `list-ref`, `list-set!`
* **Vectors**: `make-vector`, `vector`, `vector-ref`, `vector-set!`, `vector-length`, `vector-copy`, `vector-copy!`, `vector-fill!`
* **Strings**: `number->string`, `string=?`, `string<?`, `string>?`, `string-ci=?`, `string-length`, `string-append`, `substring`, `string-ref`, `string->list`, `list->string`, `string-copy`, `string-upcase`, `string-downcase`, `char->string`
* **Control/HOF**: `map`, `eval`, `apply`, `call/cc`, `call-with-current-continuation`
* **I/O**: `open-input-file`, `open-output-file`, `close-port`, `read`, `write`, `display`, `newline`, `error`
* **Sockets**: `socket-server`, `socket-connect`, `socket-accept`, `socket-read`, `socket-write`, `socket-close`, `socket-set-nonblocking!`
* **FFI**: `load-library`, `register-function`
* **Threading**: `thread-spawn`, `thread-join`, `thread-sleep!`, `thread-current-id`, `mutex-create`, `mutex-lock!`, `mutex-unlock!`, `condition-variable-create`, `condition-variable-wait`, `condition-variable-signal!`, `condition-variable-broadcast!`
* **Values/Conversions**: `symbol->string`, `vector->list`, `list->vector`

### Standard Library: `(base)`

* **Syntax**: `do`, `begin`, `cond`, `let*`, `let-values`, `letrec`, `lej`, `and`, `or`, `when`, `unless`, `print`, `println`
* **Procedures**: `values`, `call-with-values`, `zero?`, `not`

### Standard Library: `(list-utils)`

* **CAR/CDR Combinations**: `cadr`, `cddr`, `caddr`, `cdddr`, `cadddr`, `caar`, `cdar`, `caaar`, `caadr`, `cadar`, `cddar`, `cdddar`, `cddadr`, `cdadr`, `caaaar`, `caaadr`, `caadar`, `cadaar`, `caddar`, `cdaaar`, `cdaadr`, `cdadar`, `cddaar`, `cdddar`, `caaaaa`, `caaaad`, `caaada`, `caadaa`, `caadda`, `cadaaa`, `cadaad`, `cadada`, `caddaa`, `caddda`, `cdaaaa`, `cdaaad`, `cdaada`, `cdadaa`, `cdadda`, `cddaaa`, `cddaad`, `cddada`, `cdddaa`, `cdddda`
* **List Operations**: `reverse`, `list-tail`, `last`, `last-pair`, `length`, `filter`
* **Membership/Association**: `memq`, `memv`, `member`, `assoc`, `assv`, `assq`

### Standard Library: `(loops)`

* **Syntax**: `while`, `for`, `repeat`, `until`, `do-while`, `for-each-with-index`, `for-range`, `iterate`, `fold-loop`, `select-case`, `nested-loop`, `for-each`

## Building and Running 🏗️

### Dependencies

* **CMake**: Version 3.14 or higher.
* **C++ Compiler**: Supporting C++20 (e.g., Clang, GCC).
* **Emscripten SDK**: Required *only* for building the WebAssembly version. ([Installation Guide](https://emscripten.org/docs/getting_started/downloads.html))
* **Node.js and npm**: Required *only* for building and running the web client.
* **QBE**: Required *only* if using the native compilation (`--compile`) feature. ([QBE Compiler Backend](https://c9x.me/qbe/))

### Build Process (CMake)

The project uses CMake for building both the native components and the WebAssembly module.

1. **Configure**: Create a build directory and run CMake.
    * **Native Build (REPL/Tests):**

        ```bash
        mkdir build && cd build
        cmake ..
        ```

    * **WebAssembly Build:** Requires the Emscripten SDK to be sourced first.

        ```bash
        # Example: source /path/to/emsdk/emsdk_env.sh
        mkdir build_wasm && cd build_wasm
        cmake ../web -DCMAKE_TOOLCHAIN_FILE=$EMSDK/upstream/emscripten/cmake/Modules/Platform/Emscripten.cmake
        ```

2. **Build Targets**:
    * **Native (`build` directory):**
        * `cmake --build . --target app_lib` (Builds the static library)
        * `cmake --build . --target jaws` (Builds the command-line executable)
        * `cmake --build . --target tests` (Builds the test executable)
    * **WebAssembly (`build_wasm` directory):**
        * `cmake --build . --target jaws_wasm` (Builds `jaws.js` and `jaws.wasm`)
            * This target automatically copies the built files to `web/client/public/wasm/` as a post-build step.

### Running

#### Command-Line REPL/Runner (`jaws` executable)

1. Build the `jaws` target as described above.
2. **Run REPL**: `./build/jaws`
3. **Run Script**: `./build/jaws --script <path/to/your/script.scm>`
4. **Compile Script (Native)**: `./build/jaws --script <path/to/script.scm> -c [output_dir]` (Requires QBE and Clang). This links against the C runtime.
5. **Run Tests**: `cd build && ctest`

#### Web Client 🌐

1. **Build WASM**: Ensure the `jaws_wasm` target has been built successfully (which copies `jaws.js` and `jaws.wasm` to `web/client/public/wasm/`).
3. **Navigate**: `cd web/client`.
4. **Install Dependencies**: `npm install`.
5. **Run Development Server**: `npm run dev`.
    * It then starts the Vite development server.
    * The `wasmPlugin` in `vite.config.js` copies `jaws.js` and `jaws.wasm` from `public/wasm` to `public/javascript` for development access.
    * Access the URL provided by Vite (usually `http://localhost:5173` or similar).
6. **Build for Production**: `npm run build`.
    * The `wasmPlugin` copies `jaws.js` and `jaws.wasm` from `public/wasm` to `dist/javascript`.
    * The production-ready static files will be in `web/client/dist`.
7. **Preview Production Build**: `npm run preview`.
8. **Deploy to GitHub Pages**: `npm run deploy` (uses `gh-pages` to push the `dist` directory).

## Usage (Web Client - JavaScript/TypeScript) 💻

```javascript
// Vite copies wasm files to /javascript/ in dev and dist/javascript in build
// Adjust import path based on your project structure if needed
import createJawsModule from '/javascript/jaws.js'; // Path for development server
// OR use './javascript/jaws.js' for production build if served relative

createJawsModule().then((jaws) => {
  // Create an instance of the Jaws wrapper
  const interpreter = new jaws.JawsWrapper();

  // Evaluate Scheme code
  const code = "(import (base)) (+ 1 2)"; // Import base if needed
  const result = interpreter.evaluate(code);

  if (result.error) {
    console.error("Evaluation Error:", result.error);
  } else {
    console.log("Result:", result.result);
    console.log("Stdout:", result.stdout);
  }

  // Get all compilation stages
  const stagesCode = "(import (base)) (define (add x y) (+ x y)) (add 5 7)";
  const stages = interpreter.getAllStages(stagesCode);

  if (stages.error) {
    console.error("Stages Error:", stages.error);
  } else {
    console.log("AST:", stages.ast);
    console.log("Macro Expanded:", stages.macroExpanded);
    console.log("ANF:", stages.anf);
    console.log("Optimized ANF:", stages.optimizedANF);
    console.log("Three Address Code:", stages.threeAC);
    console.log("Pre-Optimization Graph:\n", stages.preDependencyGraph);
    console.log("Post-Optimization Graph:\n", stages.postDependencyGraph);
  }

  // Clean up (if necessary, though Emscripten often handles this)
  // interpreter.delete(); // Or similar method if provided by Embind/Emscripten binding
});
