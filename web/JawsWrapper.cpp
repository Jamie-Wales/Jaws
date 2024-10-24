#include "Interpreter.h"
#include "Parser.h"
#include "Scanner.h"
#include "Error.h"
#include <emscripten/bind.h>
#include <emscripten/val.h>
#include <memory>
#include <string>

using namespace emscripten;

class JawsWrapper {
private:
    std::shared_ptr<Scanner> scanner;
    Parser parser;
    Interpreter interpreter;
    
public:
    JawsWrapper() {
        scanner = std::make_shared<Scanner>();
        parser.initialize(scanner);
    }
    
    std::string evaluate(const std::string& input) {
        if (input.empty()) {
            return "";
        }

        try {
            std::vector<Token> tokens = scanner->tokenize(input);
            parser.load(tokens);
            auto expr = parser.parse();
            
            if (expr) {
                std::optional<SchemeValue> result = interpreter.interpret(*expr);
                if (result) {
                    return result->toString();
                }
            }
            return "";
        } catch (const ParseError& e) {
            return std::string("Parse Error: ") + e.what();
        } catch (const InterpreterError& e) {
            return std::string("Interpreter Error: ") + e.what();
        } catch (const std::exception& e) {
            return std::string("Error: ") + e.what();
        }
    }

    std::string getEnvironment() const {
        return "Jaws Scheme Environment";  // Modified to return a simple string if getEnvironmentString() isn't available
    }
};

// Make sure this is outside the class definition
EMSCRIPTEN_BINDINGS(jaws_module) {
    class_<JawsWrapper>("JawsWrapper")
        .constructor<>()
        .function("evaluate", &JawsWrapper::evaluate)
        .function("getEnvironment", &JawsWrapper::getEnvironment);
}
