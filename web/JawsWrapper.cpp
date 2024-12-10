#include "Error.h"
#include "Interpreter.h"
#include "Parser.h"
#include "Run.h"
#include "Scanner.h"
#include "Visit.h"
#include <emscripten/bind.h>
#include <emscripten/val.h>
#include <memory>
#include <string>

using namespace emscripten;

class JawsWrapper {
private:
    std::shared_ptr<Scanner> scanner = std::make_shared<Scanner>();
    std::shared_ptr<Parser> parser = std::make_shared<Parser>();
    Interpreter interpreter = { scanner, parser };

public:
    JawsWrapper()
        : scanner(std::make_shared<Scanner>())
        , parser(std::make_shared<Parser>())
        , interpreter { scanner, parser }
    {
    }

    std::string evaluate(const std::string& input)
    {
        try {
            parser->initialize(scanner);
            interpreter.outputStream.clear();
            interpreter.outputStream.str(""); // Clear the stream before processing

            std::vector<Token> tokens = scanner->tokenize(input);
            parser->load(tokens);
            interpreter.init();
            std::string output = interpreter.outputStream.str();
            interpreter.outputStream.str(""); // Clear it again after getting output
            interpreter.outputStream.clear();
            return output;
        } catch (const InterpreterError& e) {
            return std::string(e.what());
        } catch (const ParseError& e) {
            return std::string("Parse Error: ") + e.what();
        } catch (const std::exception& e) {
            return std::string("Error: ") + e.what();
        }
    }

    std::string getEnvironment() const
    {
        return "Jaws Scheme Environment";
    }
};

EMSCRIPTEN_BINDINGS(jaws_module)
{
    class_<JawsWrapper>("JawsWrapper")
        .constructor<>()
        .function("evaluate", &JawsWrapper::evaluate)
        .function("getEnvironment", &JawsWrapper::getEnvironment);
}
