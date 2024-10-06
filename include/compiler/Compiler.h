#pragma once
#include "Expression.h"
#include "visit.h"
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

class Compiler {
public:
    Compiler() = default;

    std::string compile(const std::unique_ptr<Expression>& expr)
    {
        instructions.clear();
        labelCounter = 0;

        emit(".section __TEXT,__text,regular,pure_instructions");
        emit(".build_version macos, 11, 0\t sdk_version 11, 0");
        emit(".globl _main");
        emit(".p2align 2");

        emit("_main:");
        emit("    stp x29, x30, [sp, #-16]!");
        emit("    mov x29, sp");
        compileExpression(expr);
        emit("    bl _print_result");
        emit("    mov w0, #0");
        emit("    ldp x29, x30, [sp], #16");
        emit("    ret");

        // Print function
        emitPrintFunction();

        return assembleInstructions();
    }

private:
    std::vector<std::string> instructions;
    int labelCounter;
    std::unordered_map<std::string, std::string> operatorInstructions = {
        { "+", "add" },
        { "-", "sub" },
        { "*", "mul" },
        { "/", "sdiv" }
    };

    void compileExpression(const std::unique_ptr<Expression>& expr)
    {
        std::visit(overloaded {
                       [this](const LiteralExpression& e) {
                           if (e.value.type == Tokentype::INTEGER) {
                               emit("    mov x0, #" + e.value.lexeme);
                           } else if (e.value.type == Tokentype::FLOAT) {
                               throw std::runtime_error("Float literals not supported in this basic version");
                           } else if (e.value.type == Tokentype::SYMBOL) {
                               throw std::runtime_error("Symbol resolution not implemented in this basic version");
                           } else {
                               throw std::runtime_error("Unsupported literal type");
                           }
                       },
                       [this](const BinaryExpression& e) {
                           compileExpression(e.left);
                           emit("    mov x1, x0"); // Store left result in x1
                           compileExpression(e.right);
                           // x0 now contains right result
                           std::string op = e.operatorToken.lexeme;
                           if (operatorInstructions.find(op) != operatorInstructions.end()) {
                               emit("    " + operatorInstructions[op] + " x0, x1, x0");
                           } else {
                               throw std::runtime_error("Unsupported binary operator: " + op);
                           }
                       },
                       [this](const PrefixExpression& e) {
                           if (e.args.empty()) {
                               throw std::runtime_error("Prefix expression requires at least one argument");
                           }
                           std::string op = e.op.lexeme;
                           if (op == "not") {
                               if (e.args.size() != 1) {
                                   throw std::runtime_error("'not' operator requires exactly one argument");
                               }
                               compileExpression(e.args[0]);
                               emit("    eor x0, x0, #1"); // Logical NOT
                           } else if (operatorInstructions.find(op) != operatorInstructions.end()) {
                               // Treat as variadic arithmetic operation
                               compileExpression(e.args[0]);
                               for (size_t i = 1; i < e.args.size(); ++i) {
                                   emit("    mov x1, x0");
                                   compileExpression(e.args[i]);
                                   emit("    " + operatorInstructions[op] + " x0, x1, x0");
                               }
                           } else {
                               throw std::runtime_error("Unsupported prefix operator: " + op);
                           }
                       },
                       [this](const ListExpression& e) {
                           if (e.elements.empty()) {
                               emit("    mov x0, #0"); // Empty list evaluates to 0
                               return;
                           }
                           const auto& first = e.elements[0];
                           if (auto* literal = std::get_if<LiteralExpression>(&first->as)) {
                               if (literal->value.type == Tokentype::SYMBOL) {
                                   std::string op = literal->value.lexeme;
                                   if (op == "if") {
                                       compileIfExpression(e.elements);
                                   } else if (operatorInstructions.find(op) != operatorInstructions.end()) {
                                       compileArithmeticOperation(op, e.elements);
                                   } else {
                                       throw std::runtime_error("Unsupported operation: " + op);
                                   }
                               } else {
                                   throw std::runtime_error("Expected symbol as first element of list");
                               }
                           } else {
                               throw std::runtime_error("Expected literal as first element of list");
                           }
                       } },
            expr->as);
    }

    void compileIfExpression(const std::vector<std::unique_ptr<Expression>>& elements)
    {
        if (elements.size() != 4) { // if + condition + then + else
            throw std::runtime_error("If expression requires exactly 3 parts: condition, then, and else");
        }

        std::string elseLabel = generateLabel();
        std::string endLabel = generateLabel();

        compileExpression(elements[1]);
        emit("    cmp x0, #0");
        emit("    beq " + elseLabel);

        // Compile 'then' part
        compileExpression(elements[2]);
        emit("    b " + endLabel);

        emit(elseLabel + ":");
        compileExpression(elements[3]);

        emit(endLabel + ":");
    }

    void compileArithmeticOperation(const std::string& op, const std::vector<std::unique_ptr<Expression>>& args)
    {
        if (args.size() < 3) {
            throw std::runtime_error("Arithmetic operations require at least 2 operands");
        }

        compileExpression(args[1]);
        for (size_t i = 2; i < args.size(); ++i) {
            emit("    mov x1, x0");
            compileExpression(args[i]);
            emit("    " + operatorInstructions[op] + " x0, x1, x0");
        }
    }

    void emitPrintFunction()
    {
        emit(".section __TEXT,__cstring,cstring_literals");
        emit("L_.str:");
        emit("    .asciz \"%ld\\n\"");

        emit(".section __TEXT,__text,regular,pure_instructions");
        emit(".p2align 2");
        emit("_print_result:");
        emit("    stp x29, x30, [sp, #-16]!");
        emit("    mov x29, sp");
        emit("    adrp x1, L_.str@PAGE");
        emit("    add x1, x1, L_.str@PAGEOFF");
        emit("    bl _printf");
        emit("    ldp x29, x30, [sp], #16");
        emit("    ret");
    }

    void emit(const std::string& instruction)
    {
        instructions.push_back(instruction);
    }

    std::string assembleInstructions()
    {
        std::stringstream ss;
        for (const auto& instruction : instructions) {
            ss << instruction << "\n";
        }
        return ss.str();
    }

    std::string generateLabel()
    {
        return "L" + std::to_string(labelCounter++);
    }
};
