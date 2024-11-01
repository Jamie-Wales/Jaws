#include "Error.h"
#include "Scanner.h"
#include "gtest/gtest.h"

TEST(ScannerTest, TokenizesBasicIdentifiers)
{
    Scanner scanner;
    std::string input = "(define x 10)";
    auto tokens = scanner.tokenize(input);
    EXPECT_EQ(tokens[0].type, Tokentype::LEFT_PAREN);
    EXPECT_EQ(tokens[0].lexeme, "(");
    EXPECT_EQ(tokens[1].type, Tokentype::DEFINE);
    EXPECT_EQ(tokens[1].lexeme, "define");
    EXPECT_EQ(tokens[2].type, Tokentype::IDENTIFIER);
    EXPECT_EQ(tokens[2].lexeme, "x");
    EXPECT_EQ(tokens[3].type, Tokentype::INTEGER);
    EXPECT_EQ(tokens[3].lexeme, "10");
    EXPECT_EQ(tokens[4].type, Tokentype::RIGHT_PAREN);
    EXPECT_EQ(tokens[4].lexeme, ")");
}

TEST(ScannerTest, HandlesComments)
{
    Scanner scanner;
    std::string input = "; This is a comment\n(define y 20)";
    auto tokens = scanner.tokenize(input);
    EXPECT_EQ(tokens[0].type, Tokentype::LEFT_PAREN);
    EXPECT_EQ(tokens[0].lexeme, "(");
    EXPECT_EQ(tokens[1].type, Tokentype::DEFINE);
    EXPECT_EQ(tokens[1].lexeme, "define");
    EXPECT_EQ(tokens[2].type, Tokentype::IDENTIFIER);
    EXPECT_EQ(tokens[2].lexeme, "y");
    EXPECT_EQ(tokens[3].type, Tokentype::INTEGER);
    EXPECT_EQ(tokens[3].lexeme, "20");
    EXPECT_EQ(tokens[4].type, Tokentype::RIGHT_PAREN);
    EXPECT_EQ(tokens[4].lexeme, ")");
}

TEST(ScannerTest, TokenizesNumbers)
{
    Scanner scanner;
    std::vector<std::string> inputs = { "42", "-3.14", "0.001" };
    std::vector<Tokentype> expectedTypes = {
        Tokentype::INTEGER, Tokentype::FLOAT, Tokentype::FLOAT
    };
    std::vector<std::string> expectedLexemes = { "42", "-3.14", "0.001" };
    for (size_t i = 0; i < inputs.size(); ++i) {
        auto tokens = scanner.tokenize(inputs[i]);
        EXPECT_EQ(tokens[0].type, expectedTypes[i]);
        EXPECT_EQ(tokens[0].lexeme, expectedLexemes[i]);
    }
}

TEST(ScannerTest, TokenizesSymbolsAndOperators)
{
    Scanner scanner;
    std::string input = "(+ 1 2)";
    auto tokens = scanner.tokenize(input);
    EXPECT_EQ(tokens[0].type, Tokentype::LEFT_PAREN);
    EXPECT_EQ(tokens[0].lexeme, "(");
    EXPECT_EQ(tokens[1].type, Tokentype::IDENTIFIER);
    EXPECT_EQ(tokens[1].lexeme, "+");
    EXPECT_EQ(tokens[2].type, Tokentype::INTEGER);
    EXPECT_EQ(tokens[2].lexeme, "1");
    EXPECT_EQ(tokens[3].type, Tokentype::INTEGER);
    EXPECT_EQ(tokens[3].lexeme, "2");
    EXPECT_EQ(tokens[4].type, Tokentype::RIGHT_PAREN);
    EXPECT_EQ(tokens[4].lexeme, ")");
}
TEST(ScannerTest, TokenizesStrings)
{
    Scanner scanner;
    std::string input = R"("Hello, World!")";
    auto tokens = scanner.tokenize(input);
    EXPECT_EQ(tokens[0].type, Tokentype::STRING);
    EXPECT_EQ(tokens[0].lexeme, "\"Hello, World!\"");
}

TEST(ScannerTest, TokenizesBooleans)
{
    Scanner scanner;
    std::string input = "#t #f";
    auto tokens = scanner.tokenize(input);
    EXPECT_EQ(tokens[0].type, Tokentype::TRUE);
    EXPECT_EQ(tokens[0].lexeme, "#t");
    EXPECT_EQ(tokens[1].type, Tokentype::FALSE);
    EXPECT_EQ(tokens[1].lexeme, "#f");
}

TEST(ScannerTest, HandlesUnknownCharacters)
{
    Scanner scanner;
    std::string input = "\\";
    try {
        auto tokens = scanner.tokenize(input);
        FAIL() << "Expected ParseError";
    } catch (const ParseError& e) {
        e.printFormattedError();
    } catch (...) {
        FAIL() << "Expected ParseError";
    }
}
