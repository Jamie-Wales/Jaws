#include "Number.h"
#include <cmath>
#include <gtest/gtest.h>
#include <limits>

TEST(NumberTest, ConstructorsAndTypeChecks)
{
    Number intNum(5);
    Number doubleNum(3.14);
    Number rationalNum(Number::Rational(1, 2));
    Number complexNum(Number::ComplexType(1, 1));

    EXPECT_TRUE(intNum.isInteger());
    EXPECT_TRUE(intNum.isRational());
    EXPECT_TRUE(intNum.isReal());
    EXPECT_FALSE(intNum.isComplex());

    EXPECT_FALSE(doubleNum.isInteger());
    EXPECT_FALSE(doubleNum.isRational());
    EXPECT_TRUE(doubleNum.isReal());
    EXPECT_FALSE(doubleNum.isComplex());

    EXPECT_FALSE(rationalNum.isInteger());
    EXPECT_TRUE(rationalNum.isRational());
    EXPECT_TRUE(rationalNum.isReal());
    EXPECT_FALSE(rationalNum.isComplex());

    EXPECT_FALSE(complexNum.isInteger());
    EXPECT_FALSE(complexNum.isRational());
    EXPECT_FALSE(complexNum.isReal());
    EXPECT_TRUE(complexNum.isComplex());
}

TEST(NumberTest, Addition)
{
    EXPECT_EQ((Number(3) + Number(2)).toString(), "5");
    EXPECT_EQ((Number(3) + Number(3.5)).toString(), "6.5");
    EXPECT_EQ((Number(3) + Number(Number::Rational(1, 2))).toString(), "7/2");
    EXPECT_EQ((Number(3) + Number(Number::ComplexType(2, 1))).toString(), "5+1i");
}

TEST(NumberTest, Subtraction)
{
    EXPECT_EQ((Number(5) - Number(2)).toString(), "3");
    EXPECT_EQ((Number(5) - Number(1.5)).toString(), "3.5");
    EXPECT_EQ((Number(5) - Number(Number::Rational(1, 2))).toString(), "9/2");
    EXPECT_EQ((Number(5) - Number(Number::ComplexType(1, 1))).toString(), "4-1i");
}

TEST(NumberTest, Multiplication)
{
    EXPECT_EQ((Number(3) * Number(2)).toString(), "6");
    EXPECT_EQ((Number(3) * Number(2.5)).toString(), "7.5");
    EXPECT_EQ((Number(3) * Number(Number::Rational(1, 2))).toString(), "3/2");
    EXPECT_EQ((Number(3) * Number(Number::ComplexType(1, 1))).toString(), "3+3i");
}

TEST(NumberTest, Division)
{
    EXPECT_EQ((Number(6) / Number(3)).toString(), "2");
    EXPECT_EQ((Number(5) / Number(2)).toString(), "5/2");
    EXPECT_EQ((Number(5) / Number(2.0)).toString(), "2.5");
    EXPECT_EQ((Number(6) / Number(Number::ComplexType(2, 2))).toString(), "1.5-1.5i");
    EXPECT_THROW(Number(5) / Number(0), std::runtime_error);
}

TEST(NumberTest, UnaryMinus)
{
    EXPECT_EQ((-Number(5)).toString(), "-5");
    EXPECT_EQ((-Number(1.5)).toString(), "-1.5");
    EXPECT_EQ((-Number(Number::Rational(1, 2))).toString(), "-1/2");
    EXPECT_EQ((-Number(Number::ComplexType(1, 1))).toString(), "-1-1i");
}

TEST(NumberTest, MixedTypeOperations)
{
    EXPECT_EQ((Number(5) + Number::Rational(1, 2)).toString(), "11/2");
    EXPECT_EQ((Number(3.14) * Number::Rational(2, 1)).toString(), "6.28");
    EXPECT_EQ((Number(5) + Number::ComplexType(2, 1)).toString(), "7+1i");
    EXPECT_EQ((Number(Number::ComplexType(1, 1)) / Number(2)).toString(), "0.5+0.5i");
}
