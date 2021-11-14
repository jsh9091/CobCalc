      *****************************************************************
      * MIT License
      * 
      * Copyright (c) 2021 Joshua Horvath
      * 
      * Permission is hereby granted, free of charge, to any person obtaining a copy
      * of this software and associated documentation files (the "Software"), to deal
      * in the Software without restriction, including without limitation the rights
      * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
      * copies of the Software, and to permit persons to whom the Software is
      * furnished to do so, subject to the following conditions:
      * 
      * The above copyright notice and this permission notice shall be included in all
      * copies or substantial portions of the Software.
      * 
      * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
      * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
      * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
      * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
      * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
      * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
      * SOFTWARE.
      *****************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CobCalc.
       AUTHOR. Joshua Horvath.
       DATE-Written. November 14, 2021
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
 
       01 FirstNum PIC 9(7)V9(2) VALUE ZEROS.
       01 SecondNum PIC 9(7)V9(2) VALUE ZEROS.
       01 Operator PIC X(1).
       01 Result PIC 9(7)V9(2) VALUE ZEROS.

       01 FirstNumDeleteZero PIC ZZZZZZ9.99.
       01 SecNumDeleteZero PIC ZZZZZZ9.99.
       01 ResultDeleteZero PIC ZZZZZZ9.99.
 
       PROCEDURE DIVISION. 

       InputNumbers.
      * Request that the user enter the first operand
       DISPLAY "Enter the first number:"
       ACCEPT FirstNum

      * Request that the user enter the second operand 
       DISPLAY "Enter the second number: "
       ACCEPT SecondNum.

      * Request the user enter operation to be performed on the numbers 
       InputOperator.
       DISPLAY "Enter the operator for the arithmetic operation "
            "to be performed (+, -, *, /): "
       ACCEPT Operator
 
      * Check the user entered a valid operator   
       IF Operator IS NOT = "+"
           AND Operator IS NOT = "-"
               AND Operator IS NOT = "*"
                   AND Operator IS NOT = "/"
            DISPLAY "An unrecognized operator was entered."
            Perform InputOperator
       END-IF

      * If division, check that denominator is not zero
       IF Operator IS = "/" AND SecondNum IS = 0
            DISPLAY "The denominator of division expression "
                "cannot be zero."
            PERFORM InputNumbers THRU InputOperator
       END-IF

      * Perform the arithmetic operation based on operator
       IF Operator IS = "+"
           ADD FirstNum TO SecondNum GIVING Result

       ELSE IF Operator IS = "-"
           SUBTRACT FirstNum FROM SecondNum GIVING Result

       ELSE IF Operator IS = "*"
           MULTIPLY FirstNum BY SecondNum GIVING Result

       ELSE IF Operator IS = "/"
           DIVIDE SecondNum INTO FirstNum GIVING Result
       ELSE
          DISPLAY "Error: Unexpected state.".
        
      * Remove leading zeros
       MOVE FirstNum TO FirstNumDeleteZero
       MOVE SecondNum TO SecNumDeleteZero
       MOVE Result TO ResultDeleteZero

      * Display the full operation with result 
       DISPLAY FirstNumDeleteZero " " Operator " " 
           SecNumDeleteZero " = " ResultDeleteZero.

       GOBACK.
