# Phone Company

Each day at The Phone Company a batch job puts all the customer calls for the previous day into a single log file of:

`'customer id','phone number called','call duration'`

For a customer the cost of a call up to and including 3 minutes in duration is charged at 0.05p/sec, any call over 3 minutes in duration the additional time is charged at 0.03p/sec. However, there is a promotion on and the calls made to the phone number with the greatest total cost is removed from the customer's bill.

## Task

Write a program that when run will parse the `calls.log` file and print out the total cost of calls for the day for each customer. You can use any libraries you wish to.

## Solution

To solve the problem I identified these streps: 
  * **Parsing**: first step of the chain, when the input file is parsed to a simple type of just strings. Object involved: Parser, RawInput.
  * **Validation**: at this point the input is validated against some rules, like valid phone number or call duration. Object involved: Call, InputValidation, PhoneNumber.
  * **Grouping**: once we have the input we need to separate the different costumers. Object involved: PhoneReport
  * **Calculation**: at this point we can calculare the total amount by costumer. Object involved: Bill, FeeCalculator
  * **Output:**: output the result to the console.
  
For testing I mainly used scala check and scala test.