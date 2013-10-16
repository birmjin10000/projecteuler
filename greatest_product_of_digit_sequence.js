// Project Euler Problem No. 8
/* jshint globalstrict: true */
"use strict";

var fs = require('fs');
var filename = process.argv[2];

fs.readFile(filename, function(err, data) {
  if (err) throw err;
  var array = [];
  var digits = data.toString().replace(/\r?\n|\r/g,"");
  var length = digits.length;
  console.log("Length of digit sequence is: " + length);
 
  for (var i = 0; i < length; i++) {
    array.push(digits.charAt(i));
  }

  var max_product_of_digit_sequence = 1;
  var product_of_digit_sequence = 1;
  var COUNT_OF_DIGITS = 5;
 
  for (var index = 0; index < length - COUNT_OF_DIGITS; index++) {
    product_of_digit_sequence = 1;
    for (var offset = 0; offset < COUNT_OF_DIGITS; offset++) {
      product_of_digit_sequence *= array[index + offset]; 
    }
    
    if (product_of_digit_sequence > max_product_of_digit_sequence) {
      max_product_of_digit_sequence = product_of_digit_sequence;
    }
  }

  console.log("Greatest product of " + COUNT_OF_DIGITS + " sequential digits is " + max_product_of_digit_sequence); 
});
