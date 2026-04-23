'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dormtr = require( './dormtr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dormtr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dormtr;
