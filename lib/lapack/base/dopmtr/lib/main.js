
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dopmtr = require( './dopmtr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dopmtr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dopmtr;
