'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgeqp3 = require( './dgeqp3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgeqp3, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgeqp3;
