'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtpttr = require( './dtpttr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtpttr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtpttr;
