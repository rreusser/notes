'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtfttr = require( './dtfttr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtfttr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtfttr;
