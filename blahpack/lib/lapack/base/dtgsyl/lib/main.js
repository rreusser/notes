'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtgsyl = require( './dtgsyl.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtgsyl, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtgsyl;
