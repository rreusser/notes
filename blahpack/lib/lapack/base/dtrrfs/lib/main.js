'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtrrfs = require( './dtrrfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtrrfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtrrfs;
