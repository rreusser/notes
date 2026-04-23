'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dposvx = require( './dposvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dposvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = dposvx;
