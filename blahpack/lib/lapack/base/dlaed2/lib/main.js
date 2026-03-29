'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaed2 = require( './dlaed2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaed2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaed2;
